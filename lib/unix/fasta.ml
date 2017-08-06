open Core_kernel
module Result = Biocaml_result
open CFStream

type header = string list

type item = {
  description : string;
  sequence : string;
}

type fmt = {
  allow_sharp_comments : bool;
  allow_semicolon_comments : bool;
  allow_empty_lines : bool;
  comments_only_at_top : bool;
  max_line_length : int option;
  alphabet : string option;
}

let default_fmt = {
  allow_sharp_comments = true;
  allow_semicolon_comments = false;
  allow_empty_lines = false;
  comments_only_at_top = true;
  max_line_length = None;
  alphabet = None;
}

type item0 = [
| `Comment of string
| `Empty_line
| `Description of string
| `Partial_sequence of string
]


let sequence_to_int_list s =
  String.split s ~on:' '
  |> Result.List.map ~f:(fun x ->
    try Ok (Int.of_string x)
    with Failure _ -> error "invalid int" x sexp_of_string
  )


(******************************************************************************)
(* Low-level Parsing                                                          *)
(******************************************************************************)
let parse_item0
    ?(allow_sharp_comments=true)
    ?(allow_semicolon_comments=false)
    ?(allow_empty_lines=false)
    ?max_line_length
    ?alphabet
    line
    =
  let open Result.Monad_infix in
  let s = (line : Line.t :> string) in
  let n = String.length s in

  (match max_line_length with
  | None -> Ok ()
  | Some x ->
    if x <= n then Ok ()
    else error
      "max_line_length exceeded"
      (x,n) [%sexp_of: int * int ]
  ) >>= fun () ->

  if allow_empty_lines && (String.for_all s ~f:Char.is_whitespace) then
    Ok `Empty_line

  else if (not allow_empty_lines && n = 0) then
    Or_error.error_string "allow_empty_lines is false but got empty line"

  (* n > 0 if we got here *)

  else if s.[0] = '>' then
    Ok (`Description (String.slice s 1 n))

  else
    match allow_sharp_comments, allow_semicolon_comments, s.[0] with
    | true,true,(';' | '#')
    | true,false,'#'
    | false,true,';' ->
      Ok (`Comment (String.slice s 1 n))
    | false,false,(';' | '#') ->
      Or_error.error_string "comments lines are not allowed"
    | _ ->
      (match alphabet with
      | None -> Ok (`Partial_sequence s)
      | Some alphabet ->
        if String.for_all s ~f:(String.mem alphabet) then
          Ok (`Partial_sequence s)
        else
          (* TODO: report which character is outside alphabet *)
          error "sequence contains string outside allowed alphabet"
            (s,alphabet) [%sexp_of: string * string ]
      )


(******************************************************************************)
(* Input/Output                                                               *)
(******************************************************************************)
module Lines = Lines.MakeIO(Future_unix)

let read0
    ?(start=Pos.(incr_line unknown))
    ?(allow_sharp_comments=true)
    ?(allow_semicolon_comments=false)
    ?(allow_empty_lines=false)
    ?max_line_length
    ?alphabet
    r
    =
  let pos = ref start in
  Stream.map (Lines.read r) ~f:(fun line ->
    let current_pos = !pos in
    pos := Pos.incr_line !pos;
    parse_item0
      line
      ~allow_sharp_comments
      ~allow_semicolon_comments
      ~allow_empty_lines
      ?max_line_length
      ?alphabet
    |> fun x ->
      Or_error.tag_arg x "position" current_pos Pos.sexp_of_t
  )

(** Return the initial comment lines. Upon return, [item0s] will point
    to first item0 that is not a `Comment, but there may still be
    additional `Comment items later. *)
let read_header
    ?(allow_empty_lines=false)
    (item0s : item0 Or_error.t Stream.t)
    : header Or_error.t
    =
  let rec loop accum : header Or_error.t =
    match Stream.peek item0s with
    | Some (Ok (`Comment x)) -> (
      Stream.junk item0s;
      loop (x::accum)
    )
    | Some (Ok `Empty_line) -> (
      if allow_empty_lines then
        loop accum
      else
        Or_error.error_string
          "allow_empty_lines is false but got empty line in header"
    )
    | Some (Ok (`Description _))
    | Some (Ok (`Partial_sequence _)) ->
      Ok accum
    | Some (Error _ as e) ->
      e
    | None ->
      Ok accum
  in
  loop [] |> Result.map ~f:List.rev

let read ?start ?(fmt=default_fmt) r =
  let {allow_sharp_comments;
       allow_semicolon_comments;
       allow_empty_lines;
       comments_only_at_top;
       max_line_length;
       alphabet} = fmt
  in
  let error_string s = Some (Or_error.error_string s) in
  let item0s = read0 r
    ?start
    ~allow_sharp_comments
    ~allow_semicolon_comments
    ~allow_empty_lines
    ?max_line_length
    ?alphabet
  in
  match read_header ~allow_empty_lines item0s with
  | Error _ as e -> e
  | Ok header ->
    let rec f description partial_seqs : item Or_error.t option =
      match Stream.peek item0s with
      | Some (Ok (`Comment _)) -> begin
        if comments_only_at_top then
          error_string "comments_only_at_top = true but got comment later"
        else (
          Stream.junk item0s;
          f description partial_seqs
        )
      end

      | Some (Ok `Empty_line) -> begin
        if allow_empty_lines then (
          Stream.junk item0s;
          f description partial_seqs
        )
        else
          error_string "allow_empty_lines = false but got empty line"
      end

      | Some (Ok (`Description x)) -> begin
        match description,partial_seqs with
        | None, [] -> (
          Stream.junk item0s;
          f (Some x) []
        )
        | None, _::_ ->
          (* `Partial_sequence branch assures this doesn't happen*)
          assert false
        | Some _, [] ->
          error_string "previous description line not followed by sequence"
        | Some description, partial_seqs ->
          Some (Ok {
            description;
            sequence = partial_seqs |> List.rev |> String.concat ~sep:"";
          })
      end

      | Some (Ok (`Partial_sequence x)) -> begin
        match description,partial_seqs with
        | None, _ ->
          error_string "sequence not preceded by description line"
        | Some _, partial_seqs -> (
          Stream.junk item0s;
          f description (x::partial_seqs)
        )
      end

      | Some (Error _ as e) -> begin
        Stream.junk item0s;
        Some e
      end

      | None -> begin
        match description,partial_seqs with
        | None, [] -> None
        | None, _::_ ->
          (* `Partial_sequence branch assures this doesn't happen*)
          assert false
        | Some _, [] ->
          error_string
            "description line not followed by sequence, reached end-of-file"
        | Some description, partial_seqs ->
          Some (Ok {
            description;
            sequence = partial_seqs |> List.rev |> String.concat ~sep:"";
          })
      end
    in
    Ok (header, Stream.from (fun _ -> f None []))


let with_file ?fmt file ~f =
  let start = Pos.make ~source:file ~line:1 () in
  In_channel.with_file file ~f:(fun cin ->
    match read ~start ?fmt cin with
    | Error _ as e -> e
    | Ok (header,strm) -> f header strm
  )
