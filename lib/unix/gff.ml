open Core_kernel
open CFStream

(*
  Version 2:
  http://www.sanger.ac.uk/resources/software/gff/spec.html
  http://gmod.org/wiki/GFF2

  Version 3:
  http://www.sequenceontology.org/gff3.shtml
  http://gmod.org/wiki/GFF3
*)

type record = {
  seqname: string;
  source: string option;
  feature: string option;
  pos: int * int;

  score: float option;
  strand: [`plus | `minus | `not_applicable | `unknown ];
  phase: int option;
  attributes: (string * string list) list;
}
[@@deriving sexp]

type item = [ `comment of string | `record of record ]
[@@deriving sexp]

(* `module_error` should progressively allow to “tag” error values. *)
let module_error e = Error (`gff e)

module Tags = struct

  type t = {
    version: [`two | `three];
    allow_empty_lines: bool;
    sharp_comments: bool;
  }
  [@@deriving sexp]

  let default =
    {version = `three; allow_empty_lines = false; sharp_comments = true}


  let to_string t = sexp_of_t t |> Sexplib.Sexp.to_string
  let of_string s =
    try Ok (Sexplib.Sexp.of_string s |> t_of_sexp)
    with e -> module_error (`tags_of_string e)

end

module Error = struct
  type parsing =
    [ `cannot_parse_float of Pos.t * string
    | `cannot_parse_int of Pos.t * string
    | `cannot_parse_strand of Pos.t * string
    | `cannot_parse_string of Pos.t * string
    | `empty_line of Pos.t
    | `incomplete_input of
        Pos.t * string list * string option
    | `wrong_attributes of Pos.t * string
    | `wrong_row of Pos.t * string
    | `wrong_url_escaping of Pos.t * string ]
  [@@deriving sexp]

  type t = parsing [@@deriving sexp]
end

module Transform = struct
  open Result.Monad_infix

  let parse_string msg pos i =
    begin try Ok (Scanf.sscanf i "%S " ident) with
    | _ ->
      begin match (Scanf.sscanf i "%s " ident) with
      | "" -> Error (`cannot_parse_string (pos, msg))
      | s -> Ok (Uri.pct_decode s)
      end
    end

  let parse_string_opt m pos i =
    parse_string m pos i >>= fun s ->
    begin match s with
    | "." -> Ok None
    | s -> Ok (Some s)
    end

  let parse_int msg pos i =
    parse_string msg pos i >>= fun s ->
    (try Ok (Int.of_string s)
     with _ -> Error (`cannot_parse_int (pos, msg)))

  let parse_float_opt msg pos i =
    parse_string_opt msg pos i >>= function
    | Some s ->
      (try Ok (Some (Float.of_string s))
       with _ -> Error (`cannot_parse_float (pos, msg)))
    | None -> Ok None

  let parse_int_opt msg pos i =
    parse_string_opt msg pos i >>= function
    | Some s ->
      (try Ok (Some (Int.of_string s))
       with _ -> Error (`cannot_parse_int (pos, msg)))
    | None -> Ok None

  let parse_attributes_version_3 position i =
    let whole_thing = String.concat ~sep:"\t" i in
  (*   let b = Buffer.create 42 in *)
  (*   String.iter (String.concat ~sep:"\t" i) (function *)
  (*   | ' ' -> Buffer.add_string b "%20" *)
  (*   | c -> Buffer.add_char b c); *)
  (*   Buffer.contents b *)
  (* in *)
    let get_csv s =
      List.map (String.split ~on:',' s)
        ~f:(fun s -> parse_string "value" position String.(strip s))
      |> List.partition_map ~f:Result.ok_fst
      |> (function
        | (ok, []) -> Ok ok
        | (_, notok :: _) -> Error notok) in
    let rec loop pos acc =
      begin match String.lfindi whole_thing ~pos ~f:(fun _ c -> c = '=') with
      | Some equal ->
        parse_string "tag" position (String.slice whole_thing pos equal)
        >>= fun tag ->
        let pos = equal + 1 in
        begin match String.lfindi whole_thing ~pos ~f:(fun _ c -> c = ';') with
        | Some semicolon ->
          let delimited = String.slice whole_thing pos semicolon in
          get_csv delimited
          >>= fun values ->
          loop (semicolon + 1) ((tag, values) :: acc)
        | None ->
          let delimited = String.(sub whole_thing ~pos ~len:(length whole_thing - pos)) in
          get_csv delimited
          >>= fun values ->
          Ok ((tag, values) :: acc)
        end
      | None ->
        if pos >= String.length whole_thing then
          Ok acc
        else
          Error (`wrong_attributes (position, whole_thing))
      end
    in
    (try loop 0 [] with _ -> Error (`wrong_attributes (position, whole_thing)))
    >>| List.rev

  let parse_attributes_version_2 position l =
    let whole_thing = String.(concat ~sep:"\t" l |> strip) in
    let parse_string i =
      begin try Some (Scanf.bscanf i "%S " ident) with
      | _ ->
        begin match (Scanf.bscanf i "%s " ident) with
        | "" -> None
        | s -> Some s
        end
      end
    in
    let inch = Scanf.Scanning.from_string whole_thing in
    let tokens =
      Stream.(from (fun _ -> parse_string inch) |> Fn.flip npeek Int.max_value) in
    let rec go_3_by_3 acc = function
    | k  :: v :: [] -> Ok (List.rev ((k, [v]) :: acc))
    | k  :: v :: ";" :: rest -> go_3_by_3 ((k, [v]) :: acc) rest
    | [] | [";"] -> Ok (List.rev acc)
    | _ -> Error (`wrong_attributes (position, whole_thing))
    in
    go_3_by_3 [] tokens


  let parse_row ~version pos s =
    let fields = String.split ~on:'\t' s in
    begin match fields with
    | seqname :: source :: feature :: start :: stop :: score :: strand :: phase
      :: rest ->
      let result =
        parse_string "Sequence name" pos seqname >>= fun seqname ->
        parse_string_opt "Source" pos source >>= fun source ->
        parse_string_opt "Feature" pos feature >>= fun feature ->
        parse_int "Start Position" pos start >>= fun start ->
        parse_int "Stop Position" pos stop >>= fun stop ->
        parse_float_opt "Score" pos score >>= fun score ->
        parse_string_opt "Strand" pos strand
        >>= (function
        | Some "+" -> Ok `plus
        | None -> Ok `not_applicable
        | Some "-" -> Ok `minus
        | Some "?" -> Ok `unknown
        | Some s -> Error (`cannot_parse_strand (pos, s)))
        >>= fun strand ->
        parse_int_opt "Phase/Frame" pos phase >>= fun phase ->
        begin match version with
        | `two -> parse_attributes_version_2 pos rest
        | `three -> parse_attributes_version_3 pos rest
        end
        >>= fun attributes ->
        Ok (`record {seqname; source; feature; pos = (start, stop); score;
                         strand; phase; attributes})
      in
      `output result

    | _ ->
      `output (Error (`wrong_row (pos, s)))
    end

  let rec next ~tags  p =
    let open Lines.Buffer in
    match (next_line p :> string option) with
    | None -> `not_ready
    | Some "" ->
      if tags.Tags.allow_empty_lines
      then `output (Error (`empty_line (current_position p)))
      else next ~tags p
    | Some l when
        tags.Tags.sharp_comments && String.(is_prefix (strip l) ~prefix:"#") ->
      `output (Ok (`comment String.(sub l ~pos:1 ~len:(length l - 1))))
    | Some l -> parse_row ~version:tags.Tags.version (current_position p) l

  let string_to_item ?filename ~tags () =
    let name = sprintf "gff_parser:%s" Option.(value ~default:"<>" filename) in
    let next = next ~tags in
    Lines.Transform.make_merge_error ~name ?filename ~next ()

  let item_to_string_pure version = (function
  | `comment c -> sprintf "#%s\n" c
  | `record t ->
    let escape =
      match version with | `three -> (fun s -> Uri.pct_encode s) | `two -> sprintf "%S" in
    let optescape  o =  Option.value_map ~default:"." o ~f:escape in
    String.concat ~sep:"\t" [
      escape t.seqname;
      optescape t.source;
      optescape t.feature;
      sprintf "%d" (fst t.pos);
      sprintf "%d" (snd t.pos);
      Option.value_map ~default:"." ~f:(sprintf "%g") t.score;
      (match t.strand with`plus -> "+" | `minus -> "-"
                        | `not_applicable -> "." | `unknown -> "?");
      Option.value_map ~default:"." ~f:(sprintf "%d") t.phase;
      String.concat ~sep:";"
        (List.map t.attributes ~f:(fun (k,v) ->
           match version with
           | `three ->
             sprintf "%s=%s" (Uri.pct_encode k)
               (List.map v ~f:Uri.pct_encode |> String.concat ~sep:",")
           | `two ->
             sprintf "%S %s" k
               (List.map v ~f:escape |> String.concat ~sep:",")
         ));
    ] ^ "\n"
  )

  let item_to_string ~tags () =
    Tfxm.of_function ~name:"gff_to_string"
      (item_to_string_pure tags.Tags.version)

end

exception Error of  Error.t
let error_to_exn e = Error e

let in_channel_to_item_stream
    ?(buffer_size=65536) ?filename ?(tags=Tags.default) inp =
  let x = Transform.string_to_item ~tags ?filename () in
  Tfxm.(in_channel_strings_to_stream inp x ~buffer_size)

let in_channel_to_item_stream_exn ?buffer_size ?tags inp =
  Stream.result_to_exn ~error_to_exn
    (in_channel_to_item_stream ?buffer_size ?tags inp)

let item_to_string ?(tags=Tags.default) item =
  Transform.item_to_string_pure tags.Tags.version item
