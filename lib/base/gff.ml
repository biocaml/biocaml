(* https://github.com/The-Sequence-Ontology/Specifications/blob/master/gff3.md *)

type record =
  { seqname : string
  ; source : string option
  ; feature : string option
  ; start_pos : int
  ; stop_pos : int
  ; score : float option
  ; strand : [ `Plus | `Minus | `Not_stranded | `Unknown ]
  ; phase : int option
  ; attributes : (string * string list) list
  }

type item =
  [ `Comment of string
  | `Record of record
  ]

let record
      ?source
      ?feature
      ?score
      ?(strand = `Unknown)
      ?phase
      ?(attributes = [])
      seqname
      start_pos
      stop_pos
  =
  { seqname; source; feature; start_pos; stop_pos; score; strand; phase; attributes }
;;

let fail s = Error (`Msg s)
let failf fmt = Printf.ksprintf fail fmt

let parse_float s =
  try Ok (Float.of_string s) with
  | Failure s -> fail s
;;

let parse_int s =
  try Ok (Int.of_string s) with
  | Failure s -> fail s
;;

let parse_opt f = function
  | "." -> None
  | s -> Some (f s)
;;

let parse_opt' f = function
  | "." -> Ok None
  | s -> Result.(f s >>| Option.some)
;;

let parse_strand = function
  | "." -> Ok `Not_stranded
  | "?" -> Ok `Unknown
  | "+" -> Ok `Plus
  | "-" -> Ok `Minus
  | _ -> Error (`Msg "Incorrect strand character")
;;

let parse_tag pos buf =
  match String.index_from buf pos '=' with
  | None -> fail "Tag without a value"
  | Some k -> Ok (k + 1, String.sub buf ~pos ~len:(k - pos))
;;

let%test "Gff.parse_tag" =
  match parse_tag 0 "gene_id=foo", Ok (8, "gene_id") with
  | Ok (k, v), Ok (k', v') -> Int.equal k k' && String.equal v v'
  | Error _, Ok _ -> false
  | Ok _, Error _ -> false
  | Error _, Error _ -> false
;;

let lfind_mapi ?(pos = 0) s ~f =
  let n = String.length s in
  let rec loop i =
    if i < n
    then (
      match f i s.[i] with
      | None -> loop (i + 1)
      | Some y -> Some y)
    else None
  in
  loop pos
;;

let rec parse_value_list pos buf acc : int * string list =
  let comma_or_semi_colon i = function
    | ',' -> Some (i, `Comma)
    | ';' -> Some (i, `Semi_colon)
    | _ -> None
  in
  match lfind_mapi ~pos buf ~f:comma_or_semi_colon with
  | None ->
    let n = String.length buf in
    let value = String.sub buf ~pos ~len:(n - pos) in
    n, List.rev (value :: acc)
  | Some (k, `Comma) ->
    let value = String.sub buf ~pos ~len:(k - pos) in
    parse_value_list (k + 1) buf (value :: acc)
  | Some (k, `Semi_colon) ->
    let value = String.sub buf ~pos ~len:(k - pos) in
    k + 1, List.rev (value :: acc)
;;

let test_parse_value buf y =
  let n, x = parse_value_list 0 buf [] in
  let n' = String.length buf in
  Int.equal n n' && List.equal String.equal x y
;;

let%test "parse_value_list1" =
  test_parse_value "region_id=chr1:3008683-3009183" [ "region_id=chr1:3008683-3009183" ]
;;

let%test "parse_value_list2" = test_parse_value "3,4" [ "3"; "4" ]

let rec parse_gff3_attributes pos buf acc =
  let open Result in
  if pos >= String.length buf
  then Ok (List.rev acc)
  else
    parse_tag pos buf
    >>= fun (pos, tag) ->
    let pos, values = parse_value_list pos buf [] in
    let acc = (tag, values) :: acc in
    parse_gff3_attributes pos buf acc
;;

let parse_gff3_attributes buf
  : ((string * string list) list, [> `Msg of string ]) Result.t
  =
  parse_gff3_attributes 0 buf []
;;

let%test "Gff.parse_gff3_attributes" =
  match parse_gff3_attributes "a=2,3;b=4", Ok [ "a", [ "2"; "3" ]; "b", [ "4" ] ] with
  | Ok x, Ok y ->
    List.equal
      (fun (x1, l1) (x2, l2) -> String.equal x1 x2 && List.equal String.equal l1 l2)
      x
      y
  | Error _, Ok _ -> false
  | Ok _, Error _ -> false
  | Error _, Error _ -> false
;;

let parse_gtf_attributes buf : ((string * string list) list, [> `Msg of string ]) Result.t
  =
  let open Result.Monad_infix in
  let rec tokenize acc p =
    if p >= String.length buf
    then Ok (List.rev acc)
    else (
      match buf.[p] with
      | '\t' -> fail "Unexpected tag character"
      | '\n' -> fail "Unexpected EOL character"
      | ' ' -> tokenize acc (p + 1)
      | ';' -> tokenize (`SEMICOLON :: acc) (p + 1)
      | '"' ->
        next_quote (p + 1)
        >>= fun q ->
        let len = q - p - 1 in
        tokenize (`QUOTED (p + 1, len) :: acc) (q + 1)
      | _ -> token_end p >>= fun q -> tokenize (`TOKEN (p, q - p + 1) :: acc) (q + 1))
  and next_quote p =
    if p >= String.length buf
    then fail "Reached end of string but expected dquote"
    else (
      match buf.[p] with
      | '"' -> Ok p
      | _ -> next_quote (p + 1))
  and token_end p =
    if p >= String.length buf
    then Ok (p - 1)
    else (
      match buf.[p] with
      | ' ' -> Ok (p - 1)
      | _ -> token_end (p + 1))
  in
  let rec attribute acc = function
    | `TOKEN (p, q) :: (`QUOTED (r, s) | `TOKEN (r, s)) :: rest ->
      let att = String.sub buf ~pos:p ~len:q, [ String.sub buf ~pos:r ~len:s ] in
      attribute_tail (att :: acc) rest
    | _ -> failf "Cannot parse attributes: %s" buf
  and attribute_tail acc = function
    | [] | [ `SEMICOLON ] -> Ok (List.rev acc)
    | `SEMICOLON :: rest -> attribute acc rest
    | _ -> failf "Cannot parse attributes: %s" buf
  in
  tokenize [] 0 >>= fun tokens -> attribute [] tokens
;;

let%test "Gff.parse_gtf_attributes1" =
  match
    parse_gtf_attributes {|gene_id "FBgn0031081"|}, Ok [ "gene_id", [ "FBgn0031081" ] ]
  with
  | Ok x, Ok y ->
    List.equal
      (fun (x1, l1) (x2, l2) -> String.equal x1 x2 && List.equal String.equal l1 l2)
      x
      y
  | Error _, Ok _ -> false
  | Ok _, Error _ -> false
  | Error _, Error _ -> false
;;

let%test "Gff.parse_gtf_attributes" =
  match
    ( parse_gtf_attributes
        {|gene_id "FBgn0031081"; gene_symbol "Nep3"; transcript_id "FBtr0070000"; transcript_symbol "Nep3-RA";|}
    , Ok
        [ "gene_id", [ "FBgn0031081" ]
        ; "gene_symbol", [ "Nep3" ]
        ; "transcript_id", [ "FBtr0070000" ]
        ; "transcript_symbol", [ "Nep3-RA" ]
        ] )
  with
  | Ok x, Ok y ->
    List.equal
      (fun (x1, l1) (x2, l2) -> String.equal x1 x2 && List.equal String.equal l1 l2)
      x
      y
  | Error _, Ok _ -> false
  | Ok _, Error _ -> false
  | Error _, Error _ -> false
;;

let parse_fields parse_attributes = function
  | [ seqname; source; feature; start_pos; stop_pos; score; strand; phase; attributes ] ->
    let open Result in
    parse_int start_pos
    >>= fun start_pos ->
    parse_int stop_pos
    >>= fun stop_pos ->
    parse_opt' parse_int phase
    >>= fun phase ->
    parse_opt' parse_float score
    >>= fun score ->
    parse_strand strand
    >>= fun strand ->
    parse_attributes attributes
    >>= fun attributes ->
    Ok
      { seqname
      ; source = parse_opt Fn.id source
      ; feature = parse_opt Fn.id feature
      ; start_pos
      ; stop_pos
      ; score
      ; strand
      ; phase
      ; attributes
      }
  | _ -> fail "Incorrect number of fields"
;;

let item_of_line parse_attributes line =
  match (line : Line.t :> string) with
  | "" -> fail "Empty line"
  | line ->
    if Char.(line.[0] = '#')
    then Ok (`Comment (String.sub line ~pos:1 ~len:(String.length line - 1)))
    else
      let open Result in
      let fields = String.split ~on:'\t' line in
      parse_fields parse_attributes fields >>| fun r -> `Record r
;;

let gff3_item_of_line line = item_of_line parse_gff3_attributes line
let gtf_item_of_line line = item_of_line parse_gtf_attributes line

let line_of_item version = function
  | `Comment c -> Line.of_string_unsafe ("#" ^ c)
  | `Record t ->
    let escape =
      match version with
      | `three -> fun s -> Uri.pct_encode s
      | `two -> sprintf "%S"
    in
    let optescape o = Option.value_map ~default:"." o ~f:escape in
    String.concat
      ~sep:"\t"
      [ t.seqname
      ; optescape t.source
      ; Option.value ~default:"." t.feature
      ; Int.to_string t.start_pos
      ; Int.to_string t.stop_pos
      ; Option.value_map ~default:"." ~f:(sprintf "%g") t.score
      ; (match t.strand with
         | `Plus -> "+"
         | `Minus -> "-"
         | `Not_stranded -> "."
         | `Unknown -> "?")
      ; Option.value_map ~default:"." ~f:(sprintf "%d") t.phase
      ; String.concat
          ~sep:";"
          (List.map t.attributes ~f:(fun (k, v) ->
             match version with
             | `three ->
               sprintf
                 "%s=%s"
                 (Uri.pct_encode k)
                 (List.map v ~f:Uri.pct_encode |> String.concat ~sep:",")
             | `two -> sprintf "%s %s" k (List.map v ~f:escape |> String.concat ~sep:",")))
      ]
    |> Line.of_string_unsafe
;;
