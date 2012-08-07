open Biocaml_internal_pervasives


type alignment = {
  qname : string;
  flag : int;
  rname : string;
  pos : int;
  mapq : int;
  cigar : string;
  rnext : string;
  pnext : int;
  tlen : int;
  seq : string;
  qual : string;
  optional : (string * char * string) list
}

type t = [
| `comment of string
| `header_line of string * (string * string) list
| `alignment of alignment
]
type parse_error = [
| `incomplete_input of Biocaml_pos.t * string list * string option
| `invalid_header_tag of Biocaml_pos.t * string
| `invalid_tag_value_list of Biocaml_pos.t * string list
| `not_an_int of Biocaml_pos.t * string * string
| `wrong_alignment of Biocaml_pos.t * string
| `wrong_optional_field of Biocaml_pos.t * string
]
open Result


let is_tag_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
  | _ -> false
let is_valid_tag s =
  if String.length s = 2 then (is_tag_char s.[0] && is_tag_char s.[1])
  else false

let parse_header_line position line =
  match String.(split ~on:'\t' (chop_prefix_exn line ~prefix:"@")) with
  | [] -> assert false
  | "CO" :: rest -> return (`comment (String.concat ~sep:"\t" rest))
  | tag :: values ->
    if is_valid_tag tag then (
      let tag_values () =
        List.map values (fun v ->
          match String.split ~on:':' v with
          | tag :: value :: [] ->
            if is_valid_tag tag then (tag, value)
            else failwith "A"
          | other ->  failwith "A") in
      begin try return (tag_values ()) with
      | Failure _ -> fail (`invalid_tag_value_list (position, values))
      end
      >>= fun tv ->
      return (`header_line (tag, tv))
    ) else
      fail (`invalid_header_tag (position, tag))

let parse_optional_field position s =
  match String.split s ~on:':' with
  | [tag; typ; value] ->
    if is_valid_tag tag then
      begin match typ with
      | "A" | "i" | "f" | "Z" | "H" | "B" ->
        return (tag, typ.[0], value)
      | _ ->
        fail (`wrong_optional_field (position, s))
      end
    else
      fail (`wrong_optional_field (position, s))
  | _ ->
    fail (`wrong_optional_field (position, s))

let parse_alignment position s  =
  let int field x =
    try return (int_of_string x)
    with Failure _ -> fail (`not_an_int (position, field, x)) in
  match String.split s ~on:'\t' with
  | qname  ::  flag :: rname :: pos :: mapq :: cigar :: rnext :: pnext
    :: tlen :: seq :: qual :: optional ->
    begin
      int "flag" flag >>= fun flag ->
      int "pos" pos >>= fun pos ->
      int "mapq" mapq >>= fun mapq ->
      int "pnext" pnext >>= fun pnext ->
      int "tlen" tlen >>= fun tlen ->
      Result_list.while_ok optional (parse_optional_field position)
      >>= fun optional ->
      return (`alignment {
        qname;  flag; rname; pos; mapq; cigar; rnext;
        pnext; tlen; seq; qual; optional })
    end
  | _ ->
    fail (`wrong_alignment (position, s))
  
let rec next p =
  let open Biocaml_transform.Line_oriented in
  let output_result = function
    | Ok o -> `output o
    | Error e -> `error e in
  match next_line p with
  | None -> `not_ready
  | Some "" -> `not_ready
  | Some l when String.(is_prefix (strip l) ~prefix:"@") ->
    parse_header_line (current_position p) l |! output_result
  | Some l ->
    parse_alignment (current_position p) l |! output_result
  
let parser ?filename () =
  let name = sprintf "sam_parser:%s" Option.(value ~default:"<>" filename) in
  Biocaml_transform.Line_oriented.stoppable_parser ~name ?filename ~next ()

(*

open Batteries;; open Printf

exception Bad of string

type record_type_code = string

type header_line =
    | Comment of string
    | NonComment of record_type_code * (string * string) list

type header = header_line list

type alignment = {
  qname : string;
  flag : int;
  rname : string;
  pos : int;
  mapq : int;
  cigar : string;
  rnext : string;
  pnext : int;
  tlen : int;
  seq : string;
  qual : string;
  optional : (string * char * string) list
}

let record_type_code_of_header_line = function
  | Comment _ -> "@CO"
  | NonComment (x,_) -> x

let record_type_code_regexp = Pcre.regexp "^@[A-Za-z][A-Za-z]$"
let header_tag_regexp = Pcre.regexp "^[A-Za-z][A-Za-z]$"
let optional_tag_regexp = Pcre.regexp "^[A-Za-z][A-Za-z0-9]$"

let header_line_to_string = function
  | NonComment (rtc, xys) ->
      sprintf "%s\t%s" rtc (String.concat "\t" (List.map (Tuple2.uncurry (sprintf "%s:%s")) xys))
  | Comment x -> sprintf "@CO\t%s" x

let header_to_string = List.map header_line_to_string |- String.concat "\n"

let alignment_to_string x =
  sprintf "%s\t%d\t%s\t%d\t%d\t%s\t%s\t%d\t%d\t%s\t%s\t%s"
    x.qname x.flag x.rname x.pos x.mapq x.cigar x.rnext x.pnext x.tlen x.seq x.qual
    (x.optional |> List.map (sprintf "%s:%c:%s" |> Tuple3.uncurry) |> String.concat "\t")

(** Parse given string as a header line if possible. *)
let header_line_of_string s : header_line =

  let parse_tag_value s : string * string =
    let x,y = String.split s ":" in
    if Pcre.pmatch ~rex:header_tag_regexp x then
      (x,y)
    else
      Bad (sprintf "invalid tag %s" x) |> raise
  in
    
  try
    match String.split s "\t" with
      | "@CO", y -> Comment y
      | x, y ->
          if Pcre.pmatch ~rex:record_type_code_regexp x then
            let tag_values = String.nsplit y "\t" |> List.map parse_tag_value in
            NonComment (x, tag_values)
          else
            Bad (sprintf "invalid record type code %s" x) |> raise
  with
      Not_found -> Bad (sprintf "invalid header line: %s" s) |> raise

let optional_field_of_string s = match String.nsplit s ":" with
  | [tag;typ;value] ->
      if Pcre.pmatch ~rex:optional_tag_regexp tag then
        match typ with
          | "A" | "i" | "f" | "Z" | "H" | "B" ->
              tag, typ.[0], value
          | _ ->
              Bad (sprintf "invalid optional field type %s" typ) |> raise
      else
        Bad (sprintf "invalid optional field tag %s" tag) |> raise
  | _ ->
      Bad (sprintf "invalid optional field %s" s) |> raise


let alignment_of_string s : alignment =
  let i field x =
    try int_of_string x
    with Failure _ -> Bad (sprintf "invalid %s: %s" field x) |> raise
  in
  match String.nsplit s "\t" with
    | qname::flag::rname::pos::mapq::cigar::rnext::pnext::tlen::seq::qual::optional ->
        {
          qname;
          flag = i "flag" flag;
          rname;
          pos = i "pos" pos;
          mapq = i "mapq" mapq;
          cigar;
          rnext;
          pnext = i "pnext" pnext;
          tlen = i "tlen" tlen;
          seq;
          qual;
          optional = List.map optional_field_of_string optional
        }
    | _ ->
        Bad (sprintf "invalid alignment line: %s" s) |> raise


let enum_file file =
  let enum = File.lines_of file in
  let rec get_header header = match Enum.peek enum with
    | None -> List.rev header
    | Some x ->
        if String.starts_with x "@" then (
          Enum.junk enum;
          get_header (header_line_of_string x :: header)
        )        
        else
          List.rev header
  in
  let a = get_header [] in
  let b = Enum.map alignment_of_string enum in
  a, b
*)
