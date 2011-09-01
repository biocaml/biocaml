open Batteries_uni;; open Printf

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
  get_header [], Enum.map alignment_of_string enum
