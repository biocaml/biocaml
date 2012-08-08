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


let alignment_to_string x =
  sprintf "%s\t%d\t%s\t%d\t%d\t%s\t%s\t%d\t%d\t%s\t%s\t%s\n"
    x.qname x.flag x.rname x.pos x.mapq x.cigar x.rnext x.pnext x.tlen x.seq x.qual
    (List.map x.optional (fun (a,b,c) -> sprintf "%s:%c:%s" a b c) |!
        String.concat ~sep:"\t")

let printer () =
  let module PQ = Biocaml_transform.Printer_queue in
  let printer =
    PQ.make ~to_string:(function
    | `comment c -> sprintf "@CO\t%s\n" c
    | `header_line (t, l) -> sprintf "@%s\t%s\n" t
      (List.map l (fun (a,b) -> sprintf "%s:%s" a b) |! String.concat ~sep:"\t")
    | `alignment a -> alignment_to_string a
    ) () in
  Biocaml_transform.make_stoppable ~name:"sam_printer" ()
    ~feed:(fun r -> PQ.feed printer r)
    ~next:(fun stopped ->
      match (PQ.flush printer) with
      | "" -> if stopped then `end_of_stream else `not_ready
      | s -> `output s)

    

