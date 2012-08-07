(** SAM files. *)


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

val parser: ?filename:string -> unit ->
  (string, t, parse_error) Biocaml_transform.t
    
val printer: unit ->
  (t, string, Biocaml_transform.no_error) Biocaml_transform.t

(*
open Batteries

exception Bad of string

type record_type_code = string
    (** String is exactly three characters, matching the regular
        expression '@[A-Za-z][A-Za-z]'. *)

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
  optional : (string * char * string) list (** custom tag-value pairs included beyond required 11 *)
}

val record_type_code_of_header_line : header_line -> string

val enum_file : string -> (header * alignment Enum.t)

val header_line_to_string : header_line -> string
val header_to_string : header -> string
val alignment_to_string : alignment -> string
*)
