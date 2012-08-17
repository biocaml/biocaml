(** SAM files. *)


type raw_alignment = {
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
(** The contents of an alignment line. *)

type raw_item = [
| `comment of string
| `header of string * (string * string) list
| `alignment of raw_alignment
]
(** The "items" of a parsed SAM file stream. *)

type raw_parsing_error = [
| `incomplete_input of Biocaml_pos.t * string list * string option
| `invalid_header_tag of Biocaml_pos.t * string
| `invalid_tag_value_list of Biocaml_pos.t * string list
| `not_an_int of Biocaml_pos.t * string * string
| `wrong_alignment of Biocaml_pos.t * string
| `wrong_optional_field of Biocaml_pos.t * string
]
(** The possible errors one can get while parsing SAM files. *)

  
val raw_parser: ?filename:string -> unit ->
  (string, raw_item, raw_parsing_error) Biocaml_transform.t
(** Create a parsing "stoppable" transform. *)   

    
val raw_printer: unit ->
  (raw_item, string, Biocaml_transform.no_error) Biocaml_transform.t
(** Create a printing "stoppable" transform. *)   

