(** GFF files. Parser silently skips comment lines, blank lines, and
    pragmas.

    Versions 2 and 3 are supported. The only difference is the
    delimiter used for tag-value pairs in the attribute list: [3] uses
    an equal sign, and [2] uses a space. Version [3] also has
    additional requirements, e.g. the [feature] must be a sequence
    ontology term, but these are not checked. *)



type t = {
  seqname: string;
  source: string option;
  feature: string option;
  pos: int * int;

  score: float option;
  strand: [`plus | `minus | `not_applicable | `unknown ];
  phase: int option;
  attributes: (string * string list) list;
}
type stream_item = [ `comment of string | `record of t ]

type parse_error = 
[ `cannot_parse_float of Biocaml_pos.t * string
| `cannot_parse_int of Biocaml_pos.t * string
| `cannot_parse_strand of Biocaml_pos.t * string
| `cannot_parse_string of Biocaml_pos.t * string
| `empty_line of Biocaml_pos.t
| `incomplete_input of Biocaml_pos.t * string list * string option
| `wrong_attributes of Biocaml_pos.t * string
| `wrong_row of Biocaml_pos.t * string
| `wrong_url_escaping of Biocaml_pos.t * string ]

  
val parser:
  ?filename:string ->
  ?pedantic:bool ->
  ?version:[ `two | `three ] ->
  unit ->
  (string, stream_item, parse_error) Biocaml_transform.t

val printer:
  ?version:[ `two | `three ] ->
  unit ->
  (stream_item, string, Biocaml_transform.no_error) Biocaml_transform.t

