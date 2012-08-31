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
(** The type of the GFF records/rows. *)
  
type stream_item = [ `comment of string | `record of t ]
(** The items being output by the parser. *)

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
(** The possible parsing errors. *)
  
type tag = [ `version of [`two | `three] | `pedantic ] with sexp
  
module Transform: sig
  val string_to_item:
    ?filename:string ->
    ?tags: tag list ->
    unit ->
    (string, (stream_item, parse_error) Core.Result.t) Biocaml_transform.t
(** Create a parsing [Biocaml_transform.t] for a given version. *)

  val item_to_string:
    ?tags: tag list ->
    unit ->
    (stream_item, string) Biocaml_transform.t
(** Create a printer for a given version. *)

end
