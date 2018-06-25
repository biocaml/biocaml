(** GFF files.

    Versions 2 and 3 are supported. The only difference is the
    delimiter used for tag-value pairs in the attribute list: [3] uses
    an equal sign, and [2] uses a space. Version [3] also has
    additional requirements, e.g. the [feature] must be a sequence
    ontology term, but these are not checked.


    More information: {ul
      {- Version 2:
         {{:http://www.sanger.ac.uk/resources/software/gff/spec.html
            }www.sanger.ac.uk/resources/software/gff/spec.html},
         {{:http://gmod.org/wiki/GFF2}gmod.org/wiki/GFF2}
      }
      {- Version 3:
         {{:http://www.sequenceontology.org/gff3.shtml
             }www.sequenceontology.org/gff3.shtml},
         {{:http://gmod.org/wiki/GFF3
            }gmod.org/wiki/GFF3}
      }
    }
*)

(** {2 GFF Item Types} *)

type record = {
  seqname    : string ;
  source     : string option ;
  feature    : string option ;
  start_pos  : int ;
  stop_pos   : int ;
  score      : float option ;
  strand     : [`Plus | `Minus | `Not_stranded | `Unknown ] ;
  phase      : int option ;
  attributes : (string * string list) list ;
}
(** The type of the GFF records/rows. *)

type item = [ `Comment of string | `Record of record ]
(** The items being output by the parser. *)

val record :
  ?source:string ->
  ?feature:string ->
  ?score:float ->
  ?strand:[`Plus | `Minus | `Not_stranded | `Unknown ] ->
  ?phase:int ->
  ?attributes:(string * string list) list ->
  string -> int -> int -> record

val gff3_item_of_line : Line.t -> (item, [> `Msg of string]) Caml.result
val line_of_item : [`two | `three] -> item -> Line.t
