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
  strand     : [`plus | `minus | `not_applicable | `unknown ] ;
  phase      : int option ;
  attributes : (string * string list) list ;
}
(** The type of the GFF records/rows. *)

type item = [ `comment of string | `record of record ]
(** The items being output by the parser. *)

val record :
  ?source:string ->
  ?feature:string ->
  ?score:float ->
  ?strand:[`plus | `minus | `not_applicable | `unknown ] ->
  ?phase:int ->
  ?attributes:(string * string list) list ->
  string -> int -> int -> record

(* val item_of_line : Line.t -> (item, string) result *)
val line_of_item : [`two | `three] -> item -> Line.t
