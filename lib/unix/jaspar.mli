(** Access to Jaspar database *)

(** The possible kinds of motifs. *)
type collection =
  Core | Phylofacts | CNE | PBM | PBM_HOMEO | PBM_HLH | FAM | SPLICE | POLII

type motif = private {
  id : string ;
  jaspar_id : string ;
  collection : collection ;
  factor_name : string ;
  factor_class : string ;
  family : string option ;
  comment : string option ;
  medline : string ;
  matrix : int array array ;
}
(** A Jaspar motif *)

val load : string -> motif list
(** Loads a database in SQL dump format, as available at {{:http://jaspar.genereg.net/html/DOWNLOAD/database/}Jaspar website} *)













