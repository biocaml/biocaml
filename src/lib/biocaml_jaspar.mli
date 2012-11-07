type collection = Core | Phylofacts | CNE | PBM | PBM_HOMEO | PBM_HLH | FAM | SPLICE | POLII

type motif = private {
  id : string ;
  collection : collection ;
  factor_name : string ;
  factor_class : string ;
  information_contents : float ;
  comment : string option ;
  accession : string option ;
  medline : string ;
  matrix : int array array ;
}

val load : string -> motif list













