type collection = Core | Phylofacts | CNE | PBM | PBM_HOMEO | PBM_HLH | FAM | SPLICE | POLII

type matrix = private {
  id : string ;
  collection : collection ;
  tf_id : string ;
  tf_family : string ;
  information_contents : float ;
  comment : string option ;
  accession : string option ;
  medline : string ;
}

val load : string -> matrix list
