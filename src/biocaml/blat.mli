type psl_record = {
  psl_matches : int;
  psl_misMatches : int;
  psl_repMatches : int;
  psl_nCount : int;
  psl_qNumInsert : int;
  psl_qBaseInsert : int;
  psl_tNumInsert : int;
  psl_tBaseInsert : int;
  psl_strand : bool * bool;
  psl_qName : string;
  psl_qSize : int;
  psl_qStart : int;
  psl_qEnd : int;
  psl_tName : string;
  psl_tSize : int;
  psl_tStart : int;
  psl_tEnd : int;
  psl_blockCount : int;
  psl_blockSizes : int list;
  psl_qStarts : int list;
  psl_tStarts : int list;
}

val invoke_blat_files : string -> string -> string -> string -> string list
val invoke_blat_mem : string -> Fasta.t -> Fasta.t -> psl_record list
