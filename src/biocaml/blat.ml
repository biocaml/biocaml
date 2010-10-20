type psl_record = {
 psl_matches: int;
 psl_misMatches: int;
 psl_repMatches: int;
 psl_nCount: int;
 psl_qNumInsert: int;
 psl_qBaseInsert: int;
 psl_tNumInsert: int;
 psl_tBaseInsert: int;
 psl_strand: bool * bool;
 psl_qName: string;
 psl_qSize: int;
 psl_qStart: int;
 psl_qEnd: int;
 psl_tName: string;
 psl_tSize: int;
 psl_tStart: int;
 psl_tEnd: int;
 psl_blockCount: int;
 psl_blockSizes: int list;
 psl_qStarts: int list;
 psl_tStarts: int list;
}

let parse_psl_line line =
 let extract_int strlist pos fieldname = (try int_of_string (List.nth strlist pos) with Failure f -> invalid_arg ("unknown value in "^fieldname)) in
 let extract_intlist strlist pos fieldname = (try List.map int_of_string (Str.split (Str.regexp_string ",") (List.nth strlist pos)) with Failure f -> invalid_arg ("unknown value in "^fieldname)) in
  let termlist = Str.split (Str.regexp_string "	") line in
    if ((List.length termlist) = 21 && ((List.nth termlist 0) <> "match")) then 
     Some({
      psl_matches = extract_int termlist 0 "matches";
      psl_misMatches = extract_int termlist 1 "misMatches";
      psl_repMatches = extract_int termlist 2 "repMatches";
      psl_nCount = extract_int termlist 3 "nCount";
      psl_qNumInsert = extract_int termlist 4 "qNumInsert";
      psl_qBaseInsert = extract_int termlist 5 "qBaseInsert";
      psl_tNumInsert = extract_int termlist 6 "tNumInsert";
      psl_tBaseInsert = extract_int termlist 7 "tBaseInsert";
      psl_strand = (function 
                       | "+" | "+-" -> (true,false)  
                       | "-" | "--" -> (false,false) 
                       | "++" -> (true,true) 
                       | "-+" -> (false,true)
                       | _ -> invalid_arg "unknown value in strand"
                       ) (List.nth termlist 8);
      psl_qName = (List.nth termlist 9);
      psl_qSize = extract_int termlist 10 "qSize";
      psl_qStart = extract_int termlist 11 "qStart";
      psl_qEnd = extract_int termlist 12 "qEnd";
      psl_tName = (List.nth termlist 13);
      psl_tSize = extract_int termlist 14 "tSize";
      psl_tStart = extract_int termlist 15 "tStart";
      psl_tEnd = extract_int termlist 16 "tEnd";
      psl_blockCount = extract_int termlist 17 "blockCount";
      psl_blockSizes = extract_intlist termlist 18 "blockSizes";
      psl_qStarts = extract_intlist termlist 19 "qStarts";
      psl_tStarts = extract_intlist termlist 20 "tStarts";
     }) 
    else None

let invoke_blat_files path_to_blat target query outfile =                                (*TODO optional path*)
 InvokeCmdline.invoke_cmd (path_to_blat ^ "blat") [target; query; outfile]

let invoke_blat_mem path_to_blat target_fasta query_fasta =
  let target_file = (Filename.temp_file "" ".fa") in (Fasta.to_file target_fasta target_file);
  let query_file = (Filename.temp_file "" ".fa") in (Fasta.to_file query_fasta query_file);
  let out_file = (Filename.temp_file "" ".psl") in
  ignore (invoke_blat_files path_to_blat target_file query_file out_file); (*should we use the console output somehow? *)
  let res = InvokeCmdline.parse_lines out_file parse_psl_line in 
  Sys.remove (target_file); Sys.remove (query_file); Sys.remove (out_file); 
  res
