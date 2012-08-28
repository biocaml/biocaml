open OUnit
open Core.Std

module TS = Biocaml_transform.Pull_based
let make_stream file =
  let t =
    Biocaml_fasta.sequence_parser ~pedantic:true 
      ~sharp_comments:true ~semicolon_comments:true () in
  let stream = TS.of_file ~buffer_size:10 file t in
  stream

let check_next_ok stream v = TS.next stream = `output (Ok v)

let test_parsing_01 stream =
  assert_bool "Name 1" (check_next_ok stream (`header "sequence 1|sid=4"));
  assert_bool "Sequence 1" (check_next_ok stream (`partial_sequence "ATACTGCATGATCGATCGATCG"));
  ignore (TS.next stream);
  assert_bool "Name 2" (check_next_ok stream (`header "sequence 2|sid=42"));
  ignore (TS.next stream);
  assert_bool "Non-EOF" (TS.next stream <> `end_of_stream);
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  assert_bool "EOF again" (TS.next stream = `end_of_stream);
  
  ()

let parse_comments stream =
  assert_bool "comment 1"
    (check_next_ok stream  (`comment " with comments"));
  assert_bool "comment 2"
    (check_next_ok stream  (`comment " at the beginning"));
  ()

let get_empty_line_error_after_two stream =
  ignore (TS.next stream);
  ignore (TS.next stream);
  assert_bool "Error because of empty line (fasta_03.fa)"
    (match TS.next stream with
    | `output (Error (`empty_line _)) -> true | _ -> false);
  ignore (TS.next stream);
  ignore (TS.next stream);
  ignore (TS.next stream);
  assert_bool "Non-EOF" (TS.next stream <> `end_of_stream);
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  ()
  
let malformed stream =
  assert_bool "Name 1" (check_next_ok stream (`header "sequence 1|sid=4"));
  assert_bool "Error because of malformed sequence (fasta_04.fa)"
    (match TS.next stream with
    | `output (Error (`malformed_partial_sequence _)) -> true
    | _ -> false);
  ignore (TS.next stream);
  ignore (TS.next stream);
  ignore (TS.next stream);
  assert_bool "Non-EOF" (TS.next stream <> `end_of_stream);
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  ()
  
let test_printer () =
  let stream_02 = make_stream "src/tests/data/fasta_02.fa" in
  let fasta_printer = Biocaml_fasta.sequence_printer ~comment_char:'#' () in
  let stream = 
    TS.of_feeder (fun () ->
      match TS.next stream_02 with
      | `output (Ok o)  -> Some o
      | `output (Error e) -> failwith "Error while parsing fasta_02.fa"
      | `end_of_stream -> None) fasta_printer in
  let check_next_ok stream v = TS.next stream  = `output v in
  assert_bool "Printer0" (check_next_ok stream  ("# with comments\n"));
  assert_bool "Printer1" (check_next_ok stream  ("# at the beginning\n"));
  assert_bool "Printer2" (check_next_ok stream  (">sequence 1|sid=4\n"));
  assert_bool "Printer3" (check_next_ok stream  ("ATACTGCATGATCGATCGATCG\n"));
  assert_bool "Printer4" (check_next_ok stream  ("ACTGCTAGTAGTCGATCGAT\n"));
  assert_bool "Printer5" (check_next_ok stream  (">sequence 2|sid=42\n"));
  assert_bool "Printer6" (check_next_ok stream  ("ATCGTACTGACTGATCGATGCATGCATG\n"));
  assert_bool "Printer7" (check_next_ok stream  ("ACTACGTACGATCAGTCGATCG\n"));
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  ()
  
let score_parser () =
  let t = 
    Biocaml_fasta.score_parser ~pedantic:true 
      ~sharp_comments:true ~semicolon_comments:true () in
  let stream = TS.of_file ~buffer_size:100 "src/tests/data/fasta_05.fa" t in
  parse_comments stream;
  assert_bool "Name 1" (check_next_ok stream (`header "sequence 1|sid=4"));
  ignore (TS.next stream);
  assert_bool "Sequence 2" (check_next_ok stream (`partial_sequence [42; 43]));
  ignore (TS.next stream);
  assert_bool "Sequence 3"
    (check_next_ok stream (`partial_sequence [32; 32; 32]));
  assert_bool "Error in sequence (score_parser)"
    (match TS.next stream with
    | `output (Error (`malformed_partial_sequence _)) -> true
    | _ -> false);
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  ()
  

let sequence_aggregator_stream file =
  let t = Biocaml_fasta.sequence_parser  () in
  let aggregator = Biocaml_fasta.sequence_aggregator () in
  let transform = Biocaml_transform.bind_result_merge_error t aggregator in
  let stream = TS.of_file ~buffer_size:5 file transform in
  stream
  
let sequence_aggregator () =
  let stream = sequence_aggregator_stream "src/tests/data/fasta_02.fa" in
  assert_bool "seqaggr: 1"
    (check_next_ok stream  ("sequence 1|sid=4",
                            "ATACTGCATGATCGATCGATCGACTGCTAGTAGTCGATCGAT"));
  assert_bool "seqaggr: 2"
    (check_next_ok stream  ("sequence 2|sid=42",
                            "ATCGTACTGACTGATCGATGCATGCATGACTACGTACGATCAGTCGATCG"));
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  assert_bool "EOF" (TS.next stream = `end_of_stream);

  let stream = sequence_aggregator_stream "src/tests/data/fasta_06.fa" in

  assert_bool "Error sequence_aggregator-> unnamed"
    (match TS.next stream with
    | `output (Error (`right (`unnamed_sequence "ATACTGCATGATCGATCGATCG"))) -> true
    | _ -> false);
  ignore (TS.next stream);
  ignore (TS.next stream);
  assert_bool "seqaggr: empty" (check_next_ok stream  ("empty sequence", ""));
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  ()

  
let score_aggregator () =
  let t = 
    Biocaml_fasta.score_parser ~pedantic:true 
      ~sharp_comments:true ~semicolon_comments:true () in
  let aggregator = Biocaml_fasta.score_aggregator () in
  let transform = Biocaml_transform.bind_result_merge_error t aggregator in
  let stream = TS.of_file ~buffer_size:10 "src/tests/data/fasta_05.fa" transform in
  assert_bool "scoaggr: 1"
    (check_next_ok stream  ("sequence 1|sid=4",
                               [42; 42; 224354; 54325543;
                                54354544; 543554; 42; 43]));
  assert_bool "Error score_aggregator -> error"
    (match TS.next stream with
    | `output (Error (`left (`malformed_partial_sequence _))) -> true
    | _ -> false);
  (* After reporting the error the aggregator continues with what it has... *)
  assert_bool "scoaggr: 2"
    (check_next_ok stream  ("sequence 2|sid=42", [32; 32; 32]));
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  ()
  
let sequence_slicer_stream file = 
  let t = Biocaml_fasta.sequence_parser  () in
  let aggregator = Biocaml_fasta.sequence_aggregator () in
  let slicer = Biocaml_fasta.sequence_slicer ~line_width:4 () in
  let transform =
    Biocaml_transform.(map_result
                         (bind_result_merge_error t aggregator) slicer) in
  let stream = TS.of_file ~buffer_size:5 file transform in
  stream

let sequence_slicer () =
  let stream = sequence_slicer_stream "src/tests/data/fasta_02.fa" in
  assert_bool "name 1"   (check_next_ok stream  (`header "sequence 1|sid=4"));
  assert_bool "seq ATAC" (check_next_ok stream  (`partial_sequence "ATAC"));
  assert_bool "seq TGCA" (check_next_ok stream  (`partial_sequence "TGCA"));
  assert_bool "seq TGAT" (check_next_ok stream  (`partial_sequence "TGAT"));
  assert_bool "seq CGAT" (check_next_ok stream  (`partial_sequence "CGAT"));
  assert_bool "seq CGAT" (check_next_ok stream  (`partial_sequence "CGAT"));
  assert_bool "seq CGAC" (check_next_ok stream  (`partial_sequence "CGAC"));
  assert_bool "seq TGCT" (check_next_ok stream  (`partial_sequence "TGCT"));
  assert_bool "seq AGTA" (check_next_ok stream  (`partial_sequence "AGTA"));
  assert_bool "seq GTCG" (check_next_ok stream  (`partial_sequence "GTCG"));
  assert_bool "seq ATCG" (check_next_ok stream  (`partial_sequence "ATCG"));
  assert_bool "seq AT  " (check_next_ok stream  (`partial_sequence "AT"));

  assert_bool "name 2"   (check_next_ok stream (`header "sequence 2|sid=42"));
  assert_bool "seq ATCG" (check_next_ok stream (`partial_sequence "ATCG"));
  assert_bool "seq TACT" (check_next_ok stream (`partial_sequence "TACT"));
  assert_bool "seq GACT" (check_next_ok stream (`partial_sequence "GACT"));
  assert_bool "seq GATC" (check_next_ok stream (`partial_sequence "GATC"));
  assert_bool "seq GATG" (check_next_ok stream (`partial_sequence "GATG"));
  assert_bool "seq CATG" (check_next_ok stream (`partial_sequence "CATG"));
  assert_bool "seq CATG" (check_next_ok stream (`partial_sequence "CATG"));
  assert_bool "seq ACTA" (check_next_ok stream (`partial_sequence "ACTA"));
  assert_bool "seq CGTA" (check_next_ok stream (`partial_sequence "CGTA"));
  assert_bool "seq CGAT" (check_next_ok stream (`partial_sequence "CGAT"));
  assert_bool "seq CAGT" (check_next_ok stream (`partial_sequence "CAGT"));
  assert_bool "seq CGAT" (check_next_ok stream (`partial_sequence "CGAT"));
  assert_bool "seq CG  " (check_next_ok stream (`partial_sequence "CG"));
  ()
    
let score_slicer () =
  let t = 
    Biocaml_fasta.score_parser ~pedantic:true 
      ~sharp_comments:true ~semicolon_comments:true () in
  let aggregator = Biocaml_fasta.score_aggregator () in
  let slicer = Biocaml_fasta.score_slicer ~group_by:3 () in
  let transform =
    Biocaml_transform.(map_result
                         (bind_result_merge_error t aggregator) slicer) in
  let stream = TS.of_file ~buffer_size:10 "src/tests/data/fasta_05.fa" transform in
  assert_bool "name 1" (check_next_ok stream (`header "sequence 1|sid=4"));
  assert_bool "sco: 1" (check_next_ok stream (`partial_sequence [42; 42; 224354;]));
  assert_bool "sco: 2" (check_next_ok stream (`partial_sequence [54325543; 54354544; 543554;]));
  assert_bool "sco: 3" (check_next_ok stream (`partial_sequence [42; 43])); 
  assert_bool "Error score_slicer -> error"
    (match TS.next stream with
    | `output (Error (`left (`malformed_partial_sequence _))) -> true
    | _ -> false);
  (* After reporting the error the aggregator continues with what it has... *)
  assert_bool "name 2" (check_next_ok stream (`header "sequence 2|sid=42"));
  assert_bool "sco: 4" (check_next_ok stream (`partial_sequence [32; 32; 32]));
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  ()

let test_parser () =

  test_parsing_01 (make_stream "src/tests/data/fasta_01.fa");

  let stream_02 = make_stream "src/tests/data/fasta_02.fa" in
  parse_comments stream_02;
  test_parsing_01 stream_02;

  let stream_03 = make_stream "src/tests/data/fasta_03.fa" in
  parse_comments stream_03;
  get_empty_line_error_after_two stream_03;

  let stream_04 = make_stream "src/tests/data/fasta_04.fa" in
  parse_comments stream_04;
  malformed stream_04;

  score_parser ();
  ()
    
    
let tests = "Fasta" >::: [
  "Reading FASTA" >:: test_parser;
  "Writing FASTA" >:: test_printer;
  "FASTA's sequence aggregator" >:: sequence_aggregator;
  "FASTA's score aggregator" >:: score_aggregator;
  "FASTA's sequence slicer" >:: sequence_slicer;
  "FASTA's score slicer" >:: score_slicer;
]
