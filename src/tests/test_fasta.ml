open OUnit
open Core.Std

module TS = Biocaml_transform.Pull_based
let make_stream file =
  let parser =
    Biocaml_fasta.sequence_parser ~pedantic:true 
      ~sharp_comments:true ~semicolon_comments:true () in
  let stream = TS.of_file ~buffer_size:10 file parser in
  stream

let test_parsing_01 stream =
  assert_bool "Name 1" (TS.next stream = `output (`name "sequence 1|sid=4"));
  assert_bool "Sequence 1" (TS.next stream =
      `output (`partial_sequence "ATACTGCATGATCGATCGATCG"));
  ignore (TS.next stream);
  assert_bool "Name 2" (TS.next stream = `output (`name "sequence 2|sid=42"));
  ignore (TS.next stream);
  assert_bool "Non-EOF" (TS.next stream <> `end_of_stream);
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  assert_bool "EOF again" (TS.next stream = `end_of_stream);
  
  ()

let parse_comments stream =
  assert_bool "comment 1" (TS.next stream = `output (`comment " with comments"));
  assert_bool "comment 2" (TS.next stream = `output (`comment " at the beginning"));
  ()

let get_empty_line_error_after_two stream =
  ignore (TS.next stream);
  ignore (TS.next stream);
  assert_bool "Error because of empty line (fasta_03.fa)"
    (match TS.next stream with `error (`empty_line _) -> true | _ -> false);
  ignore (TS.next stream);
  ignore (TS.next stream);
  ignore (TS.next stream);
  assert_bool "Non-EOF" (TS.next stream <> `end_of_stream);
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  ()
  
let malformed stream =
  assert_bool "Name 1" (TS.next stream = `output (`name "sequence 1|sid=4"));
  assert_bool "Error because of malformed sequence (fasta_04.fa)"
    (match TS.next stream with `error (`malformed_partial_sequence _) -> true
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
      | `output o  -> Some o
      | `error e -> failwith "Error while parsing fasta_02.fa"
      | `end_of_stream -> None) fasta_printer in
  assert_bool "Printer0" (TS.next stream = `output ("# with comments\n"));
  assert_bool "Printer1" (TS.next stream = `output ("# at the beginning\n"));
  assert_bool "Printer2" (TS.next stream = `output (">sequence 1|sid=4\n"));
  assert_bool "Printer3" (TS.next stream = `output ("ATACTGCATGATCGATCGATCG\n"));
  assert_bool "Printer4" (TS.next stream = `output ("ACTGCTAGTAGTCGATCGAT\n"));
  assert_bool "Printer5" (TS.next stream = `output (">sequence 2|sid=42\n"));
  assert_bool "Printer6" (TS.next stream = `output ("ATCGTACTGACTGATCGATGCATGCATG\n"));
  assert_bool "Printer7" (TS.next stream = `output ("ACTACGTACGATCAGTCGATCG\n"));
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  ()
  
let score_parser () =
  let parser = 
    Biocaml_fasta.score_parser ~pedantic:true 
      ~sharp_comments:true ~semicolon_comments:true () in
  let stream = TS.of_file ~buffer_size:100 "src/tests/data/fasta_05.fa" parser in
  parse_comments stream;
  assert_bool "Name 1" (TS.next stream = `output (`name "sequence 1|sid=4"));
  ignore (TS.next stream);
  assert_bool "Sequence 2"
    (TS.next stream = `output (`partial_sequence [42.; 43.]));
  ignore (TS.next stream);
  assert_bool "Sequence 3"
    (TS.next stream = `output (`partial_sequence [32.; 32.; 32.]));
  assert_bool "Error in sequence (score_parser)"
    (match TS.next stream with `error (`malformed_partial_sequence _) -> true
    | _ -> false);
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  ()
  

let sequence_aggregator_stream file =
  let parser = Biocaml_fasta.sequence_parser  () in
  let aggregator = Biocaml_fasta.sequence_aggregator () in
  let transform = Biocaml_transform.compose parser aggregator in
  let stream = TS.of_file ~buffer_size:5 file transform in
  stream
  
let sequence_aggregator () =
  let stream = sequence_aggregator_stream "src/tests/data/fasta_02.fa" in
  assert_bool "seqaggr: 1"
    (TS.next stream = `output ("sequence 1|sid=4",
                               "ATACTGCATGATCGATCGATCGACTGCTAGTAGTCGATCGAT"));
  assert_bool "seqaggr: 2"
    (TS.next stream = `output ("sequence 2|sid=42",
                               "ATCGTACTGACTGATCGATGCATGCATGACTACGTACGATCAGTCGATCG"));
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  assert_bool "EOF" (TS.next stream = `end_of_stream);

  let stream = sequence_aggregator_stream "src/tests/data/fasta_06.fa" in

  assert_bool "Error sequence_aggregator-> unnamed"
    (match TS.next stream with
    | `error (`right (`unnamed_sequence "ATACTGCATGATCGATCGATCG")) -> true
    | _ -> false);
  ignore (TS.next stream);
  ignore (TS.next stream);
  assert_bool "seqaggr: empty" (TS.next stream = `output ("empty sequence", ""));
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  ()

  
let score_aggregator () =
  let parser = 
    Biocaml_fasta.score_parser ~pedantic:true 
      ~sharp_comments:true ~semicolon_comments:true () in
  let aggregator = Biocaml_fasta.score_aggregator () in
  let transform = Biocaml_transform.compose parser aggregator in
  let stream = TS.of_file ~buffer_size:10 "src/tests/data/fasta_05.fa" transform in
  assert_bool "scoaggr: 1"
    (TS.next stream = `output ("sequence 1|sid=4",
                               [42.; 42.; 224354.; 54325.543;
                                543.54544; 543554.; 42.; 43.]));
  assert_bool "Error score_aggregator -> error"
    (match TS.next stream with
    | `error (`left (`malformed_partial_sequence _)) -> true
    | _ -> false);
  (* After reporting the error the aggregator continues with what it has... *)
  assert_bool "scoaggr: 2"
    (TS.next stream = `output ("sequence 2|sid=42", [32.; 32.; 32.]));
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  assert_bool "EOF" (TS.next stream = `end_of_stream);
  ()
  
let sequence_slicer_stream file = 
  let parser = Biocaml_fasta.sequence_parser  () in
  let aggregator = Biocaml_fasta.sequence_aggregator () in
  let slicer = Biocaml_fasta.sequence_slicer ~line_width:4 () in
  let transform =
    Biocaml_transform.(
      compose (compose parser aggregator) slicer
    ) in
  let stream = TS.of_file ~buffer_size:5 file transform in
  stream

let sequence_slicer () =
  let stream = sequence_slicer_stream "src/tests/data/fasta_02.fa" in
  assert_bool "name 1" (TS.next stream = `output (`name "sequence 1|sid=4"));
  assert_bool "seq ATAC" (TS.next stream = `output (`partial_sequence "ATAC"));
  assert_bool "seq TGCA" (TS.next stream = `output (`partial_sequence "TGCA"));
  assert_bool "seq TGAT" (TS.next stream = `output (`partial_sequence "TGAT"));
  assert_bool "seq CGAT" (TS.next stream = `output (`partial_sequence "CGAT"));
  assert_bool "seq CGAT" (TS.next stream = `output (`partial_sequence "CGAT"));
  assert_bool "seq CGAC" (TS.next stream = `output (`partial_sequence "CGAC"));
  assert_bool "seq TGCT" (TS.next stream = `output (`partial_sequence "TGCT"));
  assert_bool "seq AGTA" (TS.next stream = `output (`partial_sequence "AGTA"));
  assert_bool "seq GTCG" (TS.next stream = `output (`partial_sequence "GTCG"));
  assert_bool "seq ATCG" (TS.next stream = `output (`partial_sequence "ATCG"));
  assert_bool "seq AT  " (TS.next stream = `output (`partial_sequence "AT"));

  assert_bool "name 2" (TS.next stream = `output (`name "sequence 2|sid=42"));
  assert_bool "seq ATCG" (TS.next stream = `output (`partial_sequence "ATCG"));
  assert_bool "seq TACT" (TS.next stream = `output (`partial_sequence "TACT"));
  assert_bool "seq GACT" (TS.next stream = `output (`partial_sequence "GACT"));
  assert_bool "seq GATC" (TS.next stream = `output (`partial_sequence "GATC"));
  assert_bool "seq GATG" (TS.next stream = `output (`partial_sequence "GATG"));
  assert_bool "seq CATG" (TS.next stream = `output (`partial_sequence "CATG"));
  assert_bool "seq CATG" (TS.next stream = `output (`partial_sequence "CATG"));
  assert_bool "seq ACTA" (TS.next stream = `output (`partial_sequence "ACTA"));
  assert_bool "seq CGTA" (TS.next stream = `output (`partial_sequence "CGTA"));
  assert_bool "seq CGAT" (TS.next stream = `output (`partial_sequence "CGAT"));
  assert_bool "seq CAGT" (TS.next stream = `output (`partial_sequence "CAGT"));
  assert_bool "seq CGAT" (TS.next stream = `output (`partial_sequence "CGAT"));
  assert_bool "seq CG  " (TS.next stream = `output (`partial_sequence "CG"));
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
]
