open OUnit
open Biocaml_internal_pervasives
module Fasta = Biocaml_fasta
module Transform = Biocaml_transform

let make_stream file =
  let t =
    Fasta.Transform.string_to_char_seq_raw_item ~pedantic:true 
      ~sharp_comments:true ~semicolon_comments:true () in
  In_channel.with_file file ~f:(fun ic ->
    Transform.in_channel_strings_to_stream ~buffer_size:10 ic t
  )

let check_next_ok stream v = Stream.next stream = Some (Ok v)

let test_parsing_01 stream =
  assert_bool "Name 1" (check_next_ok stream (`header "sequence 1|sid=4"));
  assert_bool "Sequence 1" (check_next_ok stream (`partial_sequence "ATACTGCATGATCGATCGATCG"));
  ignore (Stream.next stream);
  assert_bool "Name 2" (check_next_ok stream (`header "sequence 2|sid=42"));
  ignore (Stream.next stream);
  assert_bool "Non-EOF" (Stream.next stream <> None);
  assert_bool "EOF" (Stream.next stream = None);
  assert_bool "EOF again" (Stream.next stream = None);
  
  ()

let parse_comments stream =
  assert_bool "comment 1"
    (check_next_ok stream  (`comment " with comments"));
  assert_bool "comment 2"
    (check_next_ok stream  (`comment " at the beginning"));
  ()

let get_empty_line_error_after_two stream =
  ignore (Stream.next stream);
  ignore (Stream.next stream);
  assert_bool "Error because of empty line (fasta_03.fa)"
    (match Stream.next stream with
    | Some (Error (`empty_line _)) -> true | _ -> false);
  ignore (Stream.next stream);
  ignore (Stream.next stream);
  ignore (Stream.next stream);
  assert_bool "Non-EOF" (Stream.next stream <> None);
  assert_bool "EOF" (Stream.next stream = None);
  ()
  
let malformed stream =
  assert_bool "Name 1" (check_next_ok stream (`header "sequence 1|sid=4"));
  assert_bool "Error because of malformed sequence (fasta_04.fa)"
    (match Stream.next stream with
    | Some (Error (`malformed_partial_sequence _)) -> true
    | _ -> false);
  ignore (Stream.next stream);
  ignore (Stream.next stream);
  ignore (Stream.next stream);
  assert_bool "Non-EOF" (Stream.next stream <> None);
  assert_bool "EOF" (Stream.next stream = None);
  ()
  
let test_printer () =
  let stream_02 = make_stream "src/tests/data/fasta_02.fa" in
  let fasta_printer =
    Fasta.Transform.char_seq_raw_item_to_string ~comment_char:'#' () in
  let stream =
    Transform.to_stream_fun fasta_printer (Stream.from (fun _ ->
      match Stream.next stream_02 with
      | Some (Ok o)  -> Some o
      | Some (Error e) -> failwith "Error while parsing fasta_02.fa"
      | None -> None)
    ) in
  let check_next_ok stream v = Stream.next stream  = Some v in
  assert_bool "Printer0" (check_next_ok stream  ("# with comments\n"));
  assert_bool "Printer1" (check_next_ok stream  ("# at the beginning\n"));
  assert_bool "Printer2" (check_next_ok stream  (">sequence 1|sid=4\n"));
  assert_bool "Printer3" (check_next_ok stream  ("ATACTGCATGATCGATCGATCG\n"));
  assert_bool "Printer4" (check_next_ok stream  ("ACTGCTAGTAGTCGATCGAT\n"));
  assert_bool "Printer5" (check_next_ok stream  (">sequence 2|sid=42\n"));
  assert_bool "Printer6" (check_next_ok stream  ("ATCGTACTGACTGATCGATGCATGCATG\n"));
  assert_bool "Printer7" (check_next_ok stream  ("ACTACGTACGATCAGTCGATCG\n"));
  assert_bool "EOF" (Stream.next stream = None);
  ()
  
let score_parser () =
  let t = 
    Fasta.Transform.string_to_int_seq_raw_item ~pedantic:true 
      ~sharp_comments:true ~semicolon_comments:true () in
  let stream = In_channel.with_file "src/tests/data/fasta_05.fa" ~f:(fun ic ->
    Transform.in_channel_strings_to_stream ~buffer_size:100 ic t) in
  parse_comments stream;
  assert_bool "Name 1" (check_next_ok stream (`header "sequence 1|sid=4"));
  ignore (Stream.next stream);
  assert_bool "Sequence 2" (check_next_ok stream (`partial_sequence [42; 43]));
  ignore (Stream.next stream);
  assert_bool "Sequence 3"
    (check_next_ok stream (`partial_sequence [32; 32; 32]));
  assert_bool "Error in sequence (score_parser)"
    (match Stream.next stream with
    | Some (Error (`malformed_partial_sequence _)) -> true
    | _ -> false);
  assert_bool "EOF" (Stream.next stream = None);
  ()
  

let sequence_aggregator_stream file =
  let t = Fasta.Transform.string_to_char_seq_raw_item  () in
  let aggregator = Fasta.Transform.char_seq_raw_item_to_item () in
  let transform = Biocaml_transform.compose_results_merge_error t aggregator in
  In_channel.with_file file ~f:(fun ic ->
    Transform.in_channel_strings_to_stream ~buffer_size:5 ic transform)

let sequence_aggregator () =
  let stream = sequence_aggregator_stream "src/tests/data/fasta_02.fa" in
  assert_bool "seqaggr: 1"
    (check_next_ok stream  {Fasta.header="sequence 1|sid=4";
                            sequence="ATACTGCATGATCGATCGATCGACTGCTAGTAGTCGATCGAT"});
  assert_bool "seqaggr: 2"
    (check_next_ok stream  {Fasta.header="sequence 2|sid=42";
                            sequence="ATCGTACTGACTGATCGATGCATGCATGACTACGTACGATCAGTCGATCG"});
  assert_bool "EOF" (Stream.next stream = None);
  assert_bool "EOF" (Stream.next stream = None);

  let stream = sequence_aggregator_stream "src/tests/data/fasta_06.fa" in

  assert_bool "Error sequence_aggregator-> unnamed"
    (match Stream.next stream with
    | Some (Error (`right (`unnamed_char_seq "ATACTGCATGATCGATCGATCG"))) -> true
    | _ -> false);
  ignore (Stream.next stream);
  ignore (Stream.next stream);
  assert_bool "seqaggr: empty" (check_next_ok stream  {Fasta.header="empty sequence"; sequence=""});
  assert_bool "EOF" (Stream.next stream = None);
  ()

  
let score_aggregator () =
  let t = 
    Fasta.Transform.string_to_int_seq_raw_item ~pedantic:true 
      ~sharp_comments:true ~semicolon_comments:true () in
  let aggregator = Fasta.Transform.int_seq_raw_item_to_item () in
  let transform = Biocaml_transform.compose_results_merge_error t aggregator in
  let stream = In_channel.with_file "src/tests/data/fasta_05.fa" ~f:(fun ic ->
    Transform.in_channel_strings_to_stream ~buffer_size:10 ic transform) in
  assert_bool "scoaggr: 1"
    (check_next_ok stream  {Fasta.header="sequence 1|sid=4";
                            sequence=[42; 42; 224354; 54325543;
                                      54354544; 543554; 42; 43]});
  assert_bool "Error score_aggregator -> error"
    (match Stream.next stream with
    | Some (Error (`left (`malformed_partial_sequence _))) -> true
    | _ -> false);
  (* After reporting the error the aggregator continues with what it has... *)
  assert_bool "scoaggr: 2"
    (check_next_ok stream  {Fasta.header="sequence 2|sid=42"; sequence=[32; 32; 32]});
  assert_bool "EOF" (Stream.next stream = None);
  assert_bool "EOF" (Stream.next stream = None);
  ()
  
let sequence_slicer_stream file = 
  let t = Fasta.Transform.string_to_char_seq_raw_item  () in
  let aggregator = Fasta.Transform.char_seq_raw_item_to_item () in
  let slicer =
    Fasta.Transform.char_seq_item_to_raw_item ~items_per_line:4 () in
  let transform =
    Biocaml_transform.(compose_result_left
                         (compose_results_merge_error t aggregator) slicer) in
  In_channel.with_file file ~f:(fun ic ->
    Transform.in_channel_strings_to_stream ~buffer_size:5 ic transform)

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
    Fasta.Transform.string_to_int_seq_raw_item ~pedantic:true 
      ~sharp_comments:true ~semicolon_comments:true () in
  let aggregator = Fasta.Transform.int_seq_raw_item_to_item () in
  let slicer = Fasta.Transform.int_seq_item_to_raw_item ~items_per_line:3 () in
  let transform =
    Biocaml_transform.(compose_result_left
                         (compose_results_merge_error t aggregator) slicer) in
  let stream = In_channel.with_file "src/tests/data/fasta_05.fa" ~f:(fun ic ->
    Transform.in_channel_strings_to_stream ~buffer_size:10 ic transform) in
  assert_bool "name 1" (check_next_ok stream (`header "sequence 1|sid=4"));
  assert_bool "sco: 1" (check_next_ok stream (`partial_sequence [42; 42; 224354;]));
  assert_bool "sco: 2" (check_next_ok stream (`partial_sequence [54325543; 54354544; 543554;]));
  assert_bool "sco: 3" (check_next_ok stream (`partial_sequence [42; 43])); 
  assert_bool "Error score_slicer -> error"
    (match Stream.next stream with
    | Some (Error (`left (`malformed_partial_sequence _))) -> true
    | _ -> false);
  (* After reporting the error the aggregator continues with what it has... *)
  assert_bool "name 2" (check_next_ok stream (`header "sequence 2|sid=42"));
  assert_bool "sco: 4" (check_next_ok stream (`partial_sequence [32; 32; 32]));
  assert_bool "EOF" (Stream.next stream = None);
  assert_bool "EOF" (Stream.next stream = None);
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
