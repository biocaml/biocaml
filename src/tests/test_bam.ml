open OUnit
open Core.Std
open CFStream
open Sexplib.Std

module Bam = Biocaml_bam_alt
module Sam = Biocaml_sam

let string_option_p x = Sexp.to_string (<:sexp_of<string option>> x)

let test_read_bam () =
  Bam.with_file "src/tests/data/bam_01.bam" ~f:(fun header alignments ->
      assert_equal ~msg:"Sam version" ~printer:string_option_p (Some "1.0") header.Sam.version ;
      assert_equal ~msg:"Sort order" (Some `Unsorted) header.Sam.sort_order ;
      assert_equal ~msg:"Number of ref sequences" ~printer:string_of_int 22 (List.length header.Sam.ref_seqs) ;
      let alignments = Stream.to_list alignments in
      assert_equal ~msg:"Number of alignments" ~printer:string_of_int 26 (List.length alignments)
    )
  |> ok_exn

let tests = "Bam" >::: [
  "Read Samtools generated BAM" >:: test_read_bam
]
