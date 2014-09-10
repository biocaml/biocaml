open OUnit
open Core.Std
open CFStream
open Sexplib.Std

module Bam = Biocaml_bam_alt
module Sam = Biocaml_sam

let string_option_p x = Sexp.to_string (<:sexp_of<string option>> x)

let test_read () =
  Bam.with_file "src/tests/data/bam_01.bam" ~f:(fun header alignments ->
      assert_equal ~msg:"Sam version" ~printer:string_option_p (Some "1.0") header.Sam.version ;
      assert_equal ~msg:"Sort order" (Some `Unsorted) header.Sam.sort_order ;
      assert_equal ~msg:"Number of ref sequences" ~printer:string_of_int 22 (List.length header.Sam.ref_seqs) ;
      let alignments = Stream.to_list alignments in
      assert_equal ~msg:"Number of alignments" ~printer:string_of_int 26 (List.length alignments) ;
      Ok ()
    )
  |> ok_exn

let test_read_write_and_read () =
  let bamfile = "src/tests/data/bam_01.bam" in
  Utils.with_temp_file "biocaml" ".bam" ~f:(fun fn ->
      Bam.with_file bamfile ~f:(fun header alignments ->
          Out_channel.with_file fn ~f:(Bam.write header (Stream.map alignments ~f:ok_exn)) ;
          Ok ()
        ) |> ok_exn ;
      Bam.with_file bamfile ~f:(fun ref_header ref_alignments ->
          Bam.with_file fn ~f:(fun header alignments ->
              try Stream.map2_exn ref_alignments alignments ~f:(fun ref_al al -> ()) |> fun _ -> Ok ()
              with Stream.Expected_streams_of_equal_length -> assert_failure "Original and written files don't have the same number of alignments"
            )
        )
    )
  |> ok_exn

let () = test_read_write_and_read ()

let tests = "Bam" >::: [
  "Read Samtools generated BAM" >:: test_read ;
  "Read, write and re-read a Samtools generated BAM" >:: test_read_write_and_read
]
