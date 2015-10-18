open Core.Std
open CFStream
module Bam = Biocaml_unix.Std.Bam_alt
module Sam = Biocaml_unix.Std.Sam
open OUnit
open Utils.Printer

let assert_alignment ~qname ~rname ~mapq ~n_cigar_ops ~seq header al =
  assert_equal ~msg:"wrong rname"     ~printer:(option string) rname    (Bam.Alignment0.rname al header |> ok_exn) ;
  assert_equal ~msg:"wrong mapq"      ~printer:(option int)    mapq (Bam.Alignment0.mapq al) ;
  assert_equal ~msg:"wrong tlen"      ~printer:(option int)    None (Bam.Alignment0.tlen al) ;
  assert_equal ~msg:"wrong read_name" ~printer:(option string) qname (Bam.Alignment0.qname al) ;
  assert_equal ~msg:"wrong n_cigar_ops" ~printer:int           n_cigar_ops (List.length (ok_exn (Bam.Alignment0.cigar al))) ;
  assert_equal ~msg:"wrong seq" ~printer:ident seq (Option.value_exn (Bam.Alignment0.seq al)) ;
  ()

let assert_alignments header al1 al2 =
  let open Bam.Alignment0 in
  assert_equal ~msg:"rname"       ~printer:(or_error (option string)) (rname al1 header) (rname al2 header) ;
  assert_equal ~msg:"mapq"        ~printer:(option int)               (mapq al1) (mapq al2) ;
  assert_equal ~msg:"tlen"        ~printer:(option int)               (tlen al1) (tlen al2) ;
  assert_equal ~msg:"read_name"   ~printer:(option string)            (qname al1) (qname al2) ;
  assert_equal ~msg:"n_cigar_ops" ~printer:int                        (List.length (ok_exn (cigar al1))) (List.length (ok_exn (cigar al2))) ;
  assert_equal ~msg:"seq"         ~printer:ident                      (Option.value_exn (seq al1)) (Option.value_exn (seq al2)) ;
  ()

let test_read () =
  Bam.with_file0 "etc/test_data/bam_01.bam" ~f:(fun header alignments ->
      let sh = Bam.Header.to_sam header in
      assert_equal ~msg:"Sam version" ~printer:(option string) (Some "1.0") sh.Sam.version ;
      assert_equal ~msg:"Sort order" (Some `Unsorted) sh.Sam.sort_order ;
      assert_equal ~msg:"Number of ref sequences" ~printer:string_of_int 22 (List.length sh.Sam.ref_seqs) ;
      let al0 = Stream.next_exn alignments |> ok_exn in
      assert_alignment ~qname:(Some "ILLUMINA-D118D2_0040_FC:7:20:2683:16044#0/1") ~rname:(Some "chr1") ~mapq:None ~n_cigar_ops:1 ~seq:"TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT" header al0 ;

      let remaining_alignments = Stream.to_list alignments in
      assert_equal ~msg:"Number of alignments" ~printer:string_of_int 25 (List.length remaining_alignments) ;
      Ok ()
    )
  |> ok_exn

let test_read_write_and_read () =
  let bamfile = "etc/test_data/bam_01.bam" in
  Utils.with_temp_file "biocaml" ".bam" ~f:(fun fn ->
      Bam.with_file0 bamfile ~f:(fun header alignments ->
          Out_channel.with_file fn ~f:(Bam.write0 header (Stream.map alignments ~f:ok_exn)) ;
          Ok ()
        ) |> ok_exn ;
      Bam.with_file0 bamfile ~f:(fun ref_header ref_alignments ->
          Bam.with_file0 fn ~f:(fun header alignments ->
              try
                Stream.Result.map2_exn' ref_alignments alignments ~f:(assert_alignments ref_header)
                |> Stream.Result.fold' ~init:() ~f:(fun () () -> ())
              with Stream.Expected_streams_of_equal_length -> assert_failure "Original and written files don't have the same number of alignments"
            )
        )
    )
  |> ok_exn

let tests = "Bam" >::: [
  "Read Samtools generated BAM" >:: test_read ;
  "Read, write and re-read a Samtools generated BAM" >:: test_read_write_and_read
]
