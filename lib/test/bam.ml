open Core_kernel
open CFStream
module Bam = Biocaml_unix.Bam
module Sam = Biocaml_unix.Sam
open OUnit
open Utils.Printer

let assert_equal ?msg ?printer ?cmp x y =
  let printer = Option.map printer ~f:(fun p x -> Sexp.to_string_hum (p x)) in
  assert_equal ?msg ?cmp ?printer x y

let assert_headers h1 h2 =
  let h1, h2 = Bam.Header.to_sam h1, Bam.Header.to_sam h2 in
  assert_equal ~msg:"version"     ~printer:[%sexp_of: string option]                      h1.Sam.version h2.Sam.version ;
  assert_equal ~msg:"sort_order"  ~printer:[%sexp_of: Sam.sort_order option]              h1.Sam.sort_order h2.Sam.sort_order ;
  assert_equal ~msg:"ref_seqs"    ~printer:[%sexp_of:  Sam.ref_seq list]                  h1.Sam.ref_seqs h2.Sam.ref_seqs ;
  assert_equal ~msg:"read_groups" ~printer:[%sexp_of: Sam.read_group list]                h1.Sam.read_groups h2.Sam.read_groups ;
  assert_equal ~msg:"programs"    ~printer:[%sexp_of: Sam.program list]                   h1.Sam.programs h2.Sam.programs ;
  assert_equal ~msg:"comments"    ~printer:[%sexp_of: string list]                        h1.Sam.comments h2.Sam.comments ;
  assert_equal ~msg:"others"      ~printer:[%sexp_of: (string * Sam.tag_value list) list] h1.Sam.others h2.Sam.others ;
  ()

let assert_alignment ~qname ~rname ~mapq ~n_cigar_ops ~seq header al =
  assert_equal ~msg:"wrong rname"       ~printer:[%sexp_of: string option] rname       (Bam.Alignment0.rname al header |> ok_exn) ;
  assert_equal ~msg:"wrong mapq"        ~printer:[%sexp_of: int option]    mapq        (Bam.Alignment0.mapq al) ;
  assert_equal ~msg:"wrong tlen"        ~printer:[%sexp_of: int option]    None        (Bam.Alignment0.tlen al) ;
  assert_equal ~msg:"wrong read_name"   ~printer:[%sexp_of: string option] qname       (Bam.Alignment0.qname al) ;
  assert_equal ~msg:"wrong n_cigar_ops" ~printer:Int.sexp_of_t             n_cigar_ops (List.length (ok_exn (Bam.Alignment0.cigar al))) ;
  assert_equal ~msg:"wrong seq"         ~printer:String.sexp_of_t          seq         (Option.value_exn (Bam.Alignment0.seq al)) ;
  ()

let assert_alignments header al1 al2 =
  let open Bam.Alignment0 in
  assert_equal ~msg:"rname"       ~printer:[%sexp_of: string option Or_error.t] (rname al1 header) (rname al2 header) ;
  assert_equal ~msg:"mapq"        ~printer:[%sexp_of: int option]               (mapq al1) (mapq al2) ;
  assert_equal ~msg:"tlen"        ~printer:[%sexp_of: int option]               (tlen al1) (tlen al2) ;
  assert_equal ~msg:"read_name"   ~printer:[%sexp_of: string option]            (qname al1) (qname al2) ;
  assert_equal ~msg:"n_cigar_ops" ~printer:Int.sexp_of_t                        (List.length (ok_exn (cigar al1))) (List.length (ok_exn (cigar al2))) ;
  assert_equal ~msg:"seq"         ~printer:String.sexp_of_t                     (Option.value_exn (seq al1)) (Option.value_exn (seq al2)) ;
  ()

let test_read () =
  Bam.with_file0 "etc/test_data/bam_01.bam" ~f:(fun header alignments ->
      let sh = Bam.Header.to_sam header in
      assert_equal ~msg:"Sam version" ~printer:[%sexp_of: string option] (Some "1.0") sh.Sam.version ;
      assert_equal ~msg:"Sort order" (Some `Unsorted) sh.Sam.sort_order ;
      assert_equal ~msg:"Number of ref sequences" ~printer:Int.sexp_of_t 22 (List.length sh.Sam.ref_seqs) ;
      let al0 = Stream.next_exn alignments |> ok_exn in
      assert_alignment ~qname:(Some "ILLUMINA-D118D2_0040_FC:7:20:2683:16044#0/1") ~rname:(Some "chr1") ~mapq:None ~n_cigar_ops:1 ~seq:"TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT" header al0 ;

      let remaining_alignments = Stream.to_list alignments in
      assert_equal ~msg:"Number of alignments" ~printer:Int.sexp_of_t 25 (List.length remaining_alignments) ;
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
              assert_headers ref_header header ;
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
