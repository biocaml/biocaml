open CFStream
module Bam = Biocaml_unix.Bam
module Sam = Biocaml_unix.Sam
open OUnit

let assert_equal ?msg ?printer ?cmp x y =
  let printer = Option.map printer ~f:(fun p x -> Sexp.to_string_hum (p x)) in
  assert_equal ?msg ?cmp ?printer x y
;;

let assert_headers h1 h2 =
  let h1, h2 = Bam.Header.to_sam h1, Bam.Header.to_sam h2 in
  assert_equal
    ~msg:"version"
    ~printer:[%sexp_of: string option]
    h1.Sam.version
    h2.Sam.version;
  assert_equal
    ~msg:"sort_order"
    ~printer:[%sexp_of: Sam.sort_order option]
    h1.Sam.sort_order
    h2.Sam.sort_order;
  assert_equal
    ~msg:"ref_seqs"
    ~printer:[%sexp_of: Sam.ref_seq list]
    h1.Sam.ref_seqs
    h2.Sam.ref_seqs;
  assert_equal
    ~msg:"read_groups"
    ~printer:[%sexp_of: Sam.read_group list]
    h1.Sam.read_groups
    h2.Sam.read_groups;
  assert_equal
    ~msg:"programs"
    ~printer:[%sexp_of: Sam.program list]
    h1.Sam.programs
    h2.Sam.programs;
  assert_equal
    ~msg:"comments"
    ~printer:[%sexp_of: string list]
    h1.Sam.comments
    h2.Sam.comments;
  assert_equal
    ~msg:"others"
    ~printer:[%sexp_of: (string * Sam.tag_value list) list]
    h1.Sam.others
    h2.Sam.others;
  ()
;;

let assert_alignment ~qname ~rname ~mapq ~n_cigar_ops ~seq header al =
  assert_equal
    ~msg:"wrong rname"
    ~printer:[%sexp_of: string option]
    rname
    (Bam.Alignment0.rname al header |> ok_exn);
  assert_equal
    ~msg:"wrong mapq"
    ~printer:[%sexp_of: int option]
    mapq
    (Bam.Alignment0.mapq al);
  assert_equal
    ~msg:"wrong tlen"
    ~printer:[%sexp_of: int option]
    None
    (Bam.Alignment0.tlen al);
  assert_equal
    ~msg:"wrong read_name"
    ~printer:[%sexp_of: string option]
    qname
    (Bam.Alignment0.qname al);
  assert_equal
    ~msg:"wrong n_cigar_ops"
    ~printer:Int.sexp_of_t
    n_cigar_ops
    (List.length (ok_exn (Bam.Alignment0.cigar al)));
  assert_equal
    ~msg:"wrong seq"
    ~printer:String.sexp_of_t
    seq
    (Option.value_exn (Bam.Alignment0.seq al));
  ()
;;

let assert_alignments header al1 al2 =
  let open Bam.Alignment0 in
  assert_equal
    ~msg:"rname"
    ~printer:[%sexp_of: string option Or_error.t]
    (rname al1 header)
    (rname al2 header);
  assert_equal ~msg:"mapq" ~printer:[%sexp_of: int option] (mapq al1) (mapq al2);
  assert_equal ~msg:"tlen" ~printer:[%sexp_of: int option] (tlen al1) (tlen al2);
  assert_equal ~msg:"read_name" ~printer:[%sexp_of: string option] (qname al1) (qname al2);
  assert_equal
    ~msg:"n_cigar_ops"
    ~printer:Int.sexp_of_t
    (List.length (ok_exn (cigar al1)))
    (List.length (ok_exn (cigar al2)));
  assert_equal
    ~msg:"seq"
    ~printer:String.sexp_of_t
    (Option.value_exn (seq al1))
    (Option.value_exn (seq al2));
  ()
;;

let data1 =
  { Inline_file.content =
      "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"
  ; prefix = "bam_01"
  ; suffix = ".bam"
  }
;;

let test_read () =
  let { Inline_file.content; prefix; suffix } = data1 in
  Utils.with_temp_file prefix suffix ~f:(fun bam_file ->
    let data = Base64.decode_exn content in
    Out_channel.write_all bam_file ~data;
    Bam.with_file0 bam_file ~f:(fun header alignments ->
      let sh = Bam.Header.to_sam header in
      assert_equal
        ~msg:"Sam version"
        ~printer:[%sexp_of: string option]
        (Some "1.0")
        sh.Sam.version;
      assert_equal ~msg:"Sort order" (Some `Unsorted) sh.Sam.sort_order;
      assert_equal
        ~msg:"Number of ref sequences"
        ~printer:Int.sexp_of_t
        22
        (List.length sh.Sam.ref_seqs);
      let al0 = Stream.next_exn alignments |> ok_exn in
      assert_alignment
        ~qname:(Some "ILLUMINA-D118D2_0040_FC:7:20:2683:16044#0/1")
        ~rname:(Some "chr1")
        ~mapq:None
        ~n_cigar_ops:1
        ~seq:"TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT"
        header
        al0;
      let remaining_alignments = Stream.to_list alignments in
      assert_equal
        ~msg:"Number of alignments"
        ~printer:Int.sexp_of_t
        25
        (List.length remaining_alignments);
      Ok ())
    |> ok_exn)
;;

let test_read_write_and_read () =
  let { Inline_file.content; prefix; suffix } = data1 in
  Utils.with_temp_file prefix suffix ~f:(fun bamfile ->
    let data = Base64.decode_exn content in
    Out_channel.write_all bamfile ~data;
    Utils.with_temp_file "biocaml" ".bam" ~f:(fun fn ->
      Bam.with_file0 bamfile ~f:(fun header alignments ->
        Out_channel.with_file fn ~f:(Bam.write0 header (Stream.map alignments ~f:ok_exn));
        Ok ())
      |> ok_exn;
      Bam.with_file0 bamfile ~f:(fun ref_header ref_alignments ->
        Bam.with_file0 fn ~f:(fun header alignments ->
          assert_headers ref_header header;
          try
            Stream.Result.map2_exn'
              ref_alignments
              alignments
              ~f:(assert_alignments ref_header)
            |> Stream.Result.fold' ~init:() ~f:(fun () () -> ())
          with
          | Stream.Expected_streams_of_equal_length ->
            assert_failure
              "Original and written files don't have the same number of alignments")))
    |> ok_exn)
;;

let tests =
  "Bam"
  >::: [ "Read Samtools generated BAM" >:: test_read
       ; "Read, write and re-read a Samtools generated BAM" >:: test_read_write_and_read
       ]
;;
