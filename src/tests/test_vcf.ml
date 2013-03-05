open OUnit
open Biocaml_internal_pervasives
open Biocaml

let make_stream name =
  let open Filename.Infix in
  let filename = "src/tests/data" </> name in
  let t  = Vcf.Transform.string_to_item ~filename () in
  let ic = open_in filename in
  Transform.in_channel_strings_to_stream ~buffer_size:10 ic t

let compare_rows r1 r2 =
  let open Vcf in
  r1.vcfr_chrom = r2.vcfr_chrom &&
  r1.vcfr_pos   = r2.vcfr_pos &&
  r1.vcfr_ids   = r2.vcfr_ids &&
  r1.vcfr_ref   = r2.vcfr_ref &&
  r1.vcfr_alts  = r2.vcfr_alts &&
  r1.vcfr_qual  = r2.vcfr_qual &&
  r1.vcfr_filter = r2.vcfr_filter &&
  Hashtbl.equal r1.vcfr_info r2.vcfr_info (=)

let make_row ~chrom ~pos ~ids ~ref ~alts ~qual ~filter ~info =
    let open Vcf in
    let vcfr_info = Hashtbl.Poly.of_alist_exn info in
    {
      vcfr_chrom = chrom;
      vcfr_pos   = pos;
      vcfr_ids   = ids;
      vcfr_ref   = ref;
      vcfr_alts  = alts;
      vcfr_qual  = qual;
      vcfr_filter = filter;
      vcfr_info
    }

let test_parse_vcf_header () =
  let s = make_stream "vcf_01_header_only.vcf" in
  match Stream.next s with
  | None -> ()  (** No rows to return. *)
  | Some (Ok _) -> assert false  (* Impossible. *)
  | Some (Error err) ->
    let msg = Vcf.parse_error_to_string err in
    assert_bool (Printf.sprintf "test_parse_vcf_header, reason: %s" msg) false

let test_parse_vcf_simple () =
  let s = make_stream "vcf_02_simple.vcf" in
  let row = make_row ~chrom:"20" ~pos:14370 ~ids:["rs6054257"]
      ~ref:"G" ~alts:["A"]
      ~qual:(Some 29.0) ~filter:[]
      ~info:[("NS", [`integer 3]);
             ("DP", [`integer 14]);
             ("AF", [`float 0.5]);
             ("DB", [`flag "DB"]);
             ("H2", [`flag "H2"])]
  in match Stream.next s with
  | Some (Ok actual_row) -> assert_equal ~cmp:compare_rows actual_row row
  | Some (Error err) ->
    let msg = Vcf.parse_error_to_string err in
    assert_bool
      (Printf.sprintf "test_parse_vcf_simple:row *not* parsed, reason: %s" msg)
      false
  | None -> assert_bool "test_parse_vcf_simple:row missing" false

let test_parse_vcf_1000g () =
  let s = make_stream "vcf_03_1000g.vcf" in
  let rows = [
    make_row ~chrom:"20" ~pos:17330 ~ids:[]
      ~ref:"T" ~alts:["A"]
      ~qual:(Some 3.0) ~filter:["q10"]
      ~info:[("NS", [`integer 3]);
             ("DP", [`integer 11]);
             ("AF", [`float 0.017])];
    make_row ~chrom:"20" ~pos:1110696 ~ids:["rs6040355"]
      ~ref:"A" ~alts:["G"; "T"]
      ~qual:(Some 67.0) ~filter:[]
      ~info:[("NS", [`integer 2]);
             ("DP", [`integer 10]);
             ("AF", [`float 0.333; `float 0.667]);
             ("AA", [`string "T"]);
             ("DB", [`flag "DB"])];
    make_row ~chrom:"20" ~pos:1230237 ~ids:[]
      ~ref:"T" ~alts:[]
      ~qual:(Some 47.0) ~filter:[]
      ~info:[("NS", [`integer 3]);
             ("DP", [`integer 13]);
             ("AA", [`string "T"])];
    make_row ~chrom:"20" ~pos:1234567 ~ids:["microsat1"]
      ~ref:"GTC" ~alts:["G"; "GTCT"]
      ~qual:(Some 50.0) ~filter:[]
      ~info:[("NS", [`integer 3]);
             ("DP", [`integer 9]);
             ("AA", [`string "G"])]
  ] in List.iter rows ~f:(fun row ->
    match Stream.next s with
      | Some (Ok actual_row) ->
        assert_equal ~cmp:compare_rows row actual_row
      | Some (Error err) ->
        let msg = Vcf.parse_error_to_string err in
        assert_bool
          (Printf.sprintf "test_parse_vcf_1000g:row *not* parsed, reason: %s" msg)
          false
      | None -> assert_bool "test_parse_vcf_1000g:row missing" false)

let test_parse_vcf_reserved () =
  let s = make_stream "vcf_04_reserved.vcf" in
  let row = make_row ~chrom:"20" ~pos:14370 ~ids:["rs6054257"]
      ~ref:"G" ~alts:["A"]
      ~qual:(Some 29.0) ~filter:[]
      ~info:[("NS", [`integer 3]);
             ("DP", [`integer 14]);
             ("AF", [`float 0.5]);
             ("DB", [`flag "DB"]);
             ("H2", [`flag "H2"])]
  in match Stream.next s with
  | Some (Ok actual_row) -> assert_equal ~cmp:compare_rows actual_row row
  | Some (Error err) ->
    let msg = Vcf.parse_error_to_string err in
    assert_bool
      (Printf.sprintf "test_parse_vcf_reserved:row *not* parsed, reason: %s" msg)
      false
  | None -> assert_bool "test_parse_vcf_reserved:row missing" false

let test_parse_vcf_alt () =
  let s = make_stream "vcf_05_alt.vcf" in
  let rows = [
    make_row ~chrom:"2" ~pos:321682 ~ids:[]
      ~ref:"T" ~alts:["<DEL>"]
      ~qual:(Some 6.0) ~filter:[]
      ~info:[("IMPRECISE", [`flag "IMPRECISE"]);
             ("SVTYPE", [`string "DEL"]);
             ("END", [`integer 321887]);
             ("SVLEN", [`integer (-105)]);
             ("CIPOS", [`integer (-56); `integer 20]);
             ("CIEND", [`integer (-10); `integer 62])];
  ] in List.iter rows ~f:(fun row ->
    match Stream.next s with
      | Some (Ok actual_row) ->
        assert_equal ~cmp:compare_rows row actual_row
      | Some (Error err) ->
        let msg = Vcf.parse_error_to_string err in
        assert_bool
          (Printf.sprintf "test_parse_vcf_alt:row *not* parsed, reason: %s" msg)
          false
      | None -> assert_bool "test_parse_vcf_alt:row missing" false)

let tests = "VCF" >::: [
  "Parse VCF header" >:: test_parse_vcf_header;
  "Parse simple VCF (1 row)" >:: test_parse_vcf_simple;
  "Parse sample VCF from 1000g project" >:: test_parse_vcf_1000g;
  "Parse VCF missing INFO for reserved sub-fields" >:: test_parse_vcf_reserved;
  "Parse VCF with custom ALT" >:: test_parse_vcf_alt;
]
