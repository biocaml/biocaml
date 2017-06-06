open Core_kernel
open CFStream
module Tfxm = Biocaml_unix.Tfxm
module Vcf = Biocaml_unix.Vcf
open OUnit

let make_stream name =
  let (/) = Filename.concat in
  let filename = "etc"/"test_data"/name in
  let t  = Vcf.Transform.string_to_item ~filename () in
  let ic = In_channel.create filename in
  Tfxm.in_channel_strings_to_stream ~buffer_size:10 ic t

let compare_rows r1 r2 =
  let open Vcf in
  r1.vcfr_chrom = r2.vcfr_chrom &&
  r1.vcfr_pos   = r2.vcfr_pos &&
  r1.vcfr_ids   = r2.vcfr_ids &&
  r1.vcfr_ref   = r2.vcfr_ref &&
  r1.vcfr_alts  = r2.vcfr_alts &&
  r1.vcfr_qual  = r2.vcfr_qual &&
  r1.vcfr_filter = r2.vcfr_filter &&
  Hashtbl.equal r1.vcfr_info r2.vcfr_info (=) &&
  Hashtbl.equal r1.vcfr_samples r2.vcfr_samples (=)

let make_row ~chrom ~pos ~ids ~ref ~alts ~qual ~filter ~info ~samples =
    let open Vcf in
    let vcfr_info    = Hashtbl.Poly.of_alist_exn info in
    let vcfr_samples = Hashtbl.Poly.of_alist_exn samples in
    {
      vcfr_chrom = chrom;
      vcfr_pos   = pos;
      vcfr_ids   = ids;
      vcfr_ref   = ref;
      vcfr_alts  = alts;
      vcfr_qual  = qual;
      vcfr_filter = filter;
      vcfr_info; vcfr_samples
    }

let test_parse_vcf_generic filename rows =
  let s = make_stream filename in
  List.iter rows ~f:(fun row ->
    match Stream.next s with
      | Some (Ok actual_row) ->
        assert_equal ~cmp:compare_rows row actual_row
      | Some (Error err) ->
        let msg = Vcf.parse_error_to_string err in
        assert_bool
          (sprintf "%s:row *not* parsed, reason: %s" filename  msg)
          false
      | None -> assert_bool (sprintf "%s:row missing" filename) false)

let test_parse_vcf_header () =
  let s = make_stream "vcf_01_header_only.vcf" in
  match Stream.next s with
  | None -> ()  (* No rows to return. *)
  | Some (Ok _) -> assert false  (* Impossible. *)
  | Some (Error err) ->
    let msg = Vcf.parse_error_to_string err in
    assert_bool (Printf.sprintf "test_parse_vcf_header, reason: %s" msg) false

let test_parse_vcf_simple () =
  test_parse_vcf_generic "vcf_02_simple.vcf" [
    make_row ~chrom:"20" ~pos:14370 ~ids:["rs6054257"]
      ~ref:"G" ~alts:["A"]
      ~qual:(Some 29.0) ~filter:[]
      ~info:[("NS", [`integer 3]);
             ("DP", [`integer 14]);
             ("AF", [`float 0.5]);
             ("DB", [`flag "DB"]);
             ("H2", [`flag "H2"])]
      ~samples:[]
  ]

let test_parse_vcf_1000g () =
  test_parse_vcf_generic "vcf_03_1000g.vcf"  [
    make_row ~chrom:"20" ~pos:17330 ~ids:[]
      ~ref:"T" ~alts:["A"]
      ~qual:None ~filter:["q10"]
      ~info:[("NS", [`integer 3]);
             ("DP", [`integer 11]);
             ("AF", [`float 0.017])]
      ~samples:[];
    make_row ~chrom:"20" ~pos:1110696 ~ids:["rs6040355"]
      ~ref:"A" ~alts:["G"; "T"]
      ~qual:(Some 67.0) ~filter:[]
      ~info:[("NS", [`integer 2]);
             ("DP", [`integer 10]);
             ("AF", [`float 0.333; `float 0.667]);
             ("AA", [`string "T"]);
             ("DB", [`flag "DB"])]
      ~samples:[];
    make_row ~chrom:"20" ~pos:1230237 ~ids:[]
      ~ref:"T" ~alts:[]
      ~qual:(Some 47.0) ~filter:[]
      ~info:[("NS", [`integer 3]);
             ("DP", [`integer 13]);
             ("AA", [`string "T"])]
      ~samples:[];
    make_row ~chrom:"20" ~pos:1234567 ~ids:["microsat1"]
      ~ref:"GTC" ~alts:["G"; "GTCT"]
      ~qual:(Some 50.0) ~filter:[]
      ~info:[("NS", [`integer 3]);
             ("DP", [`integer 9]);
             ("AA", [`string "G"])]
      ~samples:[]
  ]

let test_parse_vcf_reserved () =
  test_parse_vcf_generic "vcf_04_reserved.vcf" [
    make_row ~chrom:"20" ~pos:14370 ~ids:["rs6054257"]
      ~ref:"G" ~alts:["A"]
      ~qual:(Some 29.0) ~filter:[]
      ~info:[("NS", [`integer 3]);
             ("DP", [`integer 14]);
             ("AF", [`float 0.5]);
             ("DB", [`flag "DB"]);
             ("H2", [`flag "H2"])]
      ~samples:[]
  ]

let test_parse_vcf_alt () =
  test_parse_vcf_generic "vcf_05_alt.vcf" [
    make_row ~chrom:"2" ~pos:321682 ~ids:[]
      ~ref:"T" ~alts:["<DEL>"]
      ~qual:(Some 6.0) ~filter:[]
      ~info:[("IMPRECISE", [`flag "IMPRECISE"]);
             ("SVTYPE", [`string "DEL"]);
             ("END", [`integer 321887]);
             ("SVLEN", [`integer (-105)]);
             ("CIPOS", [`integer (-56); `integer 20]);
             ("CIEND", [`integer (-10); `integer 62])]
      ~samples:[];
    make_row ~chrom:"2" ~pos:14477084 ~ids:[]
      ~ref:"C" ~alts:["<DEL:ME:ALU>"]
      ~qual:(Some 12.0) ~filter:[]
      ~info:[("IMPRECISE", [`flag "IMPRECISE"]);
             ("SVTYPE", [`string "DEL"]);
             ("END", [`integer 14477381]);
             ("SVLEN", [`integer (-297)]);
             ("MEINFO", [`string "AluYa5"; `string "5"; `string "307"; `string "+"]);
             ("CIPOS", [`integer (-22); `integer 18]);
             ("CIEND", [`integer (-12); `integer 32])]
      ~samples:[]
  ]

let test_parse_vcf_samples () =
  test_parse_vcf_generic "vcf_06_samples.vcf" [
    make_row ~chrom:"20" ~pos:14370 ~ids:["rs6054257"]
      ~ref:"G" ~alts:["A"]
      ~qual:(Some 29.0) ~filter:[]
      ~info:[("NS", [`integer 3]);
             ("DP", [`integer 14]);
             ("AF", [`float 0.5]);
             ("DB", [`flag "DB"]);
             ("H2", [`flag "H2"])]
      ~samples:[("NA00001", [("GT", [`string "0|0"]);
                             ("GQ", [`integer 48]);
                             ("DP", [`integer 1]);
                             ("HQ", [`integer 51; `integer 51])]);
                ("NA00002", [("GT", [`string "1|0"]);
                             ("GQ", [`integer 48]);
                             ("DP", [`integer 8]);
                             ("HQ", [`integer 51; `integer 51])]);
                ("NA00003", [("GT", [`string "1/1"]);
                             ("GQ", [`integer 43]);
                             ("DP", [`integer 5]);
                             ("HQ", [`missing; `missing])])]
  ]


let tests = "VCF" >::: [
  "Parse VCF header" >:: test_parse_vcf_header;
  "Parse simple VCF (1 row)" >:: test_parse_vcf_simple;
  "Parse sample VCF from 1000g project" >:: test_parse_vcf_1000g;
  "Parse VCF missing INFO for reserved sub-fields" >:: test_parse_vcf_reserved;
  "Parse VCF with custom ALT" >:: test_parse_vcf_alt;
  "Parse VCF with multiple samples" >:: test_parse_vcf_samples
]
