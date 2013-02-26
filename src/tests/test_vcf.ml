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
    let vcfr_info = Hashtbl.Poly.create () in
    List.iter ~f:(fun (k, v) -> Hashtbl.set vcfr_info k v) info;
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
  assert_bool "test_parse_vcf_header" begin
    match Stream.next s with
    | Some (Ok _) -> true
    | Some (Error err) -> false
    | None -> true  (** No rows to return. **)
  end

let test_parse_vcf_simple () =
  let s = make_stream "vcf_02_simple.vcf" in
  match Stream.next s with
  | Some (Ok row) ->
    assert_equal ~cmp:compare_rows row
      (make_row ~chrom:"20" ~pos:14370 ~ids:["rs6054257"]
         ~ref:"G" ~alts:["A"]
         ~qual:(Some 29.0) ~filter:[]
         ~info:[("NS", [`integer 3]);
                ("DP", [`integer 14]);
                ("AF", [`float 0.5]);
                ("DB", [`flag "DB"]);
                ("H2", [`flag "H2"])])
  | _ -> assert_bool "test_parse_vcf_simple:row *not* parsed" false

let tests = "VCF" >::: [
  "Parse VCF header" >:: test_parse_vcf_header;
  "Parse simple VCF (1 row)" >:: test_parse_vcf_simple;
]
