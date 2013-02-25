open OUnit
open Biocaml_internal_pervasives
open Biocaml

let make_stream name =
  let open Filename.Infix in
  let filename = "src/tests/data" </> name in
  let t  = Vcf.Transform.string_to_item ~filename () in
  let ic = open_in filename in
  Transform.in_channel_strings_to_stream ~buffer_size:10 ic t

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
    let open Vcf in
    assert_equal row.vcfr_chrom "20" ~msg:"chromosome";
    assert_equal row.vcfr_pos 14370 ~msg:"position";
    assert_equal row.vcfr_id ["rs6054257"] ~msg:"id";
    assert_equal row.vcfr_ref "G" ~msg:"ref";
    assert_equal row.vcfr_alt ["A"] ~msg:"alt";
    assert_equal row.vcfr_qual (Some 29.0) ~msg:"quality";
    assert_equal row.vcfr_filter ["PASS"] ~msg:"filter"
  | _ -> assert_bool "test_parse_vcf_simple:row *not* parsed" false

let tests = "VCF" >::: [
  "Parse VCF header" >:: test_parse_vcf_header;
  "Parse simple VCF (1 row)" >:: test_parse_vcf_simple;
]
