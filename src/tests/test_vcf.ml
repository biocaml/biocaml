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

let tests = "VCF" >::: [
  "Parse VCF header" >:: test_parse_vcf_header;
]
