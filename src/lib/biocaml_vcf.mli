open Biocaml_internal_pervasives

type vcf_id = string
type vcf_description = string
type vcf_number =
  | Number of int
  | OnePerAllele
  | OnePerGenotype
  | Unknown

type vcf_format_type = [ `integer_value
                       | `float_value
                       | `character_value
                       | `string_value
                       ]

type vcf_info_type = [ vcf_format_type | `flag_value ]

type vcf_info_meta = Info of vcf_number * vcf_info_type * vcf_description
type vcf_filter_meta = Filter of vcf_description
type vcf_format_meta = Format of vcf_number * vcf_format_type * vcf_description
type vcf_alt_meta = Alt of vcf_description

type vcf_meta = {
  vcfm_version   : string;
  vcfm_id_cache  : vcf_id Set.Poly.t;
  vcfm_info      : (vcf_id, vcf_info_meta) Hashtbl.t;
  vcfm_filters   : (vcf_id * vcf_filter_meta) list;
  vcfm_format    : (vcf_id, vcf_format_meta) Hashtbl.t;
  vcfm_alt       : (string, vcf_alt_meta) Hashtbl.t;
  vcfm_arbitrary : (string, string) Hashtbl.t;
  vcfm_header    : string list;
  vcfm_samples   : string list
}

type vcf_format = [ `integer of int
                  | `float of float
                  | `character of char
                  | `string of string
                  | `missing
                  ]
type vcf_info = [ vcf_format | `flag of string ]

type vcf_row = {
  vcfr_chrom   : string; (* FIXME(superbobry): Biocaml_chr.t *)
  vcfr_pos     : int;
  vcfr_ids     : string list;
  vcfr_ref     : string;
  vcfr_alts    : string list;
  vcfr_qual    : float option;
  vcfr_filter  : vcf_id list;
  vcfr_info    : (vcf_id, vcf_info list) Hashtbl.t;
  vcfr_samples : (vcf_id, (vcf_id * vcf_format list) list) Hashtbl.t
}

type item = vcf_row

module Pos : module type of Biocaml_pos

type vcf_parse_row_error =
  [ `invalid_int of string
  | `invalid_float of string
  | `info_type_coersion_failure of vcf_info_type * string
  | `format_type_coersion_failure of vcf_format_type * string
  | `invalid_dna of string
  | `unknown_info of vcf_id
  | `unknown_filter of vcf_id
  | `unknown_alt of string
  | `duplicate_ids of vcf_id list
  | `invalid_arguments_length of vcf_id * int * int
  | `invalid_row_length of int * int
  | `malformed_sample of string
  | `unknown_format of vcf_id
  ]

type vcf_parse_error =
  [ `malformed_meta of Pos.t * string
  | `malformed_row of Pos.t * vcf_parse_row_error * string
  | `malformed_header of Pos.t * string
  | `incomplete_input of Pos.t * string list * string option
  | `not_ready
  ]

val parse_error_to_string : vcf_parse_error -> string

module Transform : sig
  val string_to_item :
    ?filename:string ->
    unit ->
    (string, (item, vcf_parse_error) Core.Result.t) Biocaml_transform.t
end
