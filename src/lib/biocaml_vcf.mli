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

type vcf_alt_type =
  | Deletion
  | Insertion
  | Duplication
  | Inversion
  | CNV

type vcf_alt_subtype = string

type vcf_info_meta = Info of vcf_number * vcf_info_type * vcf_description
type vcf_filter_meta = Filter of vcf_description
type vcf_format_meta =
  Format of vcf_number * vcf_format_type * vcf_description
type vcf_alt_meta =
  Alt of vcf_alt_type * vcf_alt_subtype list * vcf_description

type vcf_meta = {
  vcfm_version : string;
  vcfm_id_cache: vcf_id Set.Poly.t;
  vcfm_info    : (vcf_id, vcf_info_meta) Hashtbl.t;
  vcfm_filters : (vcf_id * vcf_filter_meta) list;
  vcfm_format  : (vcf_id, vcf_format_meta) Hashtbl.t;
  vcfm_alt     : vcf_alt_meta list;
  vcfm_arbitrary : (string, string) Hashtbl.t;
  vcfm_header  : string list
}


type vcf_format = [ `integer of int
                  | `float of float
                  | `character of char
                  | `string of string
                  ]
type vcf_info = [ vcf_format | `flag of string ]

type vcf_row = {
  vcfr_chrom : string; (* FIXME(superbobry): Biocaml_chrName.t *)
  vcfr_pos   : int;
  vcfr_ids   : string list;
  vcfr_ref   : string;
  vcfr_alts  : string list;
  vcfr_qual  : float option;
  vcfr_filter : vcf_id list;
  vcfr_info  : (vcf_id, vcf_info list) Hashtbl.t
}

type item = vcf_row

module Pos : module type of Biocaml_pos

type vcf_parse_row_error =
  [ `invalid_int of string
  | `invalid_float of string
  | `info_type_coersion_failure of vcf_info_type * string
  | `invalid_dna of string
  | `unknown_info of vcf_id
  | `unknown_filter of vcf_id
  | `duplicate_ids of vcf_id list
  | `invalid_arguments_length of vcf_id * int * int
  | `arbitrary_width_rows_not_supported
  ]

type vcf_parse_error =
  [ `malformed_meta of Pos.t * string
  | `malformed_row of Pos.t * vcf_parse_row_error * string
  | `malformed_header of Pos.t * string
  | `alt_parsing_not_implemented of Pos.t
  | `arbitrary_width_rows_not_supported of Pos.t
  | `incomplete_input of Pos.t * Biocaml_lines.item list * string option
  | `not_ready
  ]

val parse_error_to_string : vcf_parse_error -> string

module Transform : sig
  val string_to_item :
    ?filename:string ->
    unit ->
    (string, (item, vcf_parse_error) Core.Result.t) Biocaml_transform.t
end
