open Biocaml_internal_pervasives

type vcf_id = string
type vcf_description = string
type vcf_number =
  | Number of int
  | OnePerAllele
  | OnePerGenotype
  | Unknown

type vcf_format_type = [ `integer
                       | `float
                       | `character
                       | `string
                       ]

type vcf_info_type = [ vcf_format_type | `flag ]

type vcf_alt_type =
  | Deletion
  | Insertion
  | Duplication
  | Inversion
  | CNV

type vcf_alt_subtype = string

type vcf_info_meta =
  Info of vcf_id * vcf_number * vcf_info_type * vcf_description
type vcf_filter_meta =
  Filter of vcf_id * vcf_description
type vcf_format_meta =
  Format of vcf_id * vcf_number * vcf_format_type * vcf_description
type vcf_alt_meta =
  Alt of vcf_alt_type * vcf_alt_subtype list * vcf_description

type vcf_meta = {
  vcfm_version : string;
  vcfm_info    : vcf_info_meta list;
  vcfm_filter  : vcf_filter_meta list;
  vcfm_format  : vcf_format_meta list;
  vcfm_alt     : vcf_alt_meta list;
  vcfm_arbitrary : (string, string) Hashtbl.t;
  vcfm_header  : string list
}

type vcf_row = {
  vcfr_chrom : string;
  vcfr_pos   : int;
  vcfr_id    : string list;
  vcfr_ref   : string;
  vcfr_alt   : string list;
  vcfr_qual  : float option;
  vcfr_filter : vcf_id list;
  vcfr_info  : (string, string) Hashtbl.t  (* FIXME: proper typing *)
}

type item = vcf_row

module Pos : module type of Biocaml_pos

type vcf_parse_error =
  [ `malformed_meta of Pos.t * string
  | `malformed_row of Pos.t * string
  | `missing_header of Pos.t
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