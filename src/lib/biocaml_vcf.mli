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

type vcf_meta =
  | Version of string
  | Info of vcf_id * vcf_number * vcf_info_type * vcf_description
  | Filter of vcf_id * vcf_description
  | Format of vcf_id * vcf_number * vcf_format_type * vcf_description
  | Alt of vcf_alt_type * vcf_alt_subtype list * vcf_description
  | Arbitrary of string * string

type vcf_row = {
  chrom : int;
  pos   : int;
  id    : string list;
  ref   : string list;
  alt   : string list;
  qual  : float option;
  filter : vcf_id list;
  info  : (string, string) Hashtbl.t  (* FIXME: proper typing *)
}

type t = [`meta of vcf_meta | `row of vcf_row]

module Pos : module type of Biocaml_pos

type vcf_parse_error =
  [ `empty_line of Pos.t
  | `malformed_meta of Pos.t * string
  | `incomplete_input of Pos.t * string list * string option
  ]

module Transform : sig
  val string_to_t :
    ?filename:string ->
    unit ->
    (string, (t, vcf_parse_error) Core.Result.t) Biocaml_transform.t
end
