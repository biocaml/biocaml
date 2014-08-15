(**
    This is the biocaml_cosmic module of biocaml. It is meant to be
    used in conjuction with COSMICMart version 69. The file can be
    found at the following URL
    https://cancer.sanger.ac.uk/files/cosmic/v69/
    and is labeled as COSMIC_MART.csv.gz .
*)

open Core.Std

type item = {
  cosmic_mart_pk_key : int;
  cosmic_id_sample : int;
  cosmic_id_gene : int option;
  sample_name : string;
  gene_name : string;
  entrez_gene_id : int option;
  ensg_id : string;
  swissprot_id : string;
  accession_number : string;
  cosmic_id_mutation : string option;
  cosmic_id_fusion_mutation : string option;
  mut_type_cds : string option;
  cds_mut_syntax : string option;
  cds_mut_start : string option;
  cds_mut_stop : string option;
  mut_type_aa : string option;
  aa_mut_syntax : string option;
  aa_mut_start : string option;
  aa_mut_stop : string option;
  ncbi36_coords : string option;
  grch37_coords : string option;
  zygosity : string;
  in_cancer_census : string;
  samp_gene_mutated : string;
  somatic_status : string;
  validation_status : string;
  whole_genome_screen : string;
  site_primary : string;
  site_subtype_1 : string;
  site_subtype_2 : string;
  site_subtype_3 : string;
  hist_primary : string;
  hist_subtype_1 : string;
  hist_subtype_2 : string;
  hist_subtype_3 : string;
  sample_source : string;
  tumour_source : string;
  pubmed_pmid : int option;
  cosmic_id_study : int option}

type something_in_cosmic_file =
| Item of item
| Something of string
| Header of string
| Empty

val parse_line : int -> string -> something_in_cosmic_file
(** [parse_line _ y] parses a single line of a cosmic file. *)

val parse_file : string -> something_in_cosmic_file Sequence.t
(** [parse_file x] returns a something_in_cosmic_file sequence *)

val print_item : item -> unit
(** [print_item x] returns a unit and was used to visualize an item
    when necessary *)


