(** Parsing and printing of BAM files. *)

(**
This module provides parsing from [stirng] buffers to [raw_item]s
(minimal parsing), and conversion from [raw_item]s to [Sam.item]s
(higher-level constructs).
*)


type raw_alignment = {
  qname : string;
  flag : int;
  ref_id: int;
  pos : int; (** 0-based, -1 if undefined*)
  mapq : int;
  bin: int;
  cigar : string;
  next_ref_id : int;
  pnext : int;
  tlen : int;
  seq : string;
  qual : int array;
  optional : string;
}
(** The type of an alignment record in a BAM file.
    The rather unmeaningful names come from the “almost official”
    specification of the format. *)

val raw_alignment_of_sexp : Sexplib.Sexp.t -> raw_alignment
val sexp_of_raw_alignment : raw_alignment -> Sexplib.Sexp.t

type raw_item =
[ `alignment of raw_alignment
| `header of string
| `reference_information of (string * int) array ]
(** The different kinds of items one can get from parsing a BAM file. *)

val raw_item_of_sexp : Sexplib.Sexp.t -> raw_item
val sexp_of_raw_item : raw_item -> Sexplib.Sexp.t

module Transform: sig
  (** The low-level [Transform.t] implementations. *)

  type raw_bam_error = [
  | `read_name_not_null_terminated of string
  | `reference_information_name_not_null_terminated of string
  | `reference_information_overflow of int * string
  | `wrong_magic_number of string
  | `wrong_int32 of string
  ]
  (** The possible non-gzip-related errors encountered while parsing a
      BAM file. *)

  val raw_bam_error_of_sexp : Sexplib.Sexp.t -> raw_bam_error
  val sexp_of_raw_bam_error : raw_bam_error -> Sexplib.Sexp.t

  val string_to_raw:
    ?zlib_buffer_size:int ->
    unit ->
    (string,
     (raw_item, [> `unzip of Biocaml_zip.Transform.unzip_error
                | `bam of raw_bam_error ] )
       Core.Result.t)
      Biocaml_transform.t
  (** Create a transform that parses a BAM file.
      The [zlib_buffer_size] is passed to the [Biocaml_zip] module. *)

  type parse_optional_error = [
  | `wrong_auxiliary_data of
      [ `array_size of int
      | `null_terminated_hexarray
      | `null_terminated_string
      | `out_of_bounds
      | `wrong_int32 of string
      | `unknown_type of char ] * string
  ]
  (** The potential failures while parsing the optional content in BAM
      alignments. *)

  val parse_optional_error_of_sexp : Sexplib.Sexp.t -> parse_optional_error
  val sexp_of_parse_optional_error : parse_optional_error -> Sexplib.Sexp.t

  val parse_optional: ?pos:int -> ?len:int -> string ->
    (Biocaml_sam.optional_content, [> parse_optional_error]) Core.Result.t
  (** Parse optional content from a string (lowest-level function). *)

  type parse_cigar_error = [
  | `wrong_cigar of string
  | `wrong_cigar_length of int ]
  (** The potential failures while parsing the so-called CIGAR operations *)

  val parse_cigar_error_of_sexp : Sexplib.Sexp.t -> parse_cigar_error
  val sexp_of_parse_cigar_error : parse_cigar_error -> Sexplib.Sexp.t


  val parse_cigar: ?pos:int -> ?len:int -> string ->
    (Biocaml_sam.cigar_op array, [> parse_cigar_error]) Core.Result.t
  (** Parse CIGAR operations from a string (lowest-level function). *)

  type raw_to_item_error = [
  | `header_line_not_first of int
  | `header_line_without_version of (string * string) list
  | `header_line_wrong_sorting of string
  | `invalid_header_tag of int * string
  | `invalid_tag_value_list of int * string list
  | `reference_sequence_not_found of raw_alignment
  | parse_optional_error
  | parse_cigar_error
  | `wrong_flag of raw_alignment
  | `wrong_mapq of raw_alignment
  | `wrong_pnext of raw_alignment
  | `wrong_pos of raw_alignment
  | `wrong_qname of raw_alignment
  | `wrong_tlen of raw_alignment ]
  (** All the possible errors one encounter while going from [raw_item]s to
      [Sam.item]s. *)

  val raw_to_item_error_of_sexp : Sexplib.Sexp.t -> raw_to_item_error
  val sexp_of_raw_to_item_error : raw_to_item_error -> Sexplib.Sexp.t

  val raw_to_item: unit ->
    (raw_item, (Biocaml_sam.item, [> raw_to_item_error]) Core.Result.t)
      Biocaml_transform.t
  (** Create a transform that lifts [raw_item]s to the higher-level representation
      defined in the [Biocaml_sam] module. *)

  type item_to_raw_error =
  [ `cannot_get_sequence of Biocaml_sam.alignment
  | `header_line_not_first of string
  | `reference_name_not_found of Biocaml_sam.alignment * string ]
  (** Inconsistency errors that may happen while trnasforming a
      [Sam.item] to a [raw_item]. *)

  val item_to_raw_error_of_sexp : Sexplib.Sexp.t -> item_to_raw_error
  val sexp_of_item_to_raw_error : item_to_raw_error -> Sexplib.Sexp.t

  val item_to_raw: unit ->
    (Biocaml_sam.item,
     (raw_item, [> item_to_raw_error]) Core.Result.t) Biocaml_transform.t
  (** Create a transform that downgrades [Sam.item]s to [raw_item]s. *)

  val raw_to_string: ?zlib_buffer_size:int -> unit ->
    (raw_item, string) Biocaml_transform.t
  (** Create a transform that “prints” [raw_item]s in the BAM format.
      The [zlib_buffer_size] option is passed to the [Biocaml_zip] module. *)

end

