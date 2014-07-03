(** Parsing and printing of BAM files. *)

(**
This module provides parsing from [string] buffers to [raw_item]s
(minimal parsing), and conversion from [raw_item]s to [Sam.item]s
(higher-level constructs).
*)
open Core.Std

(** {2 Item Types} *)

type raw_alignment = {
  qname : string;
  flag : int;
  ref_id: int;
  pos : int; (** Positions are 0-based, -1 if undefined*)
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

    The rather meaningless names come from the “almost official”
    specification of the format (c.f.
    {{:http://samtools.sourceforge.net/SAM1.pdf}SAM1.pdf}). *)

type raw_item =
[ `alignment of raw_alignment
| `header of string
| `reference_information of (string * int) array ]
(** The different kinds of items one can get from parsing a BAM file. *)

(** {2 Error Types} *)

module Error: sig
  (** The possible errors returned by parsing and printing functions. *)

  type raw_bam = [
  | `read_name_not_null_terminated of string
  | `reference_information_name_not_null_terminated of string
  | `reference_information_overflow of int * string
  | `wrong_magic_number of string
  | `wrong_int32 of string
  ]
  (** The possible non-gzip-related errors encountered while parsing a
      BAM file. *)

  type parse_optional = [
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

  type parse_cigar = [
  | `wrong_cigar of string
  | `wrong_cigar_length of int ]
  (** The potential failures while parsing the so-called CIGAR operations *)

  type raw_to_item = [
  | `header_line_not_first of int
  | `header_line_without_version of (string * string) list
  | `header_line_wrong_sorting of string
  | `invalid_header_tag of int * string
  | `invalid_tag_value_list of int * string list
  | `reference_sequence_not_found of raw_alignment
  | parse_optional
  | parse_cigar
  | `wrong_flag of raw_alignment
  | `wrong_mapq of raw_alignment
  | `wrong_pnext of raw_alignment
  | `wrong_pos of raw_alignment
  | `wrong_qname of raw_alignment
  | `wrong_tlen of raw_alignment ]
  (** All the possible errors one can encounter while going from [raw_item]s to
      [Sam.item]s. *)

  type item_to_raw =
  [ `cannot_get_sequence of Biocaml_sam.alignment
  | `header_item_not_first of string
  | `reference_name_not_found of Biocaml_sam.alignment * string ]
  (** Inconsistency errors that may happen while trnasforming a
      [Sam.item] to a [raw_item]. *)

  type t = [ raw_bam | raw_to_item | item_to_raw ]
  (** The union of all the errors. *)

  val raw_bam_of_sexp : Sexplib.Sexp.t -> raw_bam
  val sexp_of_raw_bam : raw_bam -> Sexplib.Sexp.t

  val parse_optional_of_sexp : Sexplib.Sexp.t -> parse_optional
  val sexp_of_parse_optional : parse_optional -> Sexplib.Sexp.t
  val parse_cigar_of_sexp : Sexplib.Sexp.t -> parse_cigar
  val sexp_of_parse_cigar : parse_cigar -> Sexplib.Sexp.t
  val raw_to_item_of_sexp : Sexplib.Sexp.t -> raw_to_item
  val sexp_of_raw_to_item : raw_to_item -> Sexplib.Sexp.t

  val item_to_raw_of_sexp : Sexplib.Sexp.t -> item_to_raw
  val sexp_of_item_to_raw : item_to_raw -> Sexplib.Sexp.t

  val t_of_sexp: Sexplib.Sexp.t -> t
  val sexp_of_t: t -> Sexplib.Sexp.t

end

(** {2 [In_channel] Functions} *)

exception Error of [ `bam of Error.t
                   | `unzip of Biocaml_zip.Error.unzip ]
(** The exception thrown by [*_exn] functions in this module. *)

val in_channel_to_raw_item_stream :
  ?zlib_buffer_size:int ->
  ?buffer_size:int ->
  in_channel ->
  (raw_item,
   [> `bam of [> Error.raw_bam ]
   | `unzip of [> Biocaml_zip.Error.unzip ] ])
    Result.t Stream.t
(** Create a stream of raw_item results from an input-channel. *)

val in_channel_to_raw_item_stream_exn :
  ?zlib_buffer_size:int ->
  ?buffer_size:int ->
  in_channel -> raw_item Stream.t
(** Create a stream of raw_items from an input-channel (any call to
    [Stream.next] may throw an [Error _] exception). *)

val in_channel_to_item_stream :
  ?zlib_buffer_size:int ->
  ?buffer_size:int ->
  in_channel ->
  (Biocaml_sam.item,
   [> `bam of [> Error.t ]
   | `unzip of [> Biocaml_zip.Error.unzip ] ])
    Result.t Stream.t
(** Create a stream of full [Sam.item] results from an input-channel. *)

val in_channel_to_item_stream_exn :
  ?zlib_buffer_size:int -> ?buffer_size:int -> in_channel ->
  Biocaml_sam.item Stream.t
(** Create a stream of [Sam.item]s from an input-channel (any call to
    [Stream.next] may throw an [Error _] exception). *)


(** {2 [Transform.t] Creations } *)

module Transform: sig
  (** The low-level [Transform.t] implementations. *)

  val raw_to_item: unit ->
    (raw_item, (Biocaml_sam.item, [> Error.raw_to_item]) Result.t)
      Biocaml_transform.t
  (** Create a transform that lifts [raw_item]s to the higher-level representation
      defined in the [Biocaml_sam] module. *)

  val string_to_raw:
    ?zlib_buffer_size:int ->
    unit ->
    (string,
     (raw_item, [> `unzip of Biocaml_zip.Error.unzip
                | `bam of Error.raw_bam ] )
       Result.t)
      Biocaml_transform.t
  (** Create a transform that parses a BAM file.
      The [zlib_buffer_size] is passed to the [Biocaml_zip] module. *)


  val item_to_raw: unit ->
    (Biocaml_sam.item,
     (raw_item, [> Error.item_to_raw]) Result.t) Biocaml_transform.t
  (** Create a transform that downgrades [Sam.item]s to [raw_item]s. *)

  val raw_to_string: ?gzip_level:int -> ?zlib_buffer_size:int -> unit ->
    (raw_item, string) Biocaml_transform.t
  (** Create a transform that “prints” [raw_item]s in the BAM format.
      The [gzip_level] and [zlib_buffer_size] options are passed to
      the [Biocaml_zip] module. *)

end

(** {2 Lower-level (parsing) functions} *)

val parse_cigar: ?pos:int -> ?len:int -> string ->
  (Biocaml_sam.cigar_op array, [> Error.parse_cigar]) Result.t
(** Parse CIGAR operations from a string (c.f. [raw_alignment.cigar]). *)

val parse_optional: ?pos:int -> ?len:int -> string ->
  (Biocaml_sam.optional_content, [> Error.parse_optional]) Result.t
(** Parse optional content from a string (c.f. [raw_alignment.optional]). *)


(** {2 S-Expressions} *)

val raw_alignment_of_sexp : Sexplib.Sexp.t -> raw_alignment
val sexp_of_raw_alignment : raw_alignment -> Sexplib.Sexp.t
val raw_item_of_sexp : Sexplib.Sexp.t -> raw_item
val sexp_of_raw_item : raw_item -> Sexplib.Sexp.t


