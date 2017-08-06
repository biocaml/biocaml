(** Read and write BAM format.

    The implementation does its best to comply with the {{:
    http://samtools.github.io/hts-specs/SAMv1.pdf } official
    specification}.
*)

open Core_kernel

(** A BAM file is composed of a header and a list of alignment
    records. The datatypes used in this module are based on those
    defined in the {!Sam} module. *)

(** BAM header files contain a plain text SAM header, plus additional
    information related to the encoding of the file. *)
module Header : sig
  type t
  val of_sam : Sam.header -> t
  val to_sam : t -> Sam.header
end

type alignment = Sam.alignment

(** Representation of partially parsed alignments. When traversing a
    BAM file for a specific calculation, it may be that only some
    fields of the alignment records are actually used. In that case,
    it can be significantly faster to use this representation. As a
    downside, some encoding errors in the BAM file can go
    unnoticed. *)
module Alignment0 : sig
  type t [@@deriving sexp]

  val qname : t -> string option

  val flags : t -> Sam.Flags.t Or_error.t

  val rname : t -> Header.t -> string option Or_error.t

  (** Positions are 0-based, -1 if undefined*)
  val pos : t -> int option

  val mapq : t -> int option

  val cigar : t -> Sam.cigar_op list Or_error.t

  val rnext : t -> Header.t -> Sam.rnext option Or_error.t

  val pnext : t -> int option

  val tlen : t -> int option

  val seq : t -> string option

  val qual : t -> Phred_score.t list Or_error.t

  val optional_fields : t -> Sam.optional_field list Or_error.t

  val decode : t -> Header.t -> alignment Or_error.t

  val encode : alignment -> Header.t -> t Or_error.t
end

val read0 : In_channel.t -> (Header.t * Alignment0.t Or_error.t Stream.t) Or_error.t
(** [read0 ic] returns an error if a valid header cannot be read from
    [ic] or a pair containing a header and a stream of possibly
    errored (partially parsed) alignments. The stream stops after the first error. *)

val with_file0 : string -> f:(Header.t -> Alignment0.t Or_error.t Stream.t -> 'a Or_error.t) -> 'a Or_error.t
(** [with_file fn ~f] opens a BAM file for reading, applies [f] and
    closes the file after that, even if [f] raises an exception. {b
    Beware}: the result of [f] {b should not} lazily depend on the
    stream it receives as a second argument, since after the call to
    [with_file] the underlying channel is closed. *)

val write0 : Header.t -> Alignment0.t Stream.t -> Out_channel.t -> unit
(** [write0 h xs oc] writes the header [h] and (partially parsed)
    alignments [xs] to [oc]. *)

val read : In_channel.t -> (Header.t * alignment Or_error.t Stream.t) Or_error.t
(** [read ic] returns an error if a valid header cannot be read from
    [ic] or a pair containing a header and a stream of possibly
    errored alignments. The stream stops after the first error. *)

val with_file : string -> f:(Header.t -> alignment Or_error.t Stream.t -> 'a Or_error.t) -> 'a Or_error.t
(** [with_file fn ~f] opens a BAM file for reading, applies [f] and
    closes the file after that, even if [f] raises an exception. {b
    Beware}: the result of [f] {b should not} lazily depend on the
    stream it receives as a second argument, since after the call to
    [with_file] the underlying channel is closed. *)

val write : Header.t -> alignment Stream.t -> Out_channel.t -> unit Or_error.t
(** [write h xs oc] writes the header [h] and the alignments [xs] to
    [oc]. *)
