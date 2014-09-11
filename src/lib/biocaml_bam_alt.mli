(** Read and write BAM format.

    The implementation does its best to comply with the {{:
    http://samtools.github.io/hts-specs/SAMv1.pdf } official
    specification}.
*)

open Core.Std

(** A BAM file is composed of a header and a list of alignment
    records. The datatypes used in this module are the same than in
    the {!Biocaml.SAM} module. *)

module Header : sig
  type t
  val of_sam : Biocaml_sam.header -> t
  val to_sam : t -> Biocaml_sam.header
end

module Alignment0 : sig
  type t with sexp
  val qname : t -> string option
  val flags : t -> Biocaml_sam.Flags.t Or_error.t
  val rname : t -> Header.t -> string option Or_error.t
  val pos : t -> int option
  val mapq : t -> int option
  val cigar : t -> Biocaml_sam.cigar_op list Or_error.t
  val rnext : t -> Header.t -> Biocaml_sam.rnext option Or_error.t
  val pnext : t -> int option
  val tlen : t -> int option
  val seq : t -> string option
  val qual : t -> Biocaml_phred_score.t list Or_error.t
  val optional_fields : t -> Biocaml_sam.optional_field list Or_error.t
  val decode : t -> Header.t -> Biocaml_sam.alignment Or_error.t
  val encode : Biocaml_sam.alignment -> Header.t -> t Or_error.t
end

val read : in_channel -> (Header.t * Alignment0.t Or_error.t Stream.t) Or_error.t
(** [read ic] returns an error if a valid header cannot be read from
    [ic] or a pair containing a header and a stream of possibly
    errored alignments. The stream stops after the first error. *)

val with_file : string -> f:(Header.t -> Alignment0.t Or_error.t Stream.t -> 'a Or_error.t) -> 'a Or_error.t
(** [with_file fn ~f] opens a BAM file for reading, applies [f] and
    closes the file after that, even if [f] raises an exception. {b
    Beware}: the result of [f] {b should not} lazily depend on the
    stream it receives as a second argument, since after the call to
    [with_file] the underlying channel is closed. *)

val write : Header.t -> Alignment0.t Stream.t -> out_channel -> unit
