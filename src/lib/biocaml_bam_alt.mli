(** Read and write BAM format.

    The implementation does its best to comply with the {{:
    http://samtools.github.io/hts-specs/SAMv1.pdf } official
    specification}.
*)

open Core.Std

(** A BAM file is composed of a header and a list of alignment
    records. The datatypes used in this module are the same than in
    the {!Biocaml.SAM} module. *)

type header = Biocaml_sam.header

type alignment = Biocaml_sam.alignment

val read : in_channel -> (header * alignment Or_error.t Stream.t) Or_error.t
(** [read ic] returns an error if a valid header cannot be read from
    [ic] or a pair containing a header and a stream of possibly
    errored alignments. The stream stops after the first error. *)

val with_file : string -> f:(header -> alignment Or_error.t Stream.t -> 'a) -> 'a Or_error.t
(** [with_file fn ~f] opens a BAM file for reading, applies [f] and
    closes the file after that, even if [f] raises an exception. {b
    Beware}: the result of [f] {b should not} lazily depend on the
    stream it receives as a second argument, since after the call to
    [with_file] the underlying channel is closed. *)

(* val write : header -> alignment Stream.t -> out_channel -> unit *)
