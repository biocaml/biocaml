(** PHRED quality scores.

    A PHRED score is defined as -10*log(p) rounded to an integer,
    where p is a probability.

    To conserve space, the integer value of a PHRED score is encoded
    as an ASCII character in fastq files. Unfortunately two encodings
    have been used, one that increments the value by 33 and the other
    by 64. Most fastq files use 33 and that is the default in this
    module.

    However, Illumina used a 64 offset for a brief period of time, and
    you must be careful to know whether you have fastq files with this
    encoding. For details see
    {{:http://dx.doi.org/10.1093/nar/gkp1137}The Sanger FASTQ file
    format for sequences with quality scores, and the Solexa/Illumina
    FASTQ variants}. Using an offset of 33 or 64 in this module
    corresponds to using the fastq-sanger or fastq-illumina encodings,
    respectively, defined in this paper.
*)
open Batteries

exception Error of string

type t

val to_int : t -> int

val to_probability : t -> float

val to_ascii : ?offset:[`offset33 | `offset64] -> t -> char
  (** [to_ascii t] encodes [t] as an ASCII character.

      @raise Error if [t] with the given [offset] cannot be encoded as
      a visible ASCII character (codes 33 - 126).
  *)

val of_int : int -> t
  (** [of_int x] returns the PHRED score with the same value [x], but
      assures that [x] is non-negative.
      
      @raise Error if [x] is negative.
  *)

val of_ascii : ?offset:[`offset33 | `offset64] -> char -> t
  (** [of_ascii ~offset x] returns the PHRED score encoded by ASCII
      character [x].

      @raise Error if [x] does not represent a valid score.
  *)

val of_probability : ?f:(float -> int) -> float -> t
  (** [of_probability ~f x] returns [-10 * log_10(x)], which is the
      definition of PHRED scores.

      PHRED scores are integral, and it is unclear what convention is
      used to convert the resulting float value to an integer. Thus,
      the optional [f] is provided to dictate this. The default is to
      round the computed score to the closest integer.

      @raise Error if [x] is not between 0.0 - 1.0.
  *)
