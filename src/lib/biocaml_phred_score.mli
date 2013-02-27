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

exception Error of string

type t

val of_ascii : ?offset:[`offset33 | `offset64] -> char -> t option
(** [of_ascii ~offset x] returns the PHRED score encoded by ASCII
    character [x]. *)

val of_ascii_exn : ?offset:[`offset33 | `offset64] -> char -> t
(** Like [of_ascii] but may raise
    [Error _] if [x] does not represent a valid score. *)

val to_ascii : ?offset:[`offset33 | `offset64] -> t -> char option
(** [to_ascii t] encodes [t] as an ASCII character. *)

val to_ascii_exn : ?offset:[`offset33 | `offset64] -> t -> char
  (** [to_ascii_exn t] encodes [t] as an ASCII character.

      @raise Error if [t] with the given [offset] cannot be encoded as
      a visible ASCII character (codes 33 - 126).
  *)

val of_int_exn : int -> t
  (** [of_int_exn x] returns the PHRED score with the same value [x], but
      assures that [x] is non-negative.

      @raise Error if [x] is negative.
  *)

val to_int : t -> int
(** Convert a phred score to an integer. *)

val of_probability : ?f:(float -> int) -> float -> t option
  (** [of_probability ~f x] returns [-10 * log_10(x)], which is the
      definition of PHRED scores.

      PHRED scores are integral, and it is unclear what convention is
      used to convert the resulting float value to an integer. Thus,
      the optional [f] is provided to dictate this. The default is to
      round the computed score to the closest integer.
  *)

val of_probability_exn : ?f:(float -> int) -> float -> t
(** See [of_probability]

    @raise Error if [x] is not between 0.0 - 1.0. *)

val to_probability : t -> float
  (** [to_probablity x] converts [x] to a probablity score. Note this
      is not the inverse of [of_probability] due to the rounding done by
      the latter. *)

val of_solexa_score : ?f:(float -> int) -> Biocaml_solexa_score.t -> t
  (** [of_solexa_score x] converts Solexa score [x] to a PHRED
      score.

      The conversion produces a float, and it is unclear what
      convention is used to convert the resulting float value to an
      integer. As in {!of_probability}, the optional [f] parameter is
      provided to dictate this.
  *)

val to_solexa_score : ?f:(float -> int) -> t -> Biocaml_solexa_score.t
  (** [to_solexa_score t] converts PHRED score [t] to a Solexa
      score.

      The conversion produces a float, and it is unclear what
      convention is used to convert the resulting float value to an
      integer. As in {!of_probability}, the optional [f] parameter is
      provided to dictate this.
  *)

include Core.Sexpable.S with type t := t
