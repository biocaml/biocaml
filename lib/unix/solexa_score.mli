(** Solexa quality scores.

    A Solexa score is defined as -10*log(p/(1-p)) rounded to an
    integer, where p is a probability. Phred scores are far more
    widely used, and the {!module: Phred_score} module supports
    converting Solexa scores to Phred scores.
    
    For details see {{:http://dx.doi.org/10.1093/nar/gkp1137}The
    Sanger FASTQ file format for sequences with quality scores, and
    the Solexa/Illumina FASTQ variants}. This module supports what is
    called the fastq-solexa format in this paper, with one
    exception. We are more permissive here in allowing conversions
    from/to the entire range of visible ASCII characters (codes 33 -
    126) instead of restricting to codes 59 - 126 as specified in this
    paper. The smaller range is apparently based on the original
    Solexa software returning minimum scores of -5, but there is no
    reason for this minimum based on the general definition of Solexa
    scores.
*)
open Core_kernel

exception Error of string

type t = int

val of_ascii : char -> t
  (** [of_ascii x] returns the PHRED score encoded by ASCII character
      [x].

      @raise Error if [x]'s ASCII code is not between 33 - 126.
  *)

val to_ascii : t -> char
  (** [to_ascii t] encodes [t] as an ASCII character.

      @raise Error if [t] cannot be encoded as a visible ASCII
      character (codes 33 - 126).
  *)

val of_probability : ?f:(float -> int) -> float -> t
  (** [of_probability ~f x] returns [-10 * log_10(x/(1-x))], which is the
      definition of Solexa scores.

      Solexa scores are integral, and it is unclear what convention is
      used to convert the resulting float value to an integer. Thus,
      the optional [f] is provided to dictate this. The default is to
      round the computed score to the closest integer.

      @raise Error if [x] is not between 0.0 - 1.0.
  *)

val to_probability : t -> float
  (** [to_probablity x] converts [x] to a probablity score. Note this
      is not the inverse of [of_probability] due to the rounding done by
      the latter. *)
