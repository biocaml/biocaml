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
    respectively, defined in this paper. However, note the term
    fastq-illumina is now misleading since Illumina has also switched
    to using an offset of 33.
*)
open Core_kernel

type t = private int
[@@deriving sexp]

type offset = [`Offset33 | `Offset64]
[@@deriving sexp]

val of_char : ?offset:offset -> char -> t Or_error.t
(** [of_char ~offset x] returns the PHRED score encoded by ASCII
    character [x]. *)

val to_char : ?offset:offset -> t -> char Or_error.t
(** [to_char t] encodes [t] as a visible ASCII character (codes 33 -
    126) if possible. *)

val of_int : int -> t Or_error.t
(** [of_int x] returns the PHRED score with the same value [x], or
    returns Error if [x] is negative. *)

val to_int : t -> int
(** Convert a PHRED score to an integer. *)

val of_probability : ?f:(float -> int) -> float -> t Or_error.t
(** [of_probability ~f x] returns [-10 * log_10(x)], which is the
    definition of PHRED scores.

    PHRED scores are integral, and it is only loosely specified that
    float value returned by the above formula should be "rounded to
    the closest integer". However, that statement is imprecise;
    there is more than one way to do such a rounding. A reasonable
    choice is made by default, but you can control the behavior by
    providing [f].

    Return Error if given probability [x] not between 0.0 and 1.0.
*)

val to_probability : t -> float
(** [to_probablity x] converts [x] to a probablity score. Note this
    is not the inverse of [of_probability] due to the rounding done
    by the latter. *)

val of_solexa_score : ?f:(float -> int) -> Solexa_score.t -> t
(** [of_solexa_score x] converts Solexa score [x] to a PHRED score.

    The conversion produces a float, and it is unclear what
    convention is used to convert the resulting float value to an
    integer. As in {!of_probability}, the optional [f] parameter is
    provided to dictate this.
*)

val to_solexa_score : ?f:(float -> int) -> t -> Solexa_score.t
(** [to_solexa_score t] converts PHRED score [t] to a Solexa score.

    The conversion produces a float, and it is unclear what
    convention is used to convert the resulting float value to an
    integer. As in {!of_probability}, the optional [f] parameter is
    provided to dictate this.
*)

(** The min and max PHRED scores when encoded as ASCII
    characters. Since PHRED scores are virtually always ASCII encoded,
    you are unlikely to see values outside this range. However, this
    module allows creating values outside this range,
    e.g. [of_probability 1e-13] exceeds [max_as_char], and
    [of_probability 0.9] is smaller than [min_as_char] (for either
    offset). *)
val min_as_char : offset -> t
val max_as_char : t
