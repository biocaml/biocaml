(** Chromosome names. A chromosome name, as defined by this module,
    consists of two parts. An optional prefix "chr"
    (case-insensitive), followed by a suffix identifying the
    chromosome. The possible suffixes (case-insensitive) are:

    - "X" representing the maternal chromosome
    - "Y" representing the paternal chromosome
    - "M", "Mt", or "MtDNA" representing mitochondrial DNA
    - "N" where N is a positive number in either arabic or roman numeral form

    Some suffixes have ambiguous interpretations, e.g. "X" could be a
    Roman numeral or the maternal chromosome. Preference is given to
    the non-numeric interpretation. Thus "chrX" is treated as the
    maternal chromosome, not chromosome 10.  Strings not in the above
    form are left unaltered by the functions of this module.

    Functions for converting to an Arabic or Roman format are
    provided. The benefit is that these functions convert to a
    canonical form, and thus the resulting strings can be compared
    reliably. For example, [to_arabic "Chr4"] and [to_arabic "IV"] both
    return "4". The Arabic format is recommended over the Roman form
    because the Roman form is incomplete; e.g. it cannot represent chromosome
    number 10 because there would be an ambiguity with the maternal
    chromosome "chrX". *)
open Core_kernel

module Error : sig

  (** Possible errors:

      - `chromosome_ambiguous_in_roman_form chr - [chr]'s Roman
      representation is ambiguous, e.g. "chr10" and "chrX" both lead
      to "X" *)
  type t = [
  | `chromosome_ambiguous_in_roman_form of string
  ]

end

exception Error of Error.t

val to_arabic : string -> string
(** [to_arabic s] returns the canonical Arabic representation of [s]. *)

val to_roman : string -> (string, Error.t) Result.t
(** [to_roman s] returns the canonical Roman representation of [s]. *)

val to_roman_exn : string -> string
