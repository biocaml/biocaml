(** Strand names. There are various conventions for referring to the
    two strands of DNA. This module provides an [of_string] function
    that parses the various conventions into a canonical
    representation, which we define to be '-' or '+'. *)

type t = private char
(** Only valid values are '-' or '+'. *)

val minus : t
val plus : t

val of_string : string -> t Or_error.t
(** Convert string to strand name if possible. *)

val minus_plus : t -> char
(** Return '-' or '+'. *)

val rev_fwd : t -> string
(** Return "rev" or "fwd". *)
