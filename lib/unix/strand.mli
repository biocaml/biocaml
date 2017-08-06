(** Strand names. There are various conventions for referring to the
    two strands of DNA. This module provides an [of_string] function
    that parses the various conventions into a canonical
    representation, which we define to be '-' or '+'. *)
open Core_kernel

(** Only valid values are '-' or '+'. *)
type t = private char

val minus : t
val plus : t

(** Convert string to strand name if possible. *)
val of_string : string -> t Or_error.t

(** Return '-' or '+'. *)
val minus_plus : t -> char

(** Return "rev" or "fwd". *)
val rev_fwd : t -> string
