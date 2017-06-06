(** Roman numerals. Values greater than or equal to 1 are valid roman
    numerals. *)
open Core_kernel

type t = private int

(** Parse string as a roman numeral if possible. Case-insensitive. *)
val of_roman : string -> t Or_error.t

(** Convert integer to roman numeral. Return Error if given int is
    less than 1. *)
val of_arabic : int -> t Or_error.t

(** String representation of a roman numeral. *)
val to_roman : t -> string

(** Integer representation of a roman numeral. *)
val to_arabic : t -> int
