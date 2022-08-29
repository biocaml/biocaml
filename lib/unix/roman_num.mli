(** Roman numerals. Values greater than or equal to 1 are valid roman
    numerals. *)

type t = private int

val of_roman : string -> t Or_error.t
(** Parse string as a roman numeral if possible. Case-insensitive. *)

val of_arabic : int -> t Or_error.t
(** Convert integer to roman numeral. Return Error if given int is
    less than 1. *)

val to_roman : t -> string
(** String representation of a roman numeral. *)

val to_arabic : t -> int
(** Integer representation of a roman numeral. *)
