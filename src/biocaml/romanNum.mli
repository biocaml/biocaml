(** Roman numerals. Implementation employs Nathan Mishra Linger's [Roman] module, but an improved interface is provided here. Also, zero is deliberately not supported. *)

type t
    (** Type of a roman numeral. *)
    
exception Bad of string
  (** Raised when unable to produce a well-formed roman numeral. *)
  
val of_string : string -> t option
  (** Parse string as a roman numeral if possible. *)

val of_string_exn : string -> t
  (** Like [of_string] but raise [Bad] if unable to parse string. *)

val of_int : int -> t option
  (** Convert integer to roman numeral if possible. *)
  
val of_int_exn : int -> t
  (** Like [of_int] but raise [Bad] if unable to convert integer. *)
  
val to_string : t -> string
  (** String representation of a roman numeral. Returned string can be successfully parsed back with [of_string]. *)

val to_int : t -> int
  (** Convert roman numeral to integer. *)
