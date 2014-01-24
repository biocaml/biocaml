(** Strand names. *)
open Biocaml_internal_pervasives

type t
    (** Type of strand. *)

val equal : t -> t -> bool
val compare : t -> t -> int

exception Bad of string
  (** Raised when encountering an ill-formed strand. *)

val of_string : string -> t option
  (** Convert string to chromosome name if possible. *)

val of_string_exn : string -> t
  (** Like [of_string] but raise [Bad] if conversion not possible. *)

val to_string : t -> string
  (** String representation of a strand. Only guarantee is that returned string can be converted back using [of_string]. *)

val minus_plus : t -> string
  (** Return "-" or "+". *)
  
val rev_fwd : t -> string
  (** Return "rev" or "fwd". *)
  
