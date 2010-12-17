(** Classification of data formats. *)

open Batteries_uni
  
exception Invalid of string
  (** Raised when an invalid tag or incompatible list of tags is encounted. *)
  
type t
    (** Collection of tags, which classify a data format. *)
    
val of_string : string -> t
  (** Parse given string as list of tags. Raise [Invalid] if any tags
      invalid, or if list of tags are incompatible. *)

val mem : string -> t -> bool
  (** [mem tag t] returns true if [t] contains a value for [tag]. *)

val find : string -> t -> string
  (** [find tag t] returns the value of [tag] in [t]. Raise
      [Not_found] if [t] does not contain value for [t]. *)

val tag_is : string -> string -> t -> bool
  (** [tag_is x y t] returns true if [x]=[y] in [t], or false if tag
      [x] assigned some other value or not found in [t]. *)
