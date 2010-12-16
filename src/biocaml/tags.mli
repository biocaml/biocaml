(** Classification of data formats. *)

open Batteries_uni
  
type tag
    (** Represents a single property of a data format. *)
    
type t
    (** Collection of [tag]s, which classify a data format. *)
    
val of_string : string -> t
