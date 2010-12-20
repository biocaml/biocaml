(** FASTQ data. *)
open Batteries_uni

exception Invalid of string

val enum_input : IO.input -> (string * string * string * string) Enum.t
  (** Returns enumeration of the 4 lines constituting information about
      each read. TO DO: design better data type for this. *)
