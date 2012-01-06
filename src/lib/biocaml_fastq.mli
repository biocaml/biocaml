(** FASTQ data. *)
open Batteries

exception Invalid of string

type record = (string * string * string * string)
    (** Each record in a fastq file consists of 4 fields: title,
        sequence, another title, and a quality score. *)

val enum_input : IO.input -> record Enum.t
  (** Returns enumeration of fastq records in given input. *)
