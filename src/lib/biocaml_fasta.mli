(** FASTA files. *)
open Batteries_uni

exception Error of string

type record = string * string
    (** A header and a sequence. *)

val enum_input : IO.input -> Biocaml_comments.t * record Enum.t
  (** Returns comments and enumeration of fasta records in given input. *)
