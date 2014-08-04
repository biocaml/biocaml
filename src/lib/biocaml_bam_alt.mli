(** Parsing and printing of BAM files. *)

open Core.Std

type header = Biocaml_sam.header

type alignment

val read : in_channel -> (header * alignment Or_error.t Stream.t) Or_error.t

val with_file : string -> f:(header -> alignment Or_error.t Stream.t -> 'a) -> 'a Or_error.t

val write : header -> alignment Stream.t -> out_channel -> unit
