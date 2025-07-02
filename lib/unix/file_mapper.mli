open Biocaml_base

exception Parse_error of string

type 'a t = string -> string -> f:('a -> 'a list) -> unit

val lines : Biocaml.Line.t t
val bed5 : Bed.Bed5.item t
val bed5_raw : Bed.Bed5_raw.item t
