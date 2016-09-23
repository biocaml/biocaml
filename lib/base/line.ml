open Sexplib.Std
module String = Biocaml_string

type t = string [@@deriving sexp]

let to_string x = x

let string_to_lines s = String.split ~on:'\n' s

let split = String.split
