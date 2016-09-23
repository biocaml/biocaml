open Sexplib.Std
module String = Biocaml_string

type t = string [@@deriving sexp]

let empty = ""

let rightmost x =
  match String.rsplit2 x ~on:'\n' with
  | None -> (None, x)
  | Some (b, a) -> (Some b, a)

let string_to_lines s = String.split ~on:'\n' s


let split = String.split

let append x y = x ^ y

let to_string x = x
