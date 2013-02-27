open Biocaml_internal_pervasives

type t = string

let string_to_lines s =
  match String.split ~on:'\n' s with
  | [] -> assert false
  | [""] -> [], false
  | lines ->
    let n = List.length lines in
    match List.nth lines (n - 1) with
    | None -> assert false
    | Some "" -> List.take lines (n - 1), true
    | Some _ -> lines, false

let of_string_unsafe = Fn.id

let lstrip = String.lstrip
let rstrip = String.rstrip
let strip = String.strip
