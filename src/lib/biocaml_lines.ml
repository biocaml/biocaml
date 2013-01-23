open Biocaml_internal_pervasives

type item = string

let string_to_items s =
  match String.split ~on:'\n' s with
  | [] -> assert false
  | [""] -> [], false
  | lines ->
    let n = List.length lines in
    match List.nth lines (n - 1) with
    | None -> assert false
    | Some "" -> List.take lines (n - 1), true
    | Some _ -> lines, false
