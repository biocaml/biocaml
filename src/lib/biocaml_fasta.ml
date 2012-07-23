open Batteries;; open Printf

exception Error of string

type record = string * string
type recordi = string * int list

let enum_of_lines e = 
  let same_group _ l = 
    String.length l = 0 || l.[0] <> '>'
  and record = function
    | [] -> assert false
    | h :: t when String.starts_with h ">" -> 
      (h, String.concat "" t)
    | h :: _ -> raise (Error "Incorrect FASTA format")
  in
  let comments, lines = Biocaml_comments.filter_comments_prefix '#' e in
  let records = 
    Enum.group_by same_group lines
    /@ (List.of_enum |- record)
  in comments, records

let enum_of_file path =
  File.lines_of path |> enum_of_lines

let enum_input ic =
  IO.lines_of ic |> enum_of_lines

let enum_of_linesi e =
  let same_group _ l =
    String.length l = 0 || l.[0] <> '>'
  and record = function
    | [] -> assert false
    | h :: t when String.starts_with h ">" ->
        (
          h,
          List.(t |> map (String.nsplit ~by:" " |- map int_of_string) |> flatten)
        )
    | h :: _ -> raise (Error "Incorrect FASTA format")
  in
  let comments, lines = Biocaml_comments.filter_comments_prefix '#' e in
  let records =
    Enum.group_by same_group lines
    /@ (List.of_enum |- record)
  in comments, records

let enum_inputi ic =
  IO.lines_of ic |> enum_of_linesi
