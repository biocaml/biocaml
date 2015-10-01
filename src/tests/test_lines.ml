open OUnit
open Core.Std
open CFStream
open Sexplib.Std

module Lines = Biocaml_lines

let test_of_string () =
  assert_equal
    ~printer:(fun x -> Sexp.to_string ([%sexp_of:string list] x))
    ["" ; "aa" ; "" ; "" ; "bb"]
    (Lines.of_string "\naa\n\n\nbb" |> Stream.to_list |> List.map ~f:Biocaml_line.to_string)

let tests = "Lines" >::: [
  "of_string" >:: test_of_string
]
