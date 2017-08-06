open Core_kernel
open CFStream
module Line = Biocaml_unix.Line
module Lines = Biocaml_unix.Lines
open OUnit

let test_of_string () =
  assert_equal
    ~printer:(fun x -> Sexp.to_string ([%sexp_of:string list] x))
    ["" ; "aa" ; "" ; "" ; "bb"]
    (Lines.of_string "\naa\n\n\nbb" |> Stream.to_list |> List.map ~f:Line.to_string)

let tests = "Lines" >::: [
  "of_string" >:: test_of_string
]
