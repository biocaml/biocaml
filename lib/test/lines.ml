open Core_kernel
open CFStream
open OUnit

let check_int = assert_equal ~printer:Int.to_string

let check_string_list =
  assert_equal
    ~printer:(fun x -> Sexp.to_string ([%sexp_of:string list] x))

let test_base_parser () =
  let open Biocaml_base in
  let module P = Lines.Parser in
  check_int 0 P.(line_number initial_state) ;
  let s1, l1 = P.(step initial_state) (Some "\naa\n\n\nbb") in
  check_int 5 (P.line_number s1) ;
  check_string_list ["" ; "aa" ; "" ; "" ] (l1 :> string list) ;
  let s2, l2 = P.step s1 (Some "aa\n\n\n") in
  check_int 7 (P.line_number s2) ;
  check_string_list ["bbaa" ; "" ; ""] (l2 :> string list) ;
  let s3, l3 = P.step s2 None in
  check_int 7 (P.line_number s3) ;
  check_string_list [] (l3 :> string list)

module Line = Biocaml_unix.Line
module Lines = Biocaml_unix.Lines

let test_of_string () =
  let f input answer =
    check_string_list
      answer
      (Lines.of_string input |> Stream.to_list |> List.map ~f:Line.to_string)
  in
  f "\naa\n\n\nbb" ["" ; "aa" ; "" ; "" ; "bb"]

let tests = "Lines" >::: [
    "Base.Lines.Parser" >:: test_base_parser ;
    "of_string" >:: test_of_string ;
  ]
