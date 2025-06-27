open Biocaml_base
open OUnit

let test_parse_string () =
  let f input answer =
    assert_equal
      ~printer:(fun x -> Sexp.to_string ([%sexp_of: string list] x))
      answer
      (Line.parse_string input :> string list)
  in
  f "\naa\n\n\nbb" [ ""; "aa"; ""; ""; "bb" ];
  f "aa\n\n" [ "aa"; ""; "" ]
;;

let tests = "Line" >::: [ "Base.Line.parse_string" >:: test_parse_string ]
