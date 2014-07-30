open OUnit
open Core.Std

let random_string n =
  String.init n ~f:(fun _ ->
      if Random.float 1. > 0.5 then '.' else 'o'
    )

let input_string iz n =
  let r = String.make n ' ' in
  Biocaml_bgzf.really_input iz r 0 n ;
  r

let test_parse_of_unparse n () =
  let open Biocaml_bgzf in
  let fn = Filename.temp_file "test" ".bgzf" in
  let s = random_string n in
  with_file_out fn ~f:(fun oz -> output oz s 0 n) ;
  let s' = with_file_in fn ~f:(fun iz ->
      input_string iz n
    )
  in
  assert_equal ~printer:ident s s'

let tests = "Bgzf" >::: [
  "Unparse/Parse 1-block file" >:: test_parse_of_unparse 0x100 ;
  "Unparse/Parse 2-block file" >:: test_parse_of_unparse 0x10000 ;
  "Unparse/Parse big file"     >:: test_parse_of_unparse 0x1000000 ;
]
