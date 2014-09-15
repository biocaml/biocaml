open OUnit
open Core.Std

let random_string n =
  String.init n ~f:(fun _ ->
      if Random.float 1. > 0.5 then '.' else 'o'
    )

let test_parse_past_eof () =
  let open Biocaml_bgzf in
  Utils.with_temp_file "test" ".bgzf" ~f:(fun fn ->
      with_file_out fn ~f:(fun oz -> output_string oz "BAM") ;
      assert_raises ~msg:"Reading past end of file should raise" End_of_file (fun () ->
          with_file_in fn ~f:(fun iz -> input_string iz 4)
        )
    )

let test_parse_of_unparse n () =
  let open Biocaml_bgzf in
  let s = random_string n in
  let n = String.length s in
  Utils.with_temp_file "test" ".bgzf" ~f:(fun fn ->
      with_file_out fn ~f:(fun oz -> output_string oz s) ;
      let s' = with_file_in fn ~f:(fun iz -> input_string iz n) in
      assert_equal ~printer:ident s s'
    )

let tests = "Bgzf" >::: [
    "Try parsing past EOF" >:: test_parse_past_eof ;
    "Unparse/Parse 1-block file" >:: test_parse_of_unparse 0x100 ;
    "Unparse/Parse 2-block file" >:: test_parse_of_unparse 0x10000 ;
    "Unparse/Parse big file"     >:: test_parse_of_unparse 0x1000000 ;
  ]
