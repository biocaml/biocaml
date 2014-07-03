open OUnit
open Core.Std
open Biocaml

let visible_chars = 
  List.range ~stride:1 ~start:`inclusive ~stop:`inclusive 33 126

let test_ascii_conv () = 
  assert_bool 
    "ASCII conversion failed"
    (List.for_all visible_chars ~f:(fun i -> 
      let x = i - 33 in (* substract default offset *)
      Phred_score.(x |! of_int_exn |! to_ascii_exn |! of_ascii_exn |! to_int) = x)
    )

let tests = "Phred_score" >::: [
  "ASCII conversion" >:: test_ascii_conv;
]
