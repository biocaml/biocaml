open Core_kernel.Std
module Phred_score = Biocaml_unix.Phred_score
open OUnit

let visible_chars =
  List.range ~stride:1 ~start:`inclusive ~stop:`inclusive 33 126

let test_char_conv () =
  assert_bool
    "char conversion failed"
    (List.for_all visible_chars ~f:(fun i ->
      let x = i - 33 in (* substract default offset *)
      Phred_score.(
        x
        |> fun x -> ok_exn (of_int x)
        |> fun x -> ok_exn (to_char x)
        |> fun x -> ok_exn (of_char x)
        |> to_int
      ) = x)
    )

let tests = "Phred_score" >::: [
  "char conversion" >:: test_char_conv;
]
