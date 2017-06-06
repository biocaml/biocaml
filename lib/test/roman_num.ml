open Core_kernel.Std
module Roman_num = Biocaml_unix.Roman_num
open OUnit

let test_conv () =
  assert_bool
    "roman num conversion failed"
    (
      List.init 5000 ~f:(fun x -> x+1)
      |> List.for_all ~f:(fun x ->
        x = Roman_num.(
          of_arabic x |> ok_exn
          |> to_roman |> of_roman |> ok_exn
          |> to_arabic
        ) )
    )

let tests = "Roman_num" >::: [
  "roman num conversion" >:: test_conv;
]
