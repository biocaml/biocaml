open OUnit
open Biocaml_phredScore

let visible_chars = 
  let open BatPervasives in
  33 -- 126 |> BatList.of_enum

let test_ascii_conv () = 
  let open BatPervasives in
  assert_bool 
    "ASCII conversion failed"
    (List.for_all 
       (fun i -> 
	 let x = i - 33 in (* substract default offset *)
	 (of_int |- to_ascii |- of_ascii |- to_int) x = x)
       visible_chars)

let tests = "PhredScore" >::: [
  "ASCII conversion" >:: test_ascii_conv;
]
