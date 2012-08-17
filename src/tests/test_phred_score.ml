open OUnit
open Biocaml_phred_score

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
	 (of_int_exn |- to_ascii_exn |- of_ascii_exn |- to_int) x = x)
       visible_chars)

let tests = "Phred_score" >::: [
  "ASCII conversion" >:: test_ascii_conv;
]
