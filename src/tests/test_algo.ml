open OUnit
open Biocaml_algo_edit_distance

let test_simple_edit_distance () =
  assert_bool
    "simple edit distance test failed"
    (
      let editDistanceTestList = 
      [("ISLANDER", "SLANDER", 1); ("MART", "KARMA", 3);
       ("KITTEN", "SITTING", 3); ("INTENTION", "EXECUTION", 5);
       ("PLEASANTLY", "MEANLY", 5)]
      in
      List.fold_left
        (fun result (a, b, distance) -> 
          result && (distance = (simpleEditDistance a b))
        ) true editDistanceTestList
    )

let tests = "Algo" >::: [
  "simple edit distance" >:: test_simple_edit_distance;
]
