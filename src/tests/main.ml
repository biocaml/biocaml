open OUnit

let all_tests = [
  Test_phredScore.tests;
  Test_intervalTree.tests;
]

let () =
  ignore(OUnit.run_test_tt_main ("All" >::: all_tests));

