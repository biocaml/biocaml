module Test = Biocaml_test.Std
open OUnit

let () =
  ignore(OUnit.run_test_tt_main ("All" >::: Test.all_tests));
