open OUnit

let () =
  ignore(OUnit.run_test_tt_main ("All" >::: Biocaml_test.Suite.all_tests));
