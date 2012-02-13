open OUnit

let all_tests = [
  Test_fasta.tests;
  Test_intervalTree.tests;
  Test_phredScore.tests;
  Test_pwm.tests;
]

let () =
  ignore(OUnit.run_test_tt_main ("All" >::: all_tests));

