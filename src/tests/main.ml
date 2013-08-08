open OUnit

let all_tests = [
  Test_table.tests;
  Test_fasta.tests;
  Test_interval_tree.tests;
  Test_phred_score.tests;
  Test_pwm.tests;
  Test_roc.tests;
  Test_bed.tests;
  Test_wig.tests;
  Test_gff.tests;
  Test_track.tests;
  Test_sam.tests;
  Test_zip.tests;
  Test_vcf.tests;
  Test_rset.tests;
]

let () =
  ignore(OUnit.run_test_tt_main ("All" >::: all_tests));
