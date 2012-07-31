
open OUnit
open Core.Std

module TS = Biocaml_transform.Pull_based
let stream_of_file file =
  let filename = "src/tests/data/" ^ file in
  let parser =
    Biocaml_wig.parser ~pedantic:true ~sharp_comments:true ~filename () in
  let stream = TS.of_file ~buffer_size:10 filename parser in
  stream

let test_parser () =
  let s = stream_of_file "wig_01.wig" in
  let check_output s m v =
    assert_bool (sprintf "check_output: %s" m) (TS.next s = `output v) in
  let check_error s m f =
    assert_bool (sprintf "check_error: %s" m) (
      match TS.next s with
      | `error e -> f e
      | _ -> false) in
  let check_end s =
    assert_bool "check_end 1" (TS.next s = `end_of_stream);
    assert_bool "check_end 2" (TS.next s = `end_of_stream);
    assert_bool "check_end 3" (TS.next s = `end_of_stream); in
    
  check_output s "comment line" (`comment " one comment");
  check_output s "variableStep" (`variable_step_state_change ("chr19", Some 150));

  check_output s "variable_step_value " (`variable_step_value (49304701, 10.));
  check_output s "variable_step_value " (`variable_step_value (49304901, 12.5));
  check_output s "variable_step_value " (`variable_step_value (49305401, 15.));
  check_output s "fixed_step_state_cha" (`fixed_step_state_change ("chr19", 49307401, 300, Some 200));
  check_output s "fixed_step_value 100" (`fixed_step_value 1000.);
  check_output s "fixed_step_value 900" (`fixed_step_value 900.);
  check_output s "fixed_step_value 800" (`fixed_step_value 800.);
  check_output s "fixed_step_value 300" (`fixed_step_value 300.);
  check_output s "fixed_step_value 200" (`fixed_step_value 200.);

  check_error s "incomplete line" (function
  | (`incomplete_line (_, " 100")) -> true
  | _ -> false);
  
  let s = stream_of_file "wig_02.wig" in

  check_output s "comment" (`comment " one comment");
  check_output s "variabl" (`variable_step_state_change ("chr19", None));
  check_output s "variabl" (`variable_step_value (49304701, 10.));
  check_output s "bed_gra" (`bed_graph_value ("chrA", 49304901, 49304902, 12.5));

  check_error s "missing_start_value" (function
  | (`missing_start_value (_, "fixedStep chrom=chr19  step=300 span=200")) -> true
  | _ -> false);
  
  check_output s "fix1000" (`fixed_step_value 1000.);

  check_error s "wrong_fixed_step_value" (function
  | (`wrong_fixed_step_value (_, " 900s")) -> true
  | _ -> false);
  check_output s "fixed_step_value" (`fixed_step_value 800.);
  check_end s;
  ()


let tests = "WIG" >::: [
  "Parse WIG" >:: test_parser;
]
