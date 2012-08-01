
open OUnit
open Core.Std

module TS = Biocaml_transform.Pull_based
let file_parser_stream file =
  let filename = "src/tests/data/" ^ file in
  let parser =
    Biocaml_wig.parser ~pedantic:true ~sharp_comments:true ~filename () in
  let stream = TS.of_file ~buffer_size:10 filename parser in
  stream

let file_reprinter_stream file =
  let filename = "src/tests/data/" ^ file in
  let parser =
    Biocaml_wig.parser ~pedantic:true ~sharp_comments:true ~filename () in
  let printer = Biocaml_wig.printer () in
  let transfo = Biocaml_transform.compose parser printer in
  let stream = TS.of_file ~buffer_size:4 filename transfo in
  stream

let check_output s m v =
  assert_bool (sprintf "check_output: %s" m) (TS.next s = `output v)
let check_error s m f =
  assert_bool (sprintf "check_error: %s" m) (
    match TS.next s with
    | `error e -> f e
    | _ -> false)
let check_end s =
  assert_bool "check_end 1" (TS.next s = `end_of_stream);
  assert_bool "check_end 2" (TS.next s = `end_of_stream);
  assert_bool "check_end 3" (TS.next s = `end_of_stream);
  ()

let test_parser () =
  let s = file_parser_stream "wig_01.wig" in
    
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
  
  let s = file_parser_stream "wig_02.wig" in

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


let test_printer () =
  let s = file_reprinter_stream "wig_01.wig" in
  check_output s "comment" "# one comment\n";

  check_output s "variableStep=150" "variableStep chrom=chr19 span=150\n";
  check_output s "49304701 10" "49304701 10\n";
  check_output s "49304901 12.5" "49304901 12.5\n";
  check_output s "49305401 15" "49305401 15\n";
  check_output s "fixedStep " "fixedStep chrom=chr19 start=49307401 step=300 span=200\n";
  check_output s "1000" "1000\n";
  check_output s "900" "900\n";
  check_output s "800" "800\n";
  check_output s "300" "300\n";
  check_output s "200" "200\n";
  check_error s "incomplete line" (function
  | `left (`incomplete_line (_, " 100")) -> true
  | _ -> false);
  
  let s = file_reprinter_stream "wig_02.wig" in

  check_output s "comment" "# one comment\n";
  check_output s "variableStep=150" "variableStep chrom=chr19\n";
  check_output s "49304701 10" "49304701 10\n";
  check_output s "bedgraph" "chrA 49304901 49304902 12.5\n";
  check_error s "missing_start_value" (function
  | `left (`missing_start_value (_, _)) -> true
  | _ -> false);
  
  check_output s "1000" "1000\n";

  check_error s "wrong_fixed_step_value" (function
  | `left (`wrong_fixed_step_value (_, " 900s")) -> true
  | _ -> false);

  check_output s "800" "800\n";
  
  check_end s;
  ()

let test_to_bed_graph () =
  let stream file =
    let filename = "src/tests/data/" ^ file in
    let parser =
      Biocaml_wig.parser ~pedantic:true ~sharp_comments:true ~filename () in
    let to_bg = Biocaml_wig.to_bed_graph () in
    let transfo = Biocaml_transform.compose parser to_bg in
    let stream = TS.of_file ~buffer_size:7 filename transfo in
    stream in
  
  let s = stream "wig_01.wig" in

  check_output s "" ( ("chr19", 49304701, 49304850, 10.));
  check_output s "" ( ("chr19", 49304901, 49305050, 12.5));
  check_output s "" ( ("chr19", 49305401, 49305550, 15.));
  check_output s "" ( ("chr19", 49307401, 49307600, 1000.));
  check_output s "" ( ("chr19", 49307701, 49307900, 900.));
  check_output s "" ( ("chr19", 49308001, 49308200, 800.));
  check_output s "" ( ("chr19", 49308301, 49308500, 300.));
  check_output s "" ( ("chr19", 49308601, 49308800, 200.));

  check_error s "incomplete_line" (function
  | (`left (`incomplete_line (_, " 100"))) -> true
  | _ -> false);
  ()


  
let tests = "WIG" >::: [
  "Parse WIG" >:: test_parser;
  "Print WIG" >:: test_printer;
  "WIG -> bed-graph" >:: test_to_bed_graph;
]
