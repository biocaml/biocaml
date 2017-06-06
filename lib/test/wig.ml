open Core_kernel.Std
open CFStream
module Tfxm = Biocaml_unix.Tfxm
module Wig = Biocaml_unix.Wig
open OUnit

let file_parser_stream file =
  let filename = "etc/test_data/" ^ file in
  let t =
    Wig.Transform.string_to_item  ~filename () in
  let ic = open_in filename in
  Tfxm.in_channel_strings_to_stream ~buffer_size:10 ic t

let file_reprinter_stream file =
  let filename = "etc/test_data/" ^ file in
  let t =
    Wig.Transform.string_to_item ~filename () in
  let printer = Wig.Transform.item_to_string () in
  let transfo = Tfxm.compose_result_left t printer in
  let ic = open_in filename in
  Tfxm.in_channel_strings_to_stream ~buffer_size:4 ic transfo

let check_output s m v =
  assert_bool (sprintf "check_output: %s" m) (Stream.next s = Some (Ok v))
let check_error s m f =
  assert_bool (sprintf "check_error: %s" m) (
    match Stream.next s with
    | Some (Error e) -> f e
    | _ -> false)
let check_end s =
  assert_bool "check_end 1" (Stream.next s = None);
  assert_bool "check_end 2" (Stream.next s = None);
  assert_bool "check_end 3" (Stream.next s = None);
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
  | (`incomplete_input (_)) -> true
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
  | `incomplete_input (_) -> true
  | _ -> false);

  let s = file_reprinter_stream "wig_02.wig" in

  check_output s "comment" "# one comment\n";
  check_output s "variableStep=150" "variableStep chrom=chr19\n";
  check_output s "49304701 10" "49304701 10\n";
  check_output s "bedgraph" "chrA 49304901 49304902 12.5\n";
  check_error s "missing_start_value" (function
  | `missing_start_value (_, _) -> true
  | _ -> false);

  check_output s "1000" "1000\n";

  check_error s "wrong_fixed_step_value" (function
  | `wrong_fixed_step_value (_, " 900s") -> true
  | _ -> false);

  check_output s "800" "800\n";

  check_end s;
  ()

let test_to_bed_graph () =
  let stream file =
    let filename = "etc/test_data/" ^ file in
    let t =
      Wig.Transform.string_to_item ~filename () in
    let to_bg = Wig.Transform.item_to_bed_graph () in
    let transfo = Tfxm.compose_results_merge_error t to_bg in
    let ic = open_in filename in
    Tfxm.in_channel_strings_to_stream ~buffer_size:7 ic transfo in
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
  | (`left (`incomplete_input (_))) -> true
  | _ -> false);
  ()



let tests = "WIG" >::: [
  "Parse WIG" >:: test_parser;
  "Print WIG" >:: test_printer;
  "WIG -> bed-graph" >:: test_to_bed_graph;
]
