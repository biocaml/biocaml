open OUnit
open Biocaml_internal_pervasives
open Biocaml

let make_stream ?more_columns file : ((Bed.t, Bed.parse_error) Result.t) Stream.t =
  let filename = "src/tests/data/" ^ file in
  let bed_parser = Bed.Transform.string_to_t ?more_columns ~filename () in
  let inp = open_in filename in
  Transform.in_channel_strings_to_stream ~buffer_size:10 inp bed_parser

let some_ok x = Some (Ok x)

let test_parser () =
  let s = make_stream "bed_01.bed" in
  assert_bool "01 chrA" (Stream.next s = some_ok ("chrA", 42, 45, []));
  assert_bool "01 chrB" (Stream.next s = some_ok ("chrB", 100, 130, []));
  assert_bool "01 chrC" (Stream.next s = some_ok ("chrC", 200, 245, []));
  assert_bool "01 EOF" (Stream.next s = None);

  let s = make_stream "bed_02_incomplete_line.bed" in
  assert_bool "02 chrA" (Stream.next s = some_ok ("chrA", 42, 45, []));
  assert_bool "02 chrB error "
    (match Stream.next s with
    | Some (Error (`incomplete_input (_))) -> true
    | _ -> false);

  let s =
    make_stream ~more_columns:(`enforce [`string; `int; `float])
      "bed_03_more_cols.bed" in
  let the_expected_list = [`String "some_string"; `Int 42; `Float 3.14] in
  assert_bool "03 chrA" (Stream.next s = some_ok ("chrA",  42,  45, the_expected_list));
  assert_bool "03 chrB" (Stream.next s = some_ok ("chrB", 100, 130, the_expected_list));
  assert_bool "03 chrC" (Stream.next s = some_ok ("chrC", 200, 245, the_expected_list));
  assert_bool "03 EOF" (Stream.next s = None);

  let s =
    make_stream ~more_columns:(`enforce [`string; `int; `float])
      "bed_04_more_cols_error.bed" in
  let the_expected_list = [`String "some_string"; `Int 42; `Float 3.14] in
  assert_bool "04 chrA" (Stream.next s = some_ok ("chrA",  42,  45, the_expected_list));
  assert_bool "04 chrB error "
    (match Stream.next s with
    | Some (Error (`not_an_int (_, "forty_two"))) -> true
    | _ -> false);
  assert_bool "04 chrC error "
    (match Stream.next s with
    | Some (Error (`wrong_number_of_columns (_, l))) -> true
    | _ -> false);
  assert_bool "04 EOF" (Stream.next s = None);
  
  ()

let make_printer_stream ?more_columns file =
  let filename = "src/tests/data/" ^ file in
  let bed_parser = Bed.Transform.string_to_t ?more_columns ~filename () in
  let printer = Bed.Transform.t_to_string () in
  let trans = Transform.compose_result_left bed_parser printer in
  let ic = open_in filename in
  Transform.in_channel_strings_to_stream ~buffer_size:10 ic trans

let test_printer () =
  let s =
    make_printer_stream
      ~more_columns:(`enforce [`string; `int; `float]) "bed_03_more_cols.bed" in
  let camlstream =
    Stream.result_to_exn
      ~error_to_exn:(fun e -> failwith "Unexpected error in camlstream") s in
  
  let l = Stream.npeek camlstream Int.max_value in
  assert_equal
    ~printer:(fun l -> List.map ~f:(sprintf "Output: %S") l |! String.concat ~sep:", ")
    l ["chrA 42 45 some_string 42 3.14\n";
       "chrB 100 130 some_string 42 3.14\n";
       "chrC 200 245 some_string 42 3.14\n"; ];
  
  ()

let tests = "BED" >::: [
  "Parse BED" >:: test_parser;
  "Print BED" >:: test_printer;

]




