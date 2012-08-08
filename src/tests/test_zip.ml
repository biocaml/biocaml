
open OUnit
open Core.Std


let make_stream () =
  let file = "src/tests/data/bed_03_more_cols.bed" in
  let tmp = Filename.temp_file "biocaml_test_zip" ".gz" in
  Unix.system (sprintf "gzip -c %s > %s" file tmp) |! ignore;

  let unzip_and_parse =
    Biocaml_transform.compose
      (Biocaml_zip.unzip ~format:`gzip ~zlib_buffer_size:24 ())
      (Biocaml_bed.parser ~more_columns:[`string; `int; `float] ()) in
  let stream =
    Biocaml_transform.Pull_based.(of_file tmp unzip_and_parse
                                  |! to_stream_result) in
  (stream, fun () -> Sys.remove tmp)

let test_unzip () =
  let s, clean_up = make_stream () in
    
  let the_expected_list = [`String "some_string"; `Int 42; `Float 3.14] in
  assert_bool "03 chrA" (Stream.next s = Ok ("chrA",  42,  45, the_expected_list));
  assert_bool "03 chrB" (Stream.next s = Ok ("chrB", 100, 130, the_expected_list));
  assert_bool "03 chrC" (Stream.next s = Ok ("chrC", 200, 245, the_expected_list));
  assert_raises Stream.Failure (fun () -> Stream.next s);
  clean_up ()

let tests = "ZIP" >::: [
  "Unzip" >:: test_unzip;
]
