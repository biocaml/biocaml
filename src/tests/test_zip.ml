
open OUnit
open Core.Std


let make_stream () =
  let file = "src/tests/data/bed_03_more_cols.bed" in
  let tmp = Filename.temp_file "biocaml_test_zip" ".gz" in
  Unix.system (sprintf "gzip -c %s > %s" file tmp) |! ignore;

  let unzip_and_parse =
    Biocaml_transform.bind_result_merge_error
      (Biocaml_zip.unzip ~format:`gzip ~zlib_buffer_size:24 ())
      (Biocaml_bed.Transform.string_to_t ~more_columns:[`string; `int; `float] ()) in
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

let cmd fmt = ksprintf (fun s ->
  (* eprintf "cmd: %S\n%!" s; *)
  let r = Sys.command s in
  if r <> 0 then failwithf "Command %s returned %d" s r ()) fmt

let test_gunzip_multiple ~zlib_buffer_size ~buffer_size () =
  let first = "ABCDEFGHIJKLMNOPQ" in
  let second = "abcdefghijklmnopqrstuvwxyz" in
  let tmp1 = Filename.temp_file "biocaml_test_zip_01" "" in
  let tmp2 = Filename.temp_file "biocaml_test_zip_02" "" in
  let tmp3 = Filename.temp_file "biocaml_test_zip_03" "" in
  cmd "echo '%s' > %s" first tmp1;
  cmd "echo '%s' > %s" second tmp2;
  cmd "gzip %s" tmp1;
  cmd "gzip %s" tmp2;
  cmd "cat %s.gz %s.gz > %s.gz" tmp1 tmp2 tmp3;
  let t = Biocaml_zip.unzip ~format:`gzip ~zlib_buffer_size () in
  let s =
    Biocaml_transform.Pull_based.(of_file ~buffer_size
                                    (sprintf "%s.gz" tmp3) t |! to_stream_result)
  in
  let l = Stream.npeek 300 s in
  let expected = sprintf "%s\n%s\n" first second in
  let obtained =
    String.concat ~sep:"" (List.map l (function
    | Ok s -> s
    | Error e -> failwithf "There was an unzipping error !" ())) in
  assert_equal ~printer:(ident) ~msg:"isomorphismish" expected obtained;
  cmd "rm -f %s.gz %s.gz %s.gz %s" tmp1 tmp2 tmp3 tmp3;
  ()

let gunzip_multiple_tests =
  List.map
    [ 10, 1;  10, 2; 10, 10; 1, 10; 200, 1; 200, 10; 200, 200; 10, 200; 1, 200]
    (fun (zlib_buffer_size, buffer_size) ->
      sprintf "Gunzip|cat(%d,%d)" zlib_buffer_size buffer_size
      >:: test_gunzip_multiple ~zlib_buffer_size ~buffer_size)
    
let tests = "ZIP" >::: [
  "Gunzip" >:: test_unzip;
] @ gunzip_multiple_tests
