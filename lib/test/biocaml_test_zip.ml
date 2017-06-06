open Core_kernel
open CFStream
module Bed = Biocaml_unix.Bed
module Tfxm = Biocaml_unix.Tfxm
module Zip = Biocaml_unix.Zip
open OUnit

type error =
[ `left of Zip.Error.unzip
| `right of Bed.Error.parsing
]

let some_ok x = Some (Ok x)

let make_stream () : ((Bed.item, error) Result.t) Stream.t * (unit -> unit) =
  let file = "etc/test_data/bed_03_more_cols.bed" in
  let tmp = Filename.temp_file "biocaml_test_zip" ".gz" in
  Unix.system (sprintf "gzip -c %s > %s" file tmp) |> ignore;

  let unzip_and_parse =
    Tfxm.compose_results_merge_error
      (Zip.Transform.unzip ~format:`gzip ~zlib_buffer_size:24 ())
      (Bed.Transform.string_to_item
         ~more_columns:(`enforce [|`type_string; `type_int; `type_float|]) ()) in
  let ic = In_channel.create tmp in
  let stream = Tfxm.in_channel_strings_to_stream ic unzip_and_parse in
  (stream, fun () -> Sys.remove tmp)

let test_unzip () =
  let s, clean_up = make_stream () in

  let the_expected_list = [|`string "some_string"; `int 42; `float 3.14|] in
  assert_bool "03 chrA" (Stream.next s = some_ok ("chrA",  42,  45, the_expected_list));
  assert_bool "03 chrB" (Stream.next s = some_ok ("chrB", 100, 130, the_expected_list));
  assert_bool "03 chrC" (Stream.next s = some_ok ("chrC", 200, 245, the_expected_list));
  assert_bool "03 EOF" (Stream.next s = None);
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
  let t = Zip.Transform.unzip ~format:`gzip ~zlib_buffer_size () in
  let ic = In_channel.create (sprintf "%s.gz" tmp3) in
  let s = Tfxm.in_channel_strings_to_stream ~buffer_size ic t in
  let l = Stream.npeek s 300 in
  let expected = sprintf "%s\n%s\n" first second in
  let obtained =
    String.concat ~sep:"" (List.map l ~f:(function
    | Ok s -> s
    | Error _ -> failwithf "There was an unzipping error !" ())) in
  assert_equal ~printer:(ident) ~msg:"isomorphismish" expected obtained;
  cmd "rm -f %s.gz %s.gz %s.gz %s" tmp1 tmp2 tmp3 tmp3;
  ()

let gunzip_multiple_tests =
  List.map
    [ 10, 1;  10, 2; 10, 10; 1, 10; 200, 1; 200, 10; 200, 200; 10, 200; 1, 200]
    ~f:(fun (zlib_buffer_size, buffer_size) ->
      sprintf "Gunzip|cat(%d,%d)" zlib_buffer_size buffer_size
      >:: test_gunzip_multiple ~zlib_buffer_size ~buffer_size)

let tests = "ZIP" >::: [
  "Gunzip" >:: test_unzip;
] @ gunzip_multiple_tests
