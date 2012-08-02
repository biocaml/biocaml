
open OUnit
open Core.Std

open Biocaml_gff
let test_parser () =
  let transfo = parser () in
  let test_line l f =
    let joined = (String.concat ~sep:"\t" l) in
    Biocaml_transform.feed transfo (joined ^ "\n");
    assert_bool joined (f (Biocaml_transform.next transfo))
  in
  let test_output l o = test_line l (fun oo -> o = oo) in
  test_output ["# some comment"] (`output (`comment " some comment"));

  test_output [
    "big%20spaced%20name"; ".";  "."; "42"; "43"; "."; "."; ".";
  ] (`output
        (`record
            {seqname = "big spaced name"; source = None; feature = None;
             pos = (42, 43); score = None; strand = `not_applicable;
             phase = None; attributes = []}));

  test_output [
    "\"big\\tC style\""; "some";  "s"; "42"; "43"; "2."; "+"; "2";
    "k=v;big%20k=\"annoying v\""
  ] (`output
        (`record
            {seqname = "big\tC style"; source = Some "some"; feature = Some "s";
             pos = (42, 43); score = Some 2.; strand = `plus;
             phase = Some 2; attributes = ["k", "v"; "big k", "annoying v"]}));
  
  test_line ["\"big\\tC style\""; "some";  "s"; "42"; "43"; "2."; "+"]
    (function | `error (`wrong_row (_, _)) -> true | _ -> false);

  test_output [
    "big%20spaced%20name"; ".";  "."; "42"; "43"; "2e12"; "."; "."; ""
  ] (`output
        (`record
            {seqname = "big spaced name"; source = None; feature = None;
             pos = (42, 43); score = Some (2e12); strand = `not_applicable;
             phase = None; attributes = []}));

  test_line ["\"big\\tC style\""; "some";  "s"; "42"; "43"; "2."; "wrong"; "2"]
    (function | `error (`cannot_parse_strand (_, "wrong")) -> true | _ -> false);

  test_line ["\"big\\tC style\""; "some";  "s"; "42w"; "43"; "2."; "-"; "2"]
    (function | `error (`cannot_parse_int (_, _)) -> true | _ -> false);

  test_line ["\"big\\tC style\""; "some%wrong";  "s"; "42"; "43"; "2."; "-"; "2"]
    (function | `error (`wrong_url_escaping (_, "some%wrong")) -> true | _ -> false);

  test_line ["\"big\\tC style\""; "some";  "s"; "42"; "43"; "2."; "-"; "2";
            "some string"]
    (function | `error (`wrong_attributes (_, _)) -> true | _ -> false);

  test_line ["\"big\\tC style\""; "some";  "s"; "42"; "43"; "2."; "-"; "2";
            "some=string;djf"]
    (function | `error (`wrong_attributes (_, _)) -> true | _ -> false);

  let transfo = parser ~version:`two () in
  let test_line l f =
    let joined = (String.concat ~sep:"\t" l) in
    Biocaml_transform.feed transfo (joined ^ "\n");
    assert_bool joined (f (Biocaml_transform.next transfo))
  in
  let test_output l o = test_line l (fun oo -> o = oo) in
  test_output ["# some comment"] (`output (`comment " some comment"));

  test_output [
    "\"big\\tC style\""; "some";  "s"; "42"; "43"; "2."; "+"; "2";
    "k v ; \"big\\tk\" \"annoying v\"   ; "
  ] (`output
        (`record
            {seqname = "big\tC style"; source = Some "some"; feature = Some "s";
             pos = (42, 43); score = Some 2.; strand = `plus;
             phase = Some 2; attributes = ["k", "v"; "big\tk", "annoying v"]}));
  ()
   
let test_printer () =
  let transfo = printer () in
  let test s item =
    Biocaml_transform.feed transfo item;
    let res =  Biocaml_transform.next transfo in
    match res with
    | `output o ->
      if s <> o then eprintf "NOT EQUALS:\n%S\n%S\n%!" s o;
      assert_equal ~printer:ident s o 
    | `not_ready -> assert_bool "not_ready" false
    | `end_of_stream -> assert_bool "end_of_stream" false
    | `error _ -> assert_bool "error" false
  in
  test "# some\n" (`comment " some");

  test "big%20spaced%20name\t.\t.\t42\t43\t2\t.\t.\t\n"
        (`record
            {seqname = "big spaced name"; source = None; feature = None;
             pos = (42, 43); score = Some 2.; strand = `not_applicable;
             phase = None; attributes = []});
  test "big%20spaced%20name\t%09\t.\t42\t43\t.\t+\t.\tk=v;big%20k=an%3Bno%09ing%0Av\n"
        (`record
            {seqname = "big spaced name"; source = Some "\t"; feature = None;
             pos = (42, 43); score = None; strand = `plus;
             phase = None; attributes = [
               "k", "v"; "big k", "an;no\ting\nv"
             ]});
  let transfo = printer ~version:`two () in
  let test s item =
    Biocaml_transform.feed transfo item;
    let res =  Biocaml_transform.next transfo in
    match res with
    | `output o ->
      if s <> o then eprintf "NOT EQUALS (version 2):\n%S\n%S\n%!" s o;
      assert_equal ~printer:ident s o 
    | `not_ready -> assert_bool "not_ready" false
    | `end_of_stream -> assert_bool "end_of_stream" false
    | `error _ -> assert_bool "error" false
  in
  test "\"big spaced name\"\t\"\\t\"\t.\t42\t43\t.\t+\t.\t\"k\" \"v\";\"big k\" \"an;no\\ting\\nv\"\n"
        (`record
            {seqname = "big spaced name"; source = Some "\t"; feature = None;
             pos = (42, 43); score = None; strand = `plus;
             phase = None; attributes = [
               "k", "v"; "big k", "an;no\ting\nv"
             ]});
  ()

  
let tests = "GFF" >::: [
  "Parse GFF" >:: test_parser;
  "Print GFF" >:: test_printer;
]
