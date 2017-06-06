open Core_kernel.Std
module Gff = Biocaml_unix.Gff
module Tfxm = Biocaml_unix.Tfxm
open OUnit

let test_parser () =
  let transfo = Gff.Transform.string_to_item ~tags:Gff.Tags.default () in
  let test_line l f =
    let joined = (String.concat ~sep:"\t" l) in
    Tfxm.feed transfo (joined ^ "\n");
    assert_bool joined (f (Tfxm.next transfo))
  in
  let test_output l o = test_line l (fun oo -> oo = `output (Ok o)) in
  test_output ["# some comment"]  (`comment " some comment");

  test_output [
    "big%20spaced%20name"; ".";  "."; "42"; "43"; "."; "."; ".";
  ]  (`record
         {Gff.seqname = "big spaced name"; source = None; feature = None;
          pos = (42, 43); score = None; strand = `not_applicable;
          phase = None; attributes = []});

  test_output [
    "\"big\\tC style\""; "some";  "s"; "42"; "43"; "2."; "+"; "2";
    "k=v,v%20v;big%20k=\"annoying v\""
  ]  (`record
            {Gff.seqname = "big\tC style"; source = Some "some"; feature = Some "s";
             pos = (42, 43); score = Some 2.; strand = `plus;
             phase = Some 2;
             attributes = ["k", ["v"; "v v"]; "big k", ["annoying v"]]});

  test_line ["\"big\\tC style\""; "some";  "s"; "42"; "43"; "2."; "+"]
    (function | `output (Error (`wrong_row (_, _))) -> true | _ -> false);

  test_output [
    "big%20spaced%20name"; ".";  "."; "42"; "43"; "2e12"; "."; "."; ""
  ]  (`record
         {Gff.seqname = "big spaced name"; source = None; feature = None;
          pos = (42, 43); score = Some (2e12); strand = `not_applicable;
          phase = None; attributes = []});

  test_line ["\"big\\tC style\""; "some";  "s"; "42"; "43"; "2."; "wrong"; "2"]
    (function | `output (Error (`cannot_parse_strand (_, "wrong"))) -> true | _ -> false);

  test_line ["\"big\\tC style\""; "some";  "s"; "42w"; "43"; "2."; "-"; "2"]
    (function | `output (Error (`cannot_parse_int (_, _))) -> true | _ -> false);

  test_line ["\"big\\tC style\""; "some%wrong";  "s"; "42"; "43"; "2."; "-"; "2"]
    (function | `output (Error (`wrong_url_escaping (_, "some%wrong"))) -> true
    | _ -> false);

  test_line ["\"big\\tC style\""; "some";  "s"; "42"; "43"; "2."; "-"; "2";
            "some string"]
    (function | `output (Error (`wrong_attributes (_, _))) -> true | _ -> false);

  test_line ["\"big\\tC style\""; "some";  "s"; "42"; "43"; "2."; "-"; "2";
            "some=string;djf"]
    (function | `output (Error (`wrong_attributes (_, _))) -> true | _ -> false);

  let transfo =
    let tags =
      { Gff.Tags.default with Gff.Tags.version = `two; allow_empty_lines = true} in
    Gff.Transform.string_to_item ~tags () in
  let test_line l f =
    let joined = (String.concat ~sep:"\t" l) in
    Tfxm.feed transfo (joined ^ "\n");
    assert_bool joined (f (Tfxm.next transfo))
  in
  let test_output l o = test_line l (fun oo -> oo = `output (Ok o)) in
  test_output ["# some comment"] (`comment " some comment");

  test_output [
    "\"big\\tC style\""; "some";  "s"; "42"; "43"; "2."; "+"; "2";
    "k v ; \"big\\tk\" \"annoying v\"   ; "
  ] (`record
        {Gff.seqname = "big\tC style"; source = Some "some"; feature = Some "s";
         pos = (42, 43); score = Some 2.; strand = `plus;
         phase = Some 2; attributes = ["k", ["v"]; "big\tk", ["annoying v"]]});
  ()

let test_printer () =
  let transfo = Gff.Transform.item_to_string ~tags:Gff.Tags.default () in
  let test s item =
    Tfxm.feed transfo item;
    let res =  Tfxm.next transfo in
    match res with
    | `output o ->
      if s <> o then eprintf "NOT EQUALS:\n%S\n%S\n%!" s o;
      assert_equal ~printer:ident s o
    | `not_ready -> assert_bool "not_ready" false
    | `end_of_stream -> assert_bool "end_of_stream" false
  in
  test "# some\n" (`comment " some");

  test "big%20spaced%20name\t.\t.\t42\t43\t2\t.\t.\t\n"
        (`record
            {Gff.seqname = "big spaced name"; source = None; feature = None;
             pos = (42, 43); score = Some 2.; strand = `not_applicable;
             phase = None; attributes = []});
  test "big%20spaced%20name\t%09\t.\t42\t43\t.\t+\t.\tk=v;big%20k=an%3Bno%09ing%0Av\n"
        (`record
            {Gff.seqname = "big spaced name"; source = Some "\t"; feature = None;
             pos = (42, 43); score = None; strand = `plus;
             phase = None; attributes = [
               "k", ["v"]; "big k", ["an;no\ting\nv"]
             ]});
  let transfo =
    let tags =
      { Gff.Tags.default with Gff.Tags.version = `two; allow_empty_lines = true} in
    Gff.Transform.item_to_string ~tags () in
  let test s item =
    Tfxm.feed transfo item;
    let res =  Tfxm.next transfo in
    match res with
    | `output o ->
      if s <> o then eprintf "NOT EQUALS (version 2):\n%S\n%S\n%!" s o;
      assert_equal ~printer:ident s o
    | `not_ready -> assert_bool "not_ready" false
    | `end_of_stream -> assert_bool "end_of_stream" false
  in
  test "\"big spaced name\"\t\"\\t\"\t.\t42\t43\t.\t+\t.\t\"k\" \"v\";\"big k\" \"an;no\\ting\\nv\"\n"
        (`record
            {Gff.seqname = "big spaced name"; source = Some "\t"; feature = None;
             pos = (42, 43); score = None; strand = `plus;
             phase = None; attributes = [
               "k", ["v"]; "big k", ["an;no\ting\nv"]
             ]});
  ()


let tests = "GFF" >::: [
  "Parse GFF" >:: test_parser;
  "Print GFF" >:: test_printer;
]
