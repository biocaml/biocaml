open Core_kernel
module Gff = Biocaml_unix.Gff
module Track = Biocaml_unix.Track
module Tfxm = Biocaml_unix.Tfxm
open OUnit

let test_parser () =
  let transfo = Track.Transform.string_to_string_content () in
  let test_line l f =
    Tfxm.feed transfo (l ^ "\n");
    assert_bool l (f (Tfxm.next transfo))
  in
  let test_output l o = test_line l (fun oo -> `output (Ok o) = oo) in

  test_output "# some comment" (`comment " some comment");


  test_output "browser  position   chro:42-51"
    (`browser (`position ("chro", 42, 51)));
  test_output "browser \t hide all" (`browser (`hide `all));
  test_output "browser some other command"
    (`browser (`unknown "browser some other command"));
  test_output "browser " (`browser (`unknown "browser "));
  test_output "browser" (`browser (`unknown "browser"));

  test_line "browser  position   chro:f42-51"  (function
  | `output (Error (`wrong_browser_position _)) -> true
  | _ -> false);

  test_output "track a=b c=d" (`track ["a", "b"; "c", "d"]);
  test_output "track   \t a=b \t   c=d \t" (`track ["a", "b"; "c", "d"]);
  test_output "track a=\"b b\" \"c\tc c\"=d"
    (`track ["a", "b b"; "c\tc c", "d"]);
  test_output "track" (`track []);
  test_output "track   \t" (`track []);

  test_line "track a=\"b b\" \"c\tc c\"=d  \t someguyalone" (function
  | `output (Error (`wrong_key_value_format _)) -> true
  | _ -> false);
  test_output "track a=\"b b\" \"c c\"="
    (`track ["a", "b b"; "c c", ""]);
  test_output "track a=\"b b\" \"c c\"=  o=c"
    (`track ["a", "b b"; "c c", ""; "o", "c"]);

  test_output "something else" (`content "something else");
  ()

let test_wig_parser () =
  let transfo = Track.Transform.string_to_wig () in
  let test_line l f =
    Tfxm.feed transfo (l ^ "\n");
    assert_bool l (f (Tfxm.next transfo))
  in
  let test_output l o = test_line l (fun oo -> `output (Ok o) = oo) in

  test_output "# some comment" (`comment " some comment");

  test_output "browser  position   chro:42-51"
    (`browser (`position ("chro", 42, 51)));

  test_output "variableStep chrom=chr19 span=150"
    (`variable_step_state_change ("chr19", Some 150));
  test_output "49304701 10.0" (`variable_step_value (49304701, 10.));
  test_output "49304901 12.5" (`variable_step_value (49304901, 12.5));
  ()

let test_gff_parser () =
  let transfo = Track.Transform.string_to_gff ~tags:Gff.Tags.default () in
  let test_line l f =
    Tfxm.feed transfo (l ^ "\n");
    assert_bool l (f (Tfxm.next transfo))
  in
  let test_output l o = test_line l (fun oo -> `output (Ok o) = oo) in

  test_output "# some comment" (`comment " some comment");
  test_output "track a=\"b b\" \"c c\"="
    (`track ["a", "b b"; "c c", ""]);

  test_output (String.concat ~sep:"\t" [
    "big%20spaced%20name"; ".";  "."; "42"; "43"; "2e12"; "."; "."; ""
  ])
    (`record
        {Gff.seqname = "big spaced name"; source = None; feature = None;
         pos = (42, 43); score = Some (2e12); strand = `not_applicable;
         phase = None; attributes = []});
  ()


let test_bed_parser () =
  let transfo =
    Track.Transform.string_to_bed
      ~more_columns:(`enforce [|`type_string; `type_int; `type_float|]) () in
  let test_line l f =
    Tfxm.feed transfo (l ^ "\n");
    assert_bool l (f (Tfxm.next transfo))
  in
  let test_output l o = test_line l (fun oo -> `output (Ok o) = oo) in

  test_output "# some comment" (`comment " some comment");
  test_output "track a=\"b b\" \"c c\"="
    (`track ["a", "b b"; "c c", ""]);
  test_output "chrA 42    45  some_string 42 3.14"
    (`content ("chrA", 42, 45, [| `string "some_string"; `int 42; `float 3.14 |]));
  test_output "chrB 100   130 some_string 42 3.14"
    (`content ("chrB", 100, 130, [| `string "some_string"; `int 42; `float 3.14 |]));
  ()

let test_printer () =
  let transfo = Track.Transform.string_content_to_string () in
  let test_line i l =
    Tfxm.feed transfo i;
    assert_bool l (Tfxm.next transfo = `output (l ^ "\n"))
  in
  test_line (`comment "foo") "#foo";
  test_line (`browser (`hide `all)) "browser hide all";
  test_line (`track ["a", "bb"; "some long", "one even longer"])
    "track a=bb \"some long\"=\"one even longer\"";
  test_line (`content "some content") "some content";
  ()

let test_wig_printer () =
  let transfo = Track.Transform.wig_to_string () in
  let test_line i l =
    Tfxm.feed transfo i;
    assert_bool l (Tfxm.next transfo = `output (l ^ "\n"))
  in
  test_line (`comment "foo") "#foo";
  test_line (`browser (`hide `all)) "browser hide all";
  test_line (`track ["a", "bb"; "some long", "one even longer"])
    "track a=bb \"some long\"=\"one even longer\"";
  test_line (`fixed_step_value 42.) "42";
  ()

let test_gff_printer () =
  let transfo = Track.Transform.gff_to_string ~tags:Gff.Tags.default () in
  let test_line i l =
    Tfxm.feed transfo i;
    assert_bool l (Tfxm.next transfo = `output (l ^ "\n"))
  in
  test_line (`comment "foo") "#foo";
  test_line (`browser (`hide `all)) "browser hide all";
  test_line (`track ["a", "bb"; "some long", "one even longer"])
    "track a=bb \"some long\"=\"one even longer\"";
  test_line
    (`record
        {Gff.seqname = "big spaced name"; source = None; feature = None;
         pos = (42, 43); score = Some (2.); strand = `not_applicable;
         phase = None; attributes = []})
    "big%20spaced%20name\t.\t.\t42\t43\t2\t.\t.\t";
  ()

let test_bed_printer () =
  let transfo = Track.Transform.bed_to_string () in
  let test_line i l =
    Tfxm.feed transfo i;
    assert_bool l (Tfxm.next transfo = `output (l ^ "\n"))
  in
  test_line (`comment "foo") "#foo";
  test_line (`track ["a", "bb"; "some long", "one even longer"])
    "track a=bb \"some long\"=\"one even longer\"";
  test_line (`content ("n", 0, 1, [|`float 3.14; `int 42|]))
    "n\t0\t1\t3.14\t42";
  ()



let tests = "Track" >::: [
  "Parse Track" >:: test_parser;
  "Parse WIG Track" >:: test_wig_parser;
  "Parse GFF Track" >:: test_gff_parser;
  "Parse BED Track" >:: test_bed_parser;
  "Print Track" >:: test_printer;
  "Print WIG Track" >:: test_wig_printer;
  "Print GFF Track" >:: test_gff_printer;
  "Print BED Track" >:: test_bed_printer;
]
