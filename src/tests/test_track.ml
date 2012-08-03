
open OUnit
open Core.Std


let test_parser () =
  let transfo = Biocaml_track.parser () in
  let test_line l f =
    Biocaml_transform.feed transfo (l ^ "\n");
    assert_bool l (f (Biocaml_transform.next transfo))
  in
  let test_output l o = test_line l (fun oo -> `output o = oo) in

  test_output "# some comment" (`comment " some comment");
  

  test_output "browser  position   chro:42-51" 
    (`browser (`position ("chro", 42, 51)));
  test_output "browser \t hide all" (`browser (`hide `all));
  test_output "browser some other command"
    (`browser (`unknown "browser some other command"));
  test_output "browser " (`browser (`unknown "browser "));
  test_output "browser" (`browser (`unknown "browser"));

  test_line "browser  position   chro:f42-51"  (function
  | `error (`wrong_browser_position _) -> true
  | _ -> false);
  
  test_output "track a=b c=d" (`track ["a", "b"; "c", "d"]);
  test_output "track   \t a=b \t   c=d \t" (`track ["a", "b"; "c", "d"]);
  test_output "track a=\"b b\" \"c\tc c\"=d"
    (`track ["a", "b b"; "c\tc c", "d"]);
  test_output "track" (`track []);
  test_output "track   \t" (`track []);

  test_line "track a=\"b b\" \"c\tc c\"=d  \t someguyalone" (function
  | `error (`wrong_key_value_format _) -> true
  | _ -> false);
  test_output "track a=\"b b\" \"c c\"="
    (`track ["a", "b b"; "c c", ""]);
  test_output "track a=\"b b\" \"c c\"=  o=c"
    (`track ["a", "b b"; "c c", ""; "o", "c"]);

  test_output "something else" (`content "something else");
  ()

let tests = "Track" >::: [
  "Parse Track" >:: test_parser;
]
