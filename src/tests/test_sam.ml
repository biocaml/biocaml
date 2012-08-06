
open OUnit
open Core.Std


let test_parser () =
  let transfo = Biocaml_sam.parser () in
  let test_line l f =
    Biocaml_transform.feed transfo (l ^ "\n");
    assert_bool l (f (Biocaml_transform.next transfo))
  in
  let test_output l o = test_line l (fun oo -> `output o = oo) in
  let open Biocaml_sam in

  test_output "@CO\tsome comment" (`comment "some comment");
  test_output "@HD\tVN:1.3\tSO:coordinate"
    (`header_line ("HD", [ "VN", "1.3"; "SO", "coordinate"]));
  test_output "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT\t*"
    (`alignment
        {qname = "r001"; flag = 83; rname = "ref"; pos = 37; mapq = 30;
         cigar = "9M"; rnext = "="; pnext = 7; tlen = -39; seq = "CAGCGCCAT";
         qual = "*"; optional = []});
  test_output "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT\t*\tNM:i:0"
    (`alignment
        {qname = "r001"; flag = 83; rname = "ref"; pos = 37; mapq = 30;
         cigar = "9M"; rnext = "="; pnext = 7; tlen = -39; seq = "CAGCGCCAT";
         qual = "*"; optional = [("NM", 'i', "0")]});
  test_output "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT\t*\tNM:i:0\tKJ:A:a"
    (`alignment
        {qname = "r001"; flag = 83; rname = "ref"; pos = 37; mapq = 30;
         cigar = "9M"; rnext = "="; pnext = 7; tlen = -39; seq = "CAGCGCCAT";
         qual = "*"; optional = [("NM", 'i', "0"); ("KJ", 'A', "a")]});
  test_line "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT\t*\tNM:i:0\tKJ:A"
    (function
    | `error (`wrong_optional_field (_, _)) -> true
    | _ -> false);
  test_line "r001\t83\tref\t37\t30\t9M\t=\t7h\t-39\tCAGCGCCAT\t*\tNM:i:0\tKJ:A:a"
    (function
    | `error (`not_an_int (_, "pnext", "7h"))  -> true
    | _ -> false);
  test_line  "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT"
    (function
    | `error (`wrong_alignment (_, _)) -> true
    | _ -> false);
  test_line "@HD\tT"
    (function
    | `error (`invalid_tag_value_list (_, ["T"])) -> true
    | _ -> false);
  ()

let tests = "Track" >::: [
  "Parse SAM" >:: test_parser;
]
