open Core_kernel
open OUnit
open Or_error.Monad_infix
module Sam = Biocaml_unix.Sam

let ( %> ) f g x = g (f x)


let test_parse_optional_field s v =
  let f = Sam.parse_optional_field s in
  assert_equal
    ~msg:"Optional field value (i type)"
    ~printer:((Or_error.sexp_of_t Sam.sexp_of_optional_field) %> Sexplib.Sexp.to_string_hum)
    f v

let test_parser () =
  test_parse_optional_field
    "YS:i:-1"
    (Sam.optional_field "YS" (Sam.optional_field_value_i (-1L)))

let tests = "SAM" >::: [
     "Parse SAM" >:: test_parser ;
  ]

(* module Sam = Biocaml_unix.Sam_deprecated *)
(* module Tfxm = Biocaml_unix.Tfxm *)


(* let test_parser_deprecated () = *)
(*   let transfo = Sam.Transform.string_to_raw () in *)
(*   let test_line l f = *)
(*     Tfxm.feed transfo (l ^ "\n"); *)
(*     assert_bool l (f (Tfxm.next transfo)) *)
(*   in *)
(*   let test_output l o = test_line l (fun oo -> `output (Ok o) = oo) in *)

(*   test_output "@CO\tsome comment" (`comment "some comment"); *)
(*   test_output "@HD\tVN:1.3\tSO:coordinate" *)
(*     (`header ("HD", [ "VN", "1.3"; "SO", "coordinate"])); *)
(*   test_output "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT\t*" *)
(*     (`alignment *)
(*         {Sam.qname = "r001"; flag = 83; rname = "ref"; pos = 37; mapq = 30; *)
(*          cigar = "9M"; rnext = "="; pnext = 7; tlen = -39; seq = "CAGCGCCAT"; *)
(*          qual = "*"; optional = []}); *)
(*   test_output "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT\t*\tNM:i:0" *)
(*     (`alignment *)
(*         {Sam.qname = "r001"; flag = 83; rname = "ref"; pos = 37; mapq = 30; *)
(*          cigar = "9M"; rnext = "="; pnext = 7; tlen = -39; seq = "CAGCGCCAT"; *)
(*          qual = "*"; optional = [("NM", 'i', "0")]}); *)
(*   test_output "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT\t*\tNM:i:0\tKJ:A:a" *)
(*     (`alignment *)
(*         {Sam.qname = "r001"; flag = 83; rname = "ref"; pos = 37; mapq = 30; *)
(*          cigar = "9M"; rnext = "="; pnext = 7; tlen = -39; seq = "CAGCGCCAT"; *)
(*          qual = "*"; optional = [("NM", 'i', "0"); ("KJ", 'A', "a")]}); *)
(*   test_line "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT\t*\tNM:i:0\tKJ:A" *)
(*     (function *)
(*     | `output (Error (`wrong_optional_field (_, _))) -> true *)
(*     | _ -> false); *)
(*   test_line "r001\t83\tref\t37\t30\t9M\t=\t7h\t-39\tCAGCGCCAT\t*\tNM:i:0\tKJ:A:a" *)
(*     (function *)
(*     | `output (Error (`not_an_int (_, "pnext", "7h")))  -> true *)
(*     | _ -> false); *)
(*   test_line  "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT" *)
(*     (function *)
(*     | `output (Error (`wrong_alignment (_, _))) -> true *)
(*     | _ -> false); *)
(*   test_line "@HD\tT" *)
(*     (function *)
(*     | `output (Error (`invalid_tag_value_list (_, ["T"]))) -> true *)
(*     | _ -> false); *)
(*   () *)

(* let test_item_parser_deprecated () = *)
(*   let c = ref 0 in *)
(*   let check t v f = *)
(*     let p = Sam.Transform.raw_to_string () in *)
(*     incr c; *)
(*     Tfxm.feed t v; *)
(*     let res = f (Tfxm.next t) in *)
(*     if not res then *)
(*       eprintf "Error on %s\n" *)
(*         Tfxm.( *)
(*           match feed p v; next p with `output o -> o | _ -> failwith "printer!" *)
(*         ); *)
(*     assert_bool (sprintf "test_item_parser.check %d" !c) res *)
(*   in *)
(*   let check_output t v o = check t v ((=) (`output (Ok o))) in *)

(*   let t = Sam.Transform.raw_to_item () in *)
(*   check_output t (`comment "comment") (`comment "comment"); *)
(*   check t (`header ("HD", ["VN", "42.1"])) (function *)
(*   | `output (Error (`header_line_not_first 2)) -> true *)
(*   | _ -> false); *)

(*   let t = Sam.Transform.raw_to_item () in *)
(*   check_output t (`header ("HD", ["VN", "42.1"])) *)
(*     (`header_line ("42.1", `unknown, [])); *)
(*   check t (`header ("SQ", [])) (function *)
(*   | `output (Error (`missing_ref_sequence_name [])) -> true *)
(*   | _ -> false); *)
(*   check t (`header ("SQ", ["SN", "chr0"])) (function *)
(*   | `output (Error (`missing_ref_sequence_length [("SN", "chr0")])) -> true *)
(*   | _ -> false); *)
(*   check t (`header ("SQ", ["SN", "chr0"; "LN", "not an int"])) (function *)
(*   | `output (Error (`wrong_ref_sequence_length _))  -> true *)
(*   | _ -> false); *)

(*   let t = Sam.Transform.raw_to_item () in *)
(*   check_output t (`header ("HD", ["VN", "42.1"; *)
(*                                   "SO", "coordinate"; "HP", "some other"])) *)
(*     (`header_line ("42.1", `coordinate, [("HP", "some other")])); *)
(*   check t (`header ("SQ", ["SN", "chr0"; "LN", "42"])) ((=) `not_ready); *)
(*   check t (`header ("SQ", ["SN", "chr1"; "LN", "42"; "M5", "abd34f90"])) *)
(*     ((=) `not_ready); *)
(*   (\* the ref-info is being buffered, the first alignment will output it *\) *)
(*   check_output t (`alignment *)
(*             {Sam.qname = "r001"; flag = 83; rname = "ref"; pos = 37; mapq = 30; *)
(*              cigar = "9M"; rnext = "="; pnext = 7; tlen = -39; seq = "CAGCGCCAT"; *)
(*              qual = "*"; optional = [("NM", 'i', "0")]}) *)
(*     (`reference_sequence_dictionary *)
(*         [| *)
(*           {Sam.ref_name = "chr0"; ref_length = 42; ref_assembly_identifier = None; *)
(*            ref_checksum = None; ref_species = None; ref_uri = None; *)
(*            ref_unknown = []}; *)
(*           {Sam.ref_name = "chr1"; ref_length = 42; ref_assembly_identifier = None; *)
(*            ref_checksum = Some "abd34f90"; ref_species = None; ref_uri = None; *)
(*            ref_unknown = []}; *)
(*         |]); *)
(*   (\* This one get the previous alignment: *\) *)
(*   check_output t *)
(*     (`alignment *)
(*         {Sam.qname = "chr0"; flag = 83; rname = "chr0"; pos = 37; mapq = 30; *)
(*          cigar = "9M"; rnext = "*"; pnext = 7; tlen = -39; seq = "CAGCGCCAT"; *)
(*          qual = "*"; optional = [("NM", 'i', "0")]}) *)
(*     (`alignment *)
(*         {Sam.query_template_name = "r001"; flags = Sam.Flags.of_int 83; *)
(*          reference_sequence = `name "ref"; position = Some 37; *)
(*          mapping_quality = Some 30; cigar_operations = [|`M 9|]; *)
(*          next_reference_sequence = `qname; next_position = Some 7; *)
(*          template_length = Some (-39); sequence = `string "CAGCGCCAT"; *)
(*          quality = [| |]; optional_content = [ "NM", 'i', `int 0] }); *)
(*   Tfxm.stop t; *)
(*   (\* We still have one to get: *\) *)
(*   assert_bool "last alignment" (Tfxm.next t = *)
(*       `output (Ok *)
(*                  (`alignment *)
(*                      {Sam.query_template_name = "chr0"; flags = Sam.Flags.of_int 83; *)
(*                       reference_sequence = *)
(*                          `reference_sequence *)
(*                            {Sam.ref_name = "chr0"; ref_length = 42; *)
(*                             ref_assembly_identifier = None; *)
(*                             ref_checksum = None; ref_species = None; ref_uri = None; *)
(*                             ref_unknown = []}; *)
(*                       position = Some 37; mapping_quality = Some 30; *)
(*                       cigar_operations = [|`M 9|]; next_reference_sequence = `none; *)
(*                       next_position = Some 7; template_length = Some (-39); *)
(*                       sequence = `string "CAGCGCCAT"; quality = [| |]; *)
(*                       optional_content = [ "NM", 'i', `int 0]}))); *)
(*   assert_bool "EOS" (Tfxm.next t = `end_of_stream); *)

(*
check (`header ("HD", ["VN", "42.1"; "SO", "coordinate"]));
check (`header ("HD", ["VN", "42.1"; "SO", "wut?"]));
check (`header ("HD", ["VN", "42.1"; "SO", "coordinate"; "HP", "some other"]));
check (`header ("HD", []));
check (`header ("SQ", []));
check (`header ("SQ", ["SN", "chr0"]));
check (`header ("SQ", ["SN", "chr0"; "LN", "not an int"]));
check (`header ("SQ", ["SN", "chr0"; "LN", "42"]));
check (`header ("SQ", ["SN", "chr1"; "LN", "42"; "M5", "abd34f90"]));
check (`header ("RR", ["SN", "chr1"; "LN", "42"; "M5", "abd34f90"]));
check (`alignment
          {qname = "r001"; flag = 83; rname = "ref"; pos = 37; mapq = 30;
           cigar = "9M"; rnext = "="; pnext = 7; tlen = -39; seq = "CAGCGCCAT";
           qual = "*"; optional = [("NM", 'i', "0")]});

check (`alignment
          {qname = "chr0"; flag = 83; rname = "chr0"; pos = 37; mapq = 30;
           cigar = "9M"; rnext = "*"; pnext = 7; tlen = -39; seq = "CAGCGCCAT";
           qual = "*"; optional = [("NM", 'i', "0")]});
 *)

(* let test_printer_deprecated () = *)
(*   let transfo = Sam.Transform.raw_to_string () in *)
(*   let test_line i l = *)
(*     Tfxm.feed transfo i; *)
(*     assert_bool l (Tfxm.next transfo = `output (l ^ "\n")) *)
(*   in *)

(*   test_line *)
(*     (`alignment *)
(*         {Sam.qname = "r001"; flag = 83; rname = "ref"; pos = 37; mapq = 30; *)
(*          cigar = "9M"; rnext = "="; pnext = 7; tlen = -39; seq = "CAGCGCCAT"; *)
(*          qual = "*"; optional = [("NM", 'i', "0")]}) *)
(*     "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT\t*\tNM:i:0"; *)

(*   test_line (`comment "some comment") "@CO\tsome comment" ; *)
(*   test_line (`header ("HD", [ "VN", "1.3"; "SO", "coordinate"])) *)
(*     "@HD\tVN:1.3\tSO:coordinate"; *)
(*   () *)


(* let tests = "SAM" >::: [ *)
(*     "Parse SAM" >:: test_parser ; *)
(*     "Parse SAM raw" >:: test_parser_deprecated; *)
(*     "Print SAM" >:: test_printer_deprecated; *)
(*     "Parse SAM item" >:: test_item_parser_deprecated; *)
(*   ] *)
