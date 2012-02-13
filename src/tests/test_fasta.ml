open OUnit
open Biocaml_fasta
open Batteries

let test_read_fasta () = 
  print_endline (Sys.getcwd ()) ;
  let ic = open_in "src/tests/test.fa" in
  let fa = enum_input ic |> snd |> List.of_enum in
  assert_bool "Number of sequences" (List.length fa = 2) ;
  assert_bool "Sequence length" (List.map (snd |- String.length) fa = [ 42 ; 50 ])

let tests = "Fasta" >::: [
  "Reading FASTA" >:: test_read_fasta;
]
