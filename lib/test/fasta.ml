open Core_kernel.Std
open OUnit
open Rresult

module Fasta = Biocaml_base.Std.Fasta

type nonrec ('a, 'b) result = ('a, 'b) result =
  | Ok of 'a
  | Error of 'b
[@@ deriving sexp]

type parsing_result =
  (Fasta.item0 list, Fasta.Parser0.error) result
[@@ deriving sexp ]

let failure i msg =
  Error (`Fasta_parser0_error (i, msg))

let cases : (string list * parsing_result) list = [
  [ ">A" ], failure 0 "Missing sequence in last item" ;
  [ "#A" ], Ok [ `Comment "A" ] ;
  [ ">A\nA" ], Ok [ `Description "A" ; `Partial_sequence "A" ] ;
]

let fasta0_of_strings xs : parsing_result =
  let init = Ok (Fasta.Parser0.initial_state (), []) in
  List.fold xs ~init ~f:(fun accu s ->
      match accu with
      | Ok (st, items0) ->
        Fasta.Parser0.step st (Some s) >>| fun (st', items0') ->
        st', items0 @ items0'
      | e -> e
    )
  >>= fun (st, items0) ->
  Fasta.Parser0.step st None >>| fun (_,final_items0) ->
  items0 @ final_items0

let test_parser0 () =
  let printer xs =
    Sexp.to_string ([%sexp_of: parsing_result] xs)
  in
  List.iter cases ~f:(fun (input, res) ->
      let parsed = fasta0_of_strings input in
      assert_equal ~printer res parsed
    )


let tests = "Fasta" >::: [
  "parser0" >:: test_parser0
]
