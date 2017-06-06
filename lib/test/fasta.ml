open Core_kernel
open OUnit
open Rresult

module Fasta = Biocaml_base.Fasta

type ('a, 'b) result = ('a, 'b) Pervasives.result =
  | Ok of 'a
  | Error of 'b
[@@ deriving sexp]

let fasta_of_strings ~initial_state ~step xs =
  List.fold xs ~init:(Ok (initial_state, [])) ~f:(fun accu s ->
      match accu with
      | Ok (st, items) ->
        step st (Some s) >>| fun (st', items') ->
        st', items @ items'
      | e -> e
    )
  >>= fun (st, items) ->
  step st None >>| fun (_,final_items) ->
  items @ final_items

module Parser0 = struct
  type parsing_result =
    (Fasta.item0 list, Fasta.parser_error) result
  [@@ deriving sexp ]

  let failure i msg =
    Error (`Fasta_parser_error (i, msg))

  let cases : (string list * parsing_result) list = [
    [ ">A" ], failure 0 "Missing sequence in last item" ;
    [ "#A" ], Ok [ `Comment "A" ] ;
    [ ">A\nA" ], Ok [ `Description "A" ; `Partial_sequence "A" ] ;
    [ ">" ; "A\n" ; "A" ], Ok [ `Description "A" ; `Partial_sequence "A" ] ;
    [ ">" ; "A\n" ; "AD\nB" ], Ok [ `Description "A" ;
                                    `Partial_sequence "AD" ;
                                    `Partial_sequence "B" ; ] ;
    [ ">" ; "A\n" ; "AD\n#B" ], failure 2 "Comment after first item" ;
    [ "#ook" ; "\n" ; "\n" ;">" ; "A\n" ; "AD\nB" ],
    Ok [ `Comment "ook" ;
         `Empty_line ;
         `Description "A" ;
         `Partial_sequence "AD" ;
         `Partial_sequence "B" ; ] ;
  ]

  let fasta_of_strings xs =
    let fmt = Fasta.fmt ~allow_empty_lines:true () in
    let initial_state = Fasta.Parser0.initial_state ~fmt () in
    fasta_of_strings xs ~initial_state ~step:Fasta.Parser0.step

  let test () =
    let printer xs =
      Sexp.to_string ([%sexp_of: parsing_result] xs)
    in
    List.iter cases ~f:(fun (input, res) ->
        let parsed = fasta_of_strings input in
        assert_equal ~printer res parsed
      )
end


module Parser = struct
  type parsing_result =
    (Fasta.item list, Fasta.parser_error) result
  [@@ deriving sexp ]

  let failure = Parser0.failure

  let cases = [
    [ ">A" ], failure 0 "Missing sequence in last item" ;

    [ "#A" ], Ok [] ;

    [ ">A\nA" ], Ok [ Fasta.item ~description:"A" ~sequence:"A" ] ;

    [ ">" ; "A\n" ; "A" ], Ok [ Fasta.item ~description:"A" ~sequence:"A" ] ;

    [ ">" ; "A\n" ; "AD\nB" ], Ok [ Fasta.item ~description:"A" ~sequence:"ADB" ] ;

    [ ">" ; "A\n" ; "AD\n#B" ], failure 2 "Comment after first item" ;

    [ "#ook" ; "\n" ; "\n" ;">" ; "A\n" ; "AD\nB" ],
    Ok [ Fasta.item ~description:"A" ~sequence:"ADB" ] ;
  ]

  let fasta_of_strings xs =
    let fmt = Fasta.fmt ~allow_empty_lines:true () in
    let initial_state = Fasta.Parser.initial_state ~fmt () in
    fasta_of_strings xs ~initial_state ~step:Fasta.Parser.step

  let test () =
    let printer xs =
      Sexp.to_string ([%sexp_of: parsing_result] xs)
    in
    List.iter cases ~f:(fun (input, res) ->
        let parsed = fasta_of_strings input in
        assert_equal ~printer res parsed
      )
end


let tests = "Fasta" >::: [
  "parser0" >:: Parser0.test ;
  "parser"  >:: Parser.test ;
]
