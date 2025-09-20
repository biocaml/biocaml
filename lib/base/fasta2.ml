open! Import

module Error = struct
  type t = [ `Fasta_parser_error of int * string ] [@@deriving sexp]
end

module Parser = struct
  module Item = struct
    type t =
      [ `Description of string
      | `Partial_sequence of string
      ]
    [@@deriving sexp]
  end

  (* State describes what needs to be done on next step:
     - Description_start: need to start parsing description
     - Description: need to continue parsing description, carries the description so far
     - Sequence_start: need to start parsing sequence
     - Sequence: need to continue parsing sequence
  *)
  type state =
    | Description_start
    | Description of string
    | Sequence_start
    | Sequence
  [@@deriving sexp]

  (* Parser is defined by:
     - line: number of the line currently being parsed
     - state: the current state of the parser
  *)
  type t =
    { line : int
    ; state : state
    }
  [@@deriving sexp]

  let fail st msg = Error (`Fasta_parser_error (st.line, msg))

  let failf st fmt =
    let k x = fail st x in
    Printf.ksprintf k fmt
  ;;

  let init = { line = 1; state = Description_start }

  let step_some (parser : t) (buf : string) : (t * Item.t list, Error.t) Result.t =
    let n = String.length buf in
    let rec loop (parser : t) (accu : Item.t list) (i : int) (j : int) =
      match j < n with
      | true -> (
        match parser.state, buf.[j] with
        | Description_start, '>' ->
          loop { parser with state = Description "" } accu (j + 1) (j + 1)
        | Description_start, c -> failf parser "Expected '>' but got %c" c
        | Description d, '\n' ->
          let d' = String.sub buf ~pos:i ~len:(j - i) in
          let accu = `Description (d ^ d') :: accu in
          loop { state = Sequence_start; line = parser.line + 1 } accu (j + 1) (j + 1)
        | Description _, _ -> loop parser accu i (j + 1)
        | Sequence_start, '>' -> fail parser "Unexpected '>' at start of sequence"
        | Sequence_start, '\n' -> fail parser "Unexpected empty line at start of sequence"
        | Sequence_start, _ -> loop parser accu i (j + 1)
        | Sequence, '\n' ->
          let sequence = String.sub buf ~pos:i ~len:(j - i) in
          let accu = `Partial_sequence sequence :: accu in
          let parser = { state = Sequence; line = parser.line + 1 } in
          loop parser accu (j + 1) (j + 1)
        | Sequence, '>' ->
          let sequence = String.sub buf ~pos:i ~len:(j - i) in
          let accu = `Partial_sequence sequence :: accu in
          loop { parser with state = Description_start } accu (j + 1) (j + 1)
        | Sequence, _ ->
          (* TODO(ashish): If long sequence occurs on a single line, we may
             be on this state and still builing up the (i,j) window. Should
             return what we have so far if the window has gotten very large. *)
          loop parser accu i (j + 1))
      | false -> (
        match parser.state with
        | Description_start | Sequence_start ->
          (* Any prior window was already added to [accu]. *)
          Ok (parser, accu)
        | Description d ->
          let d' = String.sub buf ~pos:i ~len:(j - i) in
          Ok ({ parser with state = Description (d ^ d') }, accu)
        | Sequence ->
          let sequence = String.sub buf ~pos:i ~len:(j - i) in
          let accu = `Partial_sequence sequence :: accu in
          Ok ({ parser with state = Sequence }, accu))
    in
    loop parser [] 0 0 |> Result.map ~f:(fun (parser, res) -> parser, List.rev res)
  ;;

  let step_eof (parser : t) : (unit, Error.t) Result.t =
    match parser.state with
    | Description_start | Sequence_start -> Ok ()
    | Description _ -> fail parser "Input ended on description, missing sequence"
    | Sequence -> Ok ()
  ;;

  let step parser buf =
    match buf with
    | `Some buf -> step_some parser buf
    | `Eof -> (
      match step_eof parser with
      | Ok () -> Ok (parser, [])
      | Error e -> Error e)
  ;;
end

module Item = struct
  type t =
    { description : string
    ; sequence : string
    }
  [@@deriving sexp]
end

type t = Item.t list [@@deriving sexp]

(* We don't expose this function because it means you have all the parser
   items in memory. If you did that, probably should should have called
   [of_string] in the first place. Also we assume this function
   is called on the result of [Parser.step]. See comment within body. *)
let of_parser_items (items : Parser.Item.t list) : t =
  let rec loop accu items =
    match items with
    | `Description description :: items ->
      let sequences, items =
        List.split_while items ~f:(function
          | `Partial_sequence _ -> true
          | `Description _ -> false)
      in
      let sequences =
        sequences
        |> List.map ~f:(function
          | `Partial_sequence sequence -> sequence
          | `Description _ -> assert false)
      in
      let sequence = String.concat ~sep:"" sequences in
      loop ({ Item.description; sequence } :: accu) items
    | `Partial_sequence _ :: _ ->
      (* Either [Parser.step] has a bug because we only call this
         function internally by creating the original [items] from
         [Parser.step], or we have a bug above where we recursively
         call [loop]. *)
      failwith "BUG: Partial_sequence shouldn't be first in items"
    | [] -> List.rev accu
  in
  loop [] items
;;

let of_string content =
  match Parser.step Parser.init (`Some content) with
  | Ok (_, items) -> Ok (of_parser_items items)
  | Error e -> Error e
;;

module Test = struct
  open Expect_test_helpers_base

  let%expect_test "Parser.step single sequence" =
    let data =
      [ ">seq1\nACGT"
      ; ">seq1 description\nACGTACGT"
      ; ">seq1\nACGT\nTGCA"
      ; ">seq1\nACGT\n>seq2\nTGCA"
      ]
    in
    let test x =
      let result = of_string x in
      print_string
        (sprintf
           "IN: \n%s\n\nSEXP:\n%s\n"
           x
           ([%sexp_of: (t, Error.t) Result.t] result |> sexp_to_string))
    in
    List.iter data ~f:test;
    [%expect
      {|
      IN:
      >seq1
      ACGT

      SEXP:
      (Ok ((
        (description seq1)
        (sequence    ""))))

      IN:
      >seq1 description
      ACGTACGT

      SEXP:
      (Ok ((
        (description "seq1 description")
        (sequence    ""))))

      IN:
      >seq1
      ACGT
      TGCA

      SEXP:
      (Error (Fasta_parser_error (2 "Unexpected empty line at start of sequence")))

      IN:
      >seq1
      ACGT
      >seq2
      TGCA

      SEXP:
      (Error (Fasta_parser_error (2 "Unexpected empty line at start of sequence")))
      |}]
  ;;

  let%expect_test "Parser.step error cases" =
    let data = [ "ACGT"; "seq1\nACGT"; ">seq1\n\nACGT"; ">seq1\n>" ] in
    let test x =
      let result = of_string x in
      print_string
        (sprintf
           "IN: \"%s\"\nSEXP:\n%s\n"
           (String.escaped x)
           ([%sexp_of: (t, Error.t) Result.t] result |> sexp_to_string))
    in
    List.iter data ~f:test;
    [%expect
      {|
      IN: "ACGT"
      SEXP:
      (Error (Fasta_parser_error (1 "Expected '>' but got A")))

      IN: "seq1\nACGT"
      SEXP:
      (Error (Fasta_parser_error (1 "Expected '>' but got s")))

      IN: ">seq1\n\nACGT"
      SEXP:
      (Error (Fasta_parser_error (2 "Unexpected empty line at start of sequence")))

      IN: ">seq1\n>"
      SEXP:
      (Error (Fasta_parser_error (2 "Unexpected '>' at start of sequence")))
      |}]
  ;;

  let%expect_test "Parser.step multi-line sequences" =
    let data = [ ">seq1\nACGT\nTGCA\nAAA"; ">seq1\nACGT\nTGCA\n>seq2\nGGG\nCCC" ] in
    let test x =
      let result = of_string x in
      print_string
        (sprintf
           "IN: \"%s\"\nSEXP:\n%s\n"
           (String.escaped x)
           ([%sexp_of: (t, Error.t) Result.t] result |> sexp_to_string))
    in
    List.iter data ~f:test;
    [%expect
      {|
      IN: ">seq1\nACGT\nTGCA\nAAA"
      SEXP:
      (Error (Fasta_parser_error (2 "Unexpected empty line at start of sequence")))

      IN: ">seq1\nACGT\nTGCA\n>seq2\nGGG\nCCC"
      SEXP:
      (Error (Fasta_parser_error (2 "Unexpected empty line at start of sequence")))
      |}]
  ;;

  let%expect_test "Parser.step partial parsing" =
    let parser = Parser.init in
    let result1 = Parser.step parser (`Some ">seq1 human chromosome 1\n") in
    let () =
      print_string
        (sprintf
           "Step 1 - IN: \"%s\"\nSEXP:\n%s\n"
           (String.escaped ">seq1 human chromosome 1\n")
           ([%sexp_of: (Parser.t * Parser.Item.t list, Error.t) Result.t] result1
            |> sexp_to_string))
    in
    let () =
      match result1 with
      | Ok (parser2, _) ->
        let result2 = Parser.step parser2 (`Some "ACGTACGTACGT") in
        print_string
          (sprintf
             "Step 2 - IN: \"%s\"\nSEXP:\n%s\n"
             (String.escaped "ACGTACGTACGT")
             ([%sexp_of: (Parser.t * Parser.Item.t list, Error.t) Result.t] result2
              |> sexp_to_string))
      | Error _ -> print_string "Step 2 can't be called due to error in step 1\n"
    in
    ();
    [%expect
      {|
      Step 1 - IN: ">seq1 human chromosome 1\n"
      SEXP:
      (Ok (
        ((line  2)
         (state Sequence_start))
        ((Description "seq1 human chromosome 1"))))

      Step 2 - IN: "ACGTACGTACGT"
      SEXP:
      (Ok (
        ((line  2)
         (state Sequence_start))
        ()))
      |}]
  ;;
end
