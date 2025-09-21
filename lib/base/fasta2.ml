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

  (* The action that needs to be taken by the parser:
     - Start_description: need to start parsing description
     - Continue_description: need to continue parsing description, carries the description so far
     - Start_sequence: need to start parsing sequence
     - Continue_sequence: need to continue parsing sequence
  *)
  type action =
    | Start_description
    | Continue_description of string
    | Start_sequence
    | Continue_sequence
  [@@deriving sexp]

  (* The parser's state is defined by:
     - action: the current action the parser needs to take
     - line_start: true if starting a new line
     - num_items: number of items parsed so far
     - line: number of the line currently being parsed
  *)
  type state =
    { action : action
    ; line_start : bool
    ; num_items : int
    ; line : int
    }
  [@@deriving sexp]

  let fail (line_num : int) msg = Error (`Fasta_parser_error (line_num, msg))

  let failf (line_num : int) fmt =
    let k x = fail line_num x in
    Printf.ksprintf k fmt
  ;;

  let init = { action = Start_description; line_start = true; num_items = 0; line = 1 }

  let step ({ action; line_start; num_items; line } : state) (buf : string) =
    let n = String.length buf in
    let rec loop
              ({ action; line_start; num_items; line } : state)
              (accu : Item.t list)
              (i : int)
              (j : int)
      =
      match j < n with
      | true -> (
        match action, line_start, buf.[j] with
        | Start_description, _, '>' ->
          loop
            { action = Continue_description ""; line_start = false; num_items; line }
            accu
            (j + 1)
            (j + 1)
        | Start_description, _, c -> failf line "Expected '>' but got %c" c
        | Continue_description d, _, '\n' -> (
          let d' = String.sub buf ~pos:i ~len:(j - i) in
          let description = d ^ d' in
          match description with
          | "" -> fail line "Description is empty"
          | _ ->
            let accu = `Description description :: accu in
            loop
              { action = Start_sequence; line_start = true; num_items; line = line + 1 }
              accu
              (j + 1)
              (j + 1))
        | Continue_description _, _, _ ->
          loop { action; line_start = false; num_items; line } accu i (j + 1)
        | Start_sequence, _, '>' -> fail line "Unexpected '>' at start of sequence"
        | Start_sequence, _, '\n' ->
          fail line "Unexpected empty line at start of sequence"
        | Start_sequence, _, _ ->
          loop
            { action = Continue_sequence; line_start = false; num_items; line }
            accu
            i
            (j + 1)
        | Continue_sequence, _, '\n' ->
          let sequence = String.sub buf ~pos:i ~len:(j - i) in
          loop
            { action = Continue_sequence
            ; line_start = true
            ; num_items = num_items + 1
            ; line = line + 1
            }
            (`Partial_sequence sequence :: accu)
            (j + 1)
            (j + 1)
        | Continue_sequence, false, '>' -> fail line "Unexpected '>' within sequence"
        | Continue_sequence, true, '>' ->
          (* Since [line_start] is true, the previous char was '\n'. Thus,
             prior sequence was already added to [accu]. *)
          loop
            { action = Continue_description ""; line_start; num_items; line }
            accu
            (j + 1)
            (j + 1)
        | Continue_sequence, _, _ ->
          loop { action; line_start; num_items; line } accu i (j + 1))
      | false -> (
        match action with
        | Start_description | Start_sequence ->
          (* Any prior window was already added to [accu]. *)
          Ok ({ action; line_start; num_items; line }, accu)
        | Continue_description d ->
          let d' = String.sub buf ~pos:i ~len:(j - i) in
          Ok
            ({ action = Continue_description (d ^ d'); line_start; num_items; line }, accu)
        | Continue_sequence ->
          let sequence = String.sub buf ~pos:i ~len:(j - i) in
          let accu = `Partial_sequence sequence :: accu in
          Ok ({ action = Continue_sequence; line_start; num_items; line }, accu))
    in
    loop { action; line_start; num_items; line } [] 0 0
    |> Result.map ~f:(fun (parser, res) -> parser, List.rev res)
  ;;

  let eof { action; line_start = _; num_items; line } =
    match action, num_items with
    | Start_description, 0 -> fail line "Empty file"
    | (Start_description | Continue_sequence), _ -> Ok ()
    | Start_sequence, _ | Continue_description _, _ ->
      fail line "Final line contains description without subsequent sequence"
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
  match Parser.step Parser.init content with
  | Error e -> Error e
  | Ok (parser, items) -> (
    match Parser.eof parser with
    | Error e -> Error e
    | Ok () -> Ok (of_parser_items items))
;;

module Test = struct
  open Expect_test_helpers_base

  let%expect_test "of_string on valid file contents" =
    let data =
      [ ">seq1\nACGT" (* single item *)
      ; ">seq1 description\nACGTACGT" (* description with spaces *)
      ; ">seq1\nACGT\n>seq2\nTGCA" (* multiple items *)
      ; ">seq1\nACGT\nTGCA" (* sequence on 2 lines *)
      ; ">seq1\nACGT\nTGCA\nGGG" (* sequence on 3 lines *)
      ; ">seq1\nACGT\nTGCA\n>seq2\nGGG\nCCC"
        (* multiple items with sequences on multiple lines *)
      ; ">seq1\nACGT\n" (* newline at end of file *)
      ]
    in
    let test x =
      let result = of_string x in
      match result with
      | Ok result ->
        print_string
          (sprintf
             "✅ SUCCESS - parsing passed\nINPUT: \n%s\n\nRESULT:\n%s\n"
             x
             (result |> sexp_of_t |> sexp_to_string))
      | Error e ->
        print_string
          (sprintf
             "❌ FIXME - should have passed but got error\nINPUT: \n%s\n\nRESULT:\n%s\n"
             x
             (e |> Error.sexp_of_t |> sexp_to_string))
    in
    List.iter data ~f:test;
    [%expect
      {|
      ✅ SUCCESS - parsing passed
      INPUT:
      >seq1
      ACGT

      RESULT:
      ((
        (description seq1)
        (sequence    ACGT)))

      ✅ SUCCESS - parsing passed
      INPUT:
      >seq1 description
      ACGTACGT

      RESULT:
      ((
        (description "seq1 description")
        (sequence    ACGTACGT)))

      ✅ SUCCESS - parsing passed
      INPUT:
      >seq1
      ACGT
      >seq2
      TGCA

      RESULT:
      (((description seq1) (sequence ACGT))
       ((description seq2) (sequence TGCA)))

      ✅ SUCCESS - parsing passed
      INPUT:
      >seq1
      ACGT
      TGCA

      RESULT:
      ((
        (description seq1)
        (sequence    ACGTTGCA)))

      ✅ SUCCESS - parsing passed
      INPUT:
      >seq1
      ACGT
      TGCA
      GGG

      RESULT:
      ((
        (description seq1)
        (sequence    ACGTTGCAGGG)))

      ✅ SUCCESS - parsing passed
      INPUT:
      >seq1
      ACGT
      TGCA
      >seq2
      GGG
      CCC

      RESULT:
      (((description seq1) (sequence ACGTTGCA))
       ((description seq2) (sequence GGGCCC)))

      ✅ SUCCESS - parsing passed
      INPUT:
      >seq1
      ACGT


      RESULT:
      ((
        (description seq1)
        (sequence    ACGT)))
      |}]
  ;;

  let%expect_test "of_string on invalid file contents" =
    let data =
      [ "", "empty file"
      ; ">\nACGT", "missing description"
      ; "ACGT", "missing description"
      ; "seq1\nACGT", "missing '>' at start of description"
      ; ">seq1\n\nACGT", "empty line at start of sequence"
      ; ">seq1\nGGG\n\nACGT", "empty line between sequence"
      ; ">seq1\nGGG\n\n", "exrtra empty line at end of file"
      ; ">seq1\n>", "'>' at start of sequence"
      ; ">seq1\nA>CGT", "'>' within sequence"
      ; ">seq1\nA>CGT\nGGG", "'>' in middle of sequence shouldn't start new description"
      ; ">seq1\nACGT\n>", "'>' at end of file"
      ; ">seq1\nACGT\n>seq2", "missing sequence at end of file"
      ]
    in
    let test (x, msg) =
      let result = of_string x in
      match result with
      | Error e ->
        print_string
          (sprintf
             "✅ SUCCESS - parsing failed as expected\n\
              TEST: %s\n\
              INPUT:\n\
              %s\n\n\
              RESULT:\n\
              %s\n"
             msg
             x
             (e |> Error.sexp_of_t |> sexp_to_string))
      | Ok result ->
        print_string
          (sprintf
             "❌ FIXME - should have failed but passed\n\
              TEST: %s\n\
              INPUT:\n\
              %s\n\n\
              RESULT:\n\
              %s\n"
             msg
             x
             (result |> sexp_of_t |> sexp_to_string))
    in
    List.iter data ~f:test;
    [%expect
      {|
      ✅ SUCCESS - parsing failed as expected
      TEST: empty file
      INPUT:


      RESULT:
      (Fasta_parser_error (1 "Empty file"))

      ✅ SUCCESS - parsing failed as expected
      TEST: missing description
      INPUT:
      >
      ACGT

      RESULT:
      (Fasta_parser_error (1 "Description is empty"))

      ✅ SUCCESS - parsing failed as expected
      TEST: missing description
      INPUT:
      ACGT

      RESULT:
      (Fasta_parser_error (1 "Expected '>' but got A"))

      ✅ SUCCESS - parsing failed as expected
      TEST: missing '>' at start of description
      INPUT:
      seq1
      ACGT

      RESULT:
      (Fasta_parser_error (1 "Expected '>' but got s"))

      ✅ SUCCESS - parsing failed as expected
      TEST: empty line at start of sequence
      INPUT:
      >seq1

      ACGT

      RESULT:
      (Fasta_parser_error (2 "Unexpected empty line at start of sequence"))

      ❌ FIXME - should have failed but passed
      TEST: empty line between sequence
      INPUT:
      >seq1
      GGG

      ACGT

      RESULT:
      ((
        (description seq1)
        (sequence    GGGACGT)))

      ❌ FIXME - should have failed but passed
      TEST: exrtra empty line at end of file
      INPUT:
      >seq1
      GGG



      RESULT:
      ((
        (description seq1)
        (sequence    GGG)))

      ✅ SUCCESS - parsing failed as expected
      TEST: '>' at start of sequence
      INPUT:
      >seq1
      >

      RESULT:
      (Fasta_parser_error (2 "Unexpected '>' at start of sequence"))

      ✅ SUCCESS - parsing failed as expected
      TEST: '>' within sequence
      INPUT:
      >seq1
      A>CGT

      RESULT:
      (Fasta_parser_error (2 "Unexpected '>' within sequence"))

      ✅ SUCCESS - parsing failed as expected
      TEST: '>' in middle of sequence shouldn't start new description
      INPUT:
      >seq1
      A>CGT
      GGG

      RESULT:
      (Fasta_parser_error (2 "Unexpected '>' within sequence"))

      ✅ SUCCESS - parsing failed as expected
      TEST: '>' at end of file
      INPUT:
      >seq1
      ACGT
      >

      RESULT:
      (Fasta_parser_error (
        3 "Final line contains description without subsequent sequence"))

      ✅ SUCCESS - parsing failed as expected
      TEST: missing sequence at end of file
      INPUT:
      >seq1
      ACGT
      >seq2

      RESULT:
      (Fasta_parser_error (
        3 "Final line contains description without subsequent sequence"))
      |}]
  ;;
end
