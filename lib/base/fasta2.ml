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
     - state: the current state of the parser
     - line: number of the line currently being parsed
     - line_start: true if starting a new line
  *)
  type t =
    { state : state
    ; line_start : bool
    ; line : int
    }
  [@@deriving sexp]

  let fail (line_num : int) msg = Error (`Fasta_parser_error (line_num, msg))

  let failf (line_num : int) fmt =
    let k x = fail line_num x in
    Printf.ksprintf k fmt
  ;;

  let init = { state = Description_start; line_start = true; line = 1 }

  let step ({ state; line_start; line } : t) (buf : [ `Some of string | `Eof ]) =
    match buf with
    | `Eof -> (
      match state with
      | Description_start | Sequence -> Ok ({ state; line_start; line }, [])
      | Sequence_start | Description _ ->
        fail line "Final line contains description without subsequent sequence")
    | `Some buf ->
      let n = String.length buf in
      let rec loop
                ({ state; line_start; line } : t)
                (accu : Item.t list)
                (i : int)
                (j : int)
        =
        match j < n with
        | true -> (
          match state, line_start, buf.[j] with
          | Description_start, _, '>' ->
            loop { state = Description ""; line_start = false; line } accu (j + 1) (j + 1)
          | Description_start, _, c -> failf line "Expected '>' but got %c" c
          | Description d, _, '\n' -> (
            let d' = String.sub buf ~pos:i ~len:(j - i) in
            let description = d ^ d' in
            match description with
            | "" -> fail line "Description is empty"
            | _ ->
              let accu = `Description description :: accu in
              loop
                { state = Sequence_start; line_start = true; line = line + 1 }
                accu
                (j + 1)
                (j + 1))
          | Description _, _, _ -> loop { state; line_start = false; line } accu i (j + 1)
          | Sequence_start, _, '>' -> fail line "Unexpected '>' at start of sequence"
          | Sequence_start, _, '\n' ->
            fail line "Unexpected empty line at start of sequence"
          | Sequence_start, _, _ ->
            loop { state = Sequence; line_start; line } accu i (j + 1)
          | Sequence, _, '\n' ->
            let sequence = String.sub buf ~pos:i ~len:(j - i) in
            loop
              { state = Sequence; line_start = true; line = line + 1 }
              (`Partial_sequence sequence :: accu)
              (j + 1)
              (j + 1)
          | Sequence, false, '>' -> loop { state; line_start; line } accu i (j + 1)
          | Sequence, true, '>' ->
            (* Since [line_start] is true, the previous char was '\n'. Thus,
               prior sequence was already added to [accu]. *)
            loop { state = Description ""; line_start; line } accu (j + 1) (j + 1)
          | Sequence, _, _ -> loop { state; line_start; line } accu i (j + 1))
        | false -> (
          match state with
          | Description_start | Sequence_start ->
            (* Any prior window was already added to [accu]. *)
            Ok ({ state; line_start; line }, accu)
          | Description d ->
            let d' = String.sub buf ~pos:i ~len:(j - i) in
            Ok ({ state = Description (d ^ d'); line_start; line }, accu)
          | Sequence ->
            let sequence = String.sub buf ~pos:i ~len:(j - i) in
            let accu = `Partial_sequence sequence :: accu in
            Ok ({ state = Sequence; line_start; line }, accu))
      in
      loop { state; line_start; line } [] 0 0
      |> Result.map ~f:(fun (parser, res) -> parser, List.rev res)
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
      [ "" (* empty file *)
      ; ">\nACGT" (* missing description *)
      ; "ACGT" (* missing description *)
      ; "seq1\nACGT" (* missing '>' at start of description *)
      ; ">seq1\n\nACGT" (* empty line at start of sequence *)
      ; ">seq1\n>" (* '>' at start of sequence *)
      ; ">seq1\nA>CGT" (* '>' within sequence *)
      ; ">seq1\nACGT\n>" (* '>' at end of file *)
      ; ">seq1\nACGT\n>seq2" (* missing sequence at end of file *)
      ]
    in
    let test x =
      let result = of_string x in
      match result with
      | Error e ->
        print_string
          (sprintf
             "✅ SUCCESS - parsing failed as expected\nINPUT:\n%s\n\nRESULT:\n%s\n"
             x
             (e |> Error.sexp_of_t |> sexp_to_string))
      | Ok result ->
        print_string
          (sprintf
             "❌ FIXME - should have failed but passed\nINPUT:\n%s\n\nRESULT:\n%s\n"
             x
             (result |> sexp_of_t |> sexp_to_string))
    in
    List.iter data ~f:test;
    [%expect
      {|
      ❌ FIXME - should have failed but passed
      INPUT:


      RESULT:
      ()

      ✅ SUCCESS - parsing failed as expected
      INPUT:
      >
      ACGT

      RESULT:
      (Fasta_parser_error (1 "Description is empty"))

      ✅ SUCCESS - parsing failed as expected
      INPUT:
      ACGT

      RESULT:
      (Fasta_parser_error (1 "Expected '>' but got A"))

      ✅ SUCCESS - parsing failed as expected
      INPUT:
      seq1
      ACGT

      RESULT:
      (Fasta_parser_error (1 "Expected '>' but got s"))

      ✅ SUCCESS - parsing failed as expected
      INPUT:
      >seq1

      ACGT

      RESULT:
      (Fasta_parser_error (2 "Unexpected empty line at start of sequence"))

      ✅ SUCCESS - parsing failed as expected
      INPUT:
      >seq1
      >

      RESULT:
      (Fasta_parser_error (2 "Unexpected '>' at start of sequence"))

      ❌ FIXME - should have failed but passed
      INPUT:
      >seq1
      A>CGT

      RESULT:
      ((
        (description seq1)
        (sequence    "")))

      ❌ FIXME - should have failed but passed
      INPUT:
      >seq1
      ACGT
      >

      RESULT:
      ((
        (description seq1)
        (sequence    ACGT)))

      ❌ FIXME - should have failed but passed
      INPUT:
      >seq1
      ACGT
      >seq2

      RESULT:
      ((
        (description seq1)
        (sequence    ACGT)))
      |}]
  ;;
end
