open! Import

type error = Parse_error of int * string [@@deriving sexp]

let parse_error (line_num : int) msg = Error (Parse_error (line_num, msg))

let parse_errorf (line_num : int) fmt =
  let k x = parse_error line_num x in
  Printf.ksprintf k fmt
;;

module Parser0 = struct
  module Item = struct
    type t =
      [ `Description of string
      | `Partial_sequence of string
      ]
    [@@deriving sexp]

    let to_string = function
      | `Description description -> description ^ "\n"
      | `Partial_sequence sequence -> sequence ^ "\n"
    ;;
  end

  (* The parser's state is defined by:
     - parse: function to parse the buffer with arguments:
       * state: current parser state
       * ~buf: string buffer to parse
       * ~start: start index of the current window being accumulated
       * ~pos: current parsing position in the buffer
       * ~len: total length of the buffer
       * ~accum: list of items that have been parsed (in reverse order)
     - eof: function to validate the final state at EOF
     - line_start: true if starting a new line
     - num_items: number of items parsed so far
     - line_num: number of the line currently being parsed
     - description_buffer: accumulated description (only used in continue_description state)
  *)
  type state =
    { parse :
        state
        -> buf:string
        -> start:int
        -> pos:int
        -> len:int
        -> accum:Item.t list
        -> (state * Item.t list, error) Result.t
    ; eof : state -> (unit, error) Result.t
    ; line_start : bool
    ; num_items : int
    ; line_num : int
    ; description_buffer : string
    }

  let rec start_description state ~buf ~start:_ ~pos ~len ~accum =
    match pos < len with
    | true -> (
      match state.line_start, buf.[pos] with
      | false, _ -> failwith "BUG: Start_description with line_start = false"
      | true, '>' ->
        let new_state =
          { state with
            parse = continue_description
          ; eof = continue_description_eof
          ; line_start = false
          ; description_buffer = ""
          }
        in
        continue_description new_state ~buf ~start:(pos + 1) ~pos:(pos + 1) ~len ~accum
      | true, c -> parse_errorf state.line_num "Expected '>' but got %c" c)
    | false ->
      (* End of buffer *)
      Ok (state, accum)

  and start_description_eof state =
    match state.num_items with
    | 0 -> parse_error state.line_num "Empty file"
    | _ -> Ok ()

  and continue_description state ~buf ~start ~pos ~len ~accum =
    match pos < len with
    | true -> (
      match state.line_start, buf.[pos] with
      | _, '>' -> parse_error state.line_num "Unexpected '>' within description"
      | true, _ -> failwith "BUG: Continue_description with line_start = true"
      | false, '\n' -> (
        let d' = String.sub buf ~pos:start ~len:(pos - start) in
        let description = state.description_buffer ^ d' in
        match description with
        | "" -> parse_error state.line_num "Description is empty"
        | _ ->
          let new_accum = `Description description :: accum in
          let new_state =
            { parse = start_sequence
            ; eof = start_sequence_eof
            ; line_start = true
            ; num_items = state.num_items + 1
            ; line_num = state.line_num + 1
            ; description_buffer = ""
            }
          in
          start_sequence
            new_state
            ~buf
            ~start:(pos + 1)
            ~pos:(pos + 1)
            ~len
            ~accum:new_accum)
      | false, _ -> continue_description state ~buf ~start ~pos:(pos + 1) ~len ~accum)
    | false ->
      (* End of buffer - accumulate partial description *)
      let d' = String.sub buf ~pos:start ~len:(pos - start) in
      let new_state = { state with description_buffer = state.description_buffer ^ d' } in
      Ok (new_state, accum)

  and continue_description_eof state =
    parse_error
      state.line_num
      "Final line contains description without subsequent sequence"

  and start_sequence state ~buf ~start ~pos ~len ~accum =
    match pos < len with
    | true -> (
      match state.line_start, buf.[pos] with
      | false, _ ->
        Error
          (Parse_error (state.line_num, "BUG: Start_sequence with line_start = false"))
      | true, '>' -> parse_error state.line_num "Unexpected '>' at start of sequence"
      | true, '\n' ->
        parse_error state.line_num "Unexpected empty line at start of sequence"
      | true, _ ->
        let new_state =
          { state with
            parse = continue_sequence
          ; eof = continue_sequence_eof
          ; line_start = false
          }
        in
        continue_sequence new_state ~buf ~start ~pos:(pos + 1) ~len ~accum)
    | false ->
      (* End of buffer *)
      Ok (state, accum)

  and start_sequence_eof state =
    parse_error
      state.line_num
      "Final line contains description without subsequent sequence"

  and continue_sequence state ~buf ~start ~pos ~len ~accum =
    match pos < len with
    | true -> (
      match state.line_start, buf.[pos] with
      | true, '\n' -> parse_error state.line_num "Unexpected empty line within sequence"
      | false, '\n' ->
        let sequence = String.sub buf ~pos:start ~len:(pos - start) in
        let new_accum = `Partial_sequence sequence :: accum in
        let new_state =
          { parse = continue_sequence
          ; eof = continue_sequence_eof
          ; line_start = true
          ; num_items = state.num_items + 1
          ; line_num = state.line_num + 1
          ; description_buffer = state.description_buffer
          }
        in
        continue_sequence
          new_state
          ~buf
          ~start:(pos + 1)
          ~pos:(pos + 1)
          ~len
          ~accum:new_accum
      | false, '>' -> parse_error state.line_num "Unexpected '>' within sequence"
      | false, '\r' -> parse_error state.line_num "Unexpected '\r' within sequence"
      | true, '>' ->
        (* Since line_start is true, the previous char was '\n'. Thus,
           prior sequence was already added to accum. *)
        let new_state =
          { state with
            parse = continue_description
          ; eof = continue_description_eof
          ; line_start = false
          ; description_buffer = ""
          }
        in
        continue_description new_state ~buf ~start:(pos + 1) ~pos:(pos + 1) ~len ~accum
      | _, _ ->
        let new_state = { state with line_start = false } in
        continue_sequence new_state ~buf ~start ~pos:(pos + 1) ~len ~accum)
    | false ->
      (* End of buffer - emit partial sequence *)
      let sequence = String.sub buf ~pos:start ~len:(pos - start) in
      let new_accum = `Partial_sequence sequence :: accum in
      Ok (state, new_accum)

  and continue_sequence_eof _state = Ok ()

  let init =
    { parse = start_description
    ; eof = start_description_eof
    ; line_start = true
    ; num_items = 0
    ; line_num = 1
    ; description_buffer = ""
    }
  ;;

  let step state (buf : string) =
    let len = String.length buf in
    match state.parse state ~buf ~start:0 ~pos:0 ~len ~accum:[] with
    | Error _ as e -> e
    | Ok (state, items) -> Ok (state, List.rev items)
  ;;

  let eof state = state.eof state
end

module Item = struct
  type t =
    { description : string
    ; sequence : string
    }
  [@@deriving sexp]

  let to_string ?(max_line_length = 70) { description; sequence } =
    let max_line_length =
      match max_line_length <= 0 with
      | true -> 70
      | false -> max_line_length
    in
    let description = ">" ^ description in
    (* Add empty string to the beginning, which becomes last after List.rev,
       to get a final newline in String.concat below. *)
    let sequence =
      "" :: String2.chunks_of_rev ~max_length:max_line_length sequence |> List.rev
    in
    description :: sequence |> String.concat ~sep:"\n"
  ;;
end

module Parser = struct
  module Item = Item

  type state =
    { parser0_state : Parser0.state
    ; parser0_items : Parser0.Item.t list
    }

  let init = { parser0_state = Parser0.init; parser0_items = [] }

  let get_initial_seq_items (item0s : Parser0.Item.t list)
    : string list * Parser0.Item.t list
    =
    let rec loop accu item0s =
      match item0s with
      | [] | `Description _ :: _ -> List.rev accu, item0s
      | `Partial_sequence seq :: rest -> loop (seq :: accu) rest
    in
    loop [] item0s
  ;;

  let get_complete_items (item0s : Parser0.Item.t list)
    : Item.t list * Parser0.Item.t list
    =
    let rec loop accu item0s =
      match item0s with
      | [] | `Partial_sequence _ :: _ -> accu, item0s
      | `Description description :: rest -> (
        let sequences, remaining = get_initial_seq_items rest in
        match remaining with
        | [] ->
          (* If no items remaining, we don't know if the sequence is complete. *)
          accu, item0s
        | `Partial_sequence _ :: _ ->
          failwith "BUG: get_initial_seq_items failed to consume Partial_sequence"
        | `Description _ :: _ ->
          (* If the first remaining item is a description, we know the sequence is complete. *)
          let sequence = String.concat ~sep:"" sequences in
          loop ({ Item.description; sequence } :: accu) remaining)
    in
    let accu, item0s = loop [] item0s in
    List.rev accu, item0s
  ;;

  let step ({ parser0_state; parser0_items } : state) (buf : string)
    : (state * Item.t list, error) Result.t
    =
    match Parser0.step parser0_state buf with
    | Error _ as e -> e
    | Ok (parser0_state, new_items) ->
      let parser0_items = parser0_items @ new_items in
      let completed, parser0_items = get_complete_items parser0_items in
      let state = { parser0_state; parser0_items } in
      Ok (state, completed)
  ;;

  let get_complete_items_at_eof (item0s : Parser0.Item.t list) : Item.t list =
    let rec loop accu item0s =
      match item0s with
      | [] -> accu
      | `Partial_sequence _ :: _ ->
        failwith
          "BUG: get_complete_items_at_eof found Partial_sequence without Description"
      | `Description description :: rest ->
        let sequences, remaining = get_initial_seq_items rest in
        let sequence = String.concat ~sep:"" sequences in
        loop ({ Item.description; sequence } :: accu) remaining
    in
    loop [] item0s |> List.rev
  ;;

  let eof ({ parser0_state; parser0_items } : state) : (Item.t list, error) Result.t =
    match Parser0.eof parser0_state with
    | Error e -> Error e
    | Ok () -> (
      match get_complete_items parser0_items with
      | items, [] -> Ok items
      | items, remaining_items ->
        (* At EOF, any remaining items should be convertible to complete items *)
        let final_items = get_complete_items_at_eof remaining_items in
        Ok (items @ final_items))
  ;;
end

type t = Item.t list [@@deriving sexp]

let of_string content =
  match Parser.step Parser.init content with
  | Error _ as e -> e
  | Ok (parser, items) -> (
    match Parser.eof parser with
    | Error _ as e -> e
    | Ok final_items -> Ok (items @ final_items))
;;

module Test = struct
  open Expect_test_helpers_base

  (* Samples of valid FASTA file contents.*)
  let valid_contents =
    [ ">seq1\nACGT", "single item"
    ; ">seq1 description\nACGTACGT", "description with spaces"
    ; ">seq1\nACGT\n>seq2\nTGCA", "multiple items"
    ; ">seq1\nACGT\nTGCA", "sequence on 2 lines"
    ; ">seq1\nACGT\nTGCA\nGGG", "sequence on 3 lines"
    ; ( ">seq1\nACGT\nTGCA\n>seq2\nGGG\nCCC"
      , "multiple items with sequences on multiple lines" )
    ; ">seq1\nACGT\n", "newline at end of file"
    ]
  ;;

  let invalid_contents =
    [ "", "empty file"
    ; ">\nACGT", "missing description"
    ; "ACGT", "missing description"
    ; "seq1\nACGT", "missing '>' at start of description"
    ; ">seq1>abc\nACGT", "'>' within description"
    ; ">seq1\n\nACGT", "empty line at start of sequence"
    ; ">seq1\nGGG\n\nACGT", "empty line between sequence"
    ; ">seq1\nGGG\n\n", "extra empty line at end of file"
    ; ">seq1\n>", "'>' at start of sequence"
    ; ">seq1\nA>CGT", "'>' within sequence"
    ; ">seq1\nA>CGT\nGGG", "'>' in middle of sequence shouldn't start new description"
    ; ">seq1\nACGT\n>", "'>' at end of file"
    ; ">seq1\nACGT\n>seq2", "missing sequence at end of file"
    ; ">seq1\nACGT\r\nGGG\n", "'\r\n' line ending"
    ; ">seq1\nACGT\nGGG\r", "final '\r'"
    ]
  ;;

  let%expect_test "of_string on valid file contents" =
    let test (x, msg) =
      let result = of_string x in
      match result with
      | Ok result ->
        print_string
          (sprintf
             "✅ SUCCESS - parsing passed\nTEST: %s\nINPUT: \n%s\n\nRESULT:\n%s\n"
             msg
             x
             (result |> sexp_of_t |> sexp_to_string))
      | Error e ->
        print_string
          (sprintf
             "❌ FIXME - should have passed but got error\n\
              TEST: %s\n\
              INPUT: \n\
              %s\n\n\
              RESULT:\n\
              %s\n"
             msg
             x
             (e |> sexp_of_error |> sexp_to_string))
    in
    List.iter valid_contents ~f:test;
    [%expect
      {|
      ✅ SUCCESS - parsing passed
      TEST: single item
      INPUT:
      >seq1
      ACGT

      RESULT:
      ((
        (description seq1)
        (sequence    ACGT)))

      ✅ SUCCESS - parsing passed
      TEST: description with spaces
      INPUT:
      >seq1 description
      ACGTACGT

      RESULT:
      ((
        (description "seq1 description")
        (sequence    ACGTACGT)))

      ✅ SUCCESS - parsing passed
      TEST: multiple items
      INPUT:
      >seq1
      ACGT
      >seq2
      TGCA

      RESULT:
      (((description seq1) (sequence ACGT))
       ((description seq2) (sequence TGCA)))

      ✅ SUCCESS - parsing passed
      TEST: sequence on 2 lines
      INPUT:
      >seq1
      ACGT
      TGCA

      RESULT:
      ((
        (description seq1)
        (sequence    ACGTTGCA)))

      ✅ SUCCESS - parsing passed
      TEST: sequence on 3 lines
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
      TEST: multiple items with sequences on multiple lines
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
      TEST: newline at end of file
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
             (String.escaped msg)
             x
             (e |> sexp_of_error |> sexp_to_string))
      | Ok result ->
        print_string
          (sprintf
             "❌ FIXME - should have failed but passed\n\
              TEST: %s\n\
              INPUT:\n\
              %s\n\n\
              RESULT:\n\
              %s\n"
             (String.escaped msg)
             x
             (result |> sexp_of_t |> sexp_to_string))
    in
    List.iter invalid_contents ~f:test;
    [%expect
      {|
      ✅ SUCCESS - parsing failed as expected
      TEST: empty file
      INPUT:


      RESULT:
      (Parse_error 1 "Empty file")

      ✅ SUCCESS - parsing failed as expected
      TEST: missing description
      INPUT:
      >
      ACGT

      RESULT:
      (Parse_error 1 "Description is empty")

      ✅ SUCCESS - parsing failed as expected
      TEST: missing description
      INPUT:
      ACGT

      RESULT:
      (Parse_error 1 "Expected '>' but got A")

      ✅ SUCCESS - parsing failed as expected
      TEST: missing '>' at start of description
      INPUT:
      seq1
      ACGT

      RESULT:
      (Parse_error 1 "Expected '>' but got s")

      ✅ SUCCESS - parsing failed as expected
      TEST: '>' within description
      INPUT:
      >seq1>abc
      ACGT

      RESULT:
      (Parse_error 1 "Unexpected '>' within description")

      ✅ SUCCESS - parsing failed as expected
      TEST: empty line at start of sequence
      INPUT:
      >seq1

      ACGT

      RESULT:
      (Parse_error 2 "Unexpected empty line at start of sequence")

      ✅ SUCCESS - parsing failed as expected
      TEST: empty line between sequence
      INPUT:
      >seq1
      GGG

      ACGT

      RESULT:
      (Parse_error 3 "Unexpected empty line within sequence")

      ✅ SUCCESS - parsing failed as expected
      TEST: extra empty line at end of file
      INPUT:
      >seq1
      GGG



      RESULT:
      (Parse_error 3 "Unexpected empty line within sequence")

      ✅ SUCCESS - parsing failed as expected
      TEST: '>' at start of sequence
      INPUT:
      >seq1
      >

      RESULT:
      (Parse_error 2 "Unexpected '>' at start of sequence")

      ✅ SUCCESS - parsing failed as expected
      TEST: '>' within sequence
      INPUT:
      >seq1
      A>CGT

      RESULT:
      (Parse_error 2 "Unexpected '>' within sequence")

      ✅ SUCCESS - parsing failed as expected
      TEST: '>' in middle of sequence shouldn't start new description
      INPUT:
      >seq1
      A>CGT
      GGG

      RESULT:
      (Parse_error 2 "Unexpected '>' within sequence")

      ✅ SUCCESS - parsing failed as expected
      TEST: '>' at end of file
      INPUT:
      >seq1
      ACGT
      >

      RESULT:
      (Parse_error 3 "Final line contains description without subsequent sequence")

      ✅ SUCCESS - parsing failed as expected
      TEST: missing sequence at end of file
      INPUT:
      >seq1
      ACGT
      >seq2

      RESULT:
      (Parse_error 3 "Final line contains description without subsequent sequence")

      ✅ SUCCESS - parsing failed as expected
      TEST: '\r\n' line ending
      INPUT:
      >seq1
      ACGT
      GGG


      RESULT:
      (Parse_error 2 "Unexpected '\r' within sequence")

      ✅ SUCCESS - parsing failed as expected
      TEST: final '\r'
      INPUT:
      >seq1
      ACGT
      GGG

      RESULT:
      (Parse_error 3 "Unexpected '\r' within sequence")
      |}]
  ;;

  let%expect_test "to_string on valid file contents" =
    let test (x, msg) =
      match of_string x with
      | Error _ -> failwith (sprintf "of_string tested above, should succeed here")
      | Ok parsed ->
        let result =
          parsed
          |> List.map ~f:(Item.to_string ~max_line_length:2)
          |> String.concat ~sep:""
        in
        print_string
          (sprintf "✅ SUCCESS\nTEST: %s\nINPUT: \n%s\n\nRESULT:\n%s\n" msg x result)
    in
    List.iter valid_contents ~f:test;
    [%expect
      {|
      ✅ SUCCESS
      TEST: single item
      INPUT:
      >seq1
      ACGT

      RESULT:
      >seq1
      AC
      GT

      ✅ SUCCESS
      TEST: description with spaces
      INPUT:
      >seq1 description
      ACGTACGT

      RESULT:
      >seq1 description
      AC
      GT
      AC
      GT

      ✅ SUCCESS
      TEST: multiple items
      INPUT:
      >seq1
      ACGT
      >seq2
      TGCA

      RESULT:
      >seq1
      AC
      GT
      >seq2
      TG
      CA

      ✅ SUCCESS
      TEST: sequence on 2 lines
      INPUT:
      >seq1
      ACGT
      TGCA

      RESULT:
      >seq1
      AC
      GT
      TG
      CA

      ✅ SUCCESS
      TEST: sequence on 3 lines
      INPUT:
      >seq1
      ACGT
      TGCA
      GGG

      RESULT:
      >seq1
      AC
      GT
      TG
      CA
      GG
      G

      ✅ SUCCESS
      TEST: multiple items with sequences on multiple lines
      INPUT:
      >seq1
      ACGT
      TGCA
      >seq2
      GGG
      CCC

      RESULT:
      >seq1
      AC
      GT
      TG
      CA
      >seq2
      GG
      GC
      CC

      ✅ SUCCESS
      TEST: newline at end of file
      INPUT:
      >seq1
      ACGT


      RESULT:
      >seq1
      AC
      GT
      |}]
  ;;
end
