(* FIXME: max_line_length and alphabet format options are not implemented *)

type header = string list

type item =
  { description : string
  ; sequence : string
  }
[@@deriving sexp]

(* FIXME: should check there is no newline in the arguments *)
let item ~description ~sequence = { description; sequence }

type fmt =
  { allow_sharp_comments : bool
  ; allow_semicolon_comments : bool
  ; allow_empty_lines : bool
  ; max_line_length : int option
  ; alphabet : string option
  }

let fmt
  ?(allow_sharp_comments = true)
  ?(allow_semicolon_comments = false)
  ?(allow_empty_lines = false)
  ?max_line_length
  ?alphabet
  ()
  =
  { allow_sharp_comments
  ; allow_semicolon_comments
  ; allow_empty_lines
  ; max_line_length
  ; alphabet
  }
;;

let default_fmt = fmt ()

type item0 =
  [ `Comment of string
  | `Empty_line
  | `Description of string
  | `Partial_sequence of string
  ]
[@@deriving sexp]

let sequence_to_int_list s =
  try String.split s ~on:' ' |> List.map ~f:Int.of_string |> Result.return with
  | Failure msg -> Error (`Msg msg)
;;

type parser_error = [ `Fasta_parser_error of int * string ] [@@deriving sexp]

module Parser0 = struct
  type state =
    { fmt : fmt
    ; line : int
    ; line_start : bool
    ; started_first_item : bool
    ; symbol : symbol
    }

  and symbol =
    | S (* start of comment or description *)
    | Comment of string
    | Description of string
    | Sequence of { empty : bool }
    | Terminal

  let initial_state ?(fmt = default_fmt) () =
    { fmt; line = 0; line_start = true; started_first_item = false; symbol = S }
  ;;

  let fail st msg = Error (`Fasta_parser_error (st.line, msg))

  let failf st fmt =
    let k x = fail st x in
    Printf.ksprintf k fmt
  ;;

  let newline ?sym st =
    { st with
      line = st.line + 1
    ; line_start = true
    ; symbol =
        (match sym with
         | None -> st.symbol
         | Some s -> s)
    }
  ;;

  let step st = function
    | None -> (
      match st.symbol with
      | S -> Ok ({ st with symbol = Terminal }, [])
      | Comment c ->
        assert (not st.started_first_item);
        Ok ({ st with symbol = Terminal }, [ `Comment c ])
      | Description _ -> fail st "Missing sequence in last item"
      | Sequence { empty = true } -> fail st "Missing sequence in last item"
      | Sequence { empty = false } -> Ok ({ st with symbol = Terminal }, [])
      | Terminal -> Ok (st, []))
    | Some buf ->
      let allowed_comment_char c =
        let open Char in
        (c = '#' && st.fmt.allow_sharp_comments)
        || (c = ';' && st.fmt.allow_semicolon_comments)
      in
      let n = String.length buf in
      let rec loop st accu i j =
        if j < n
        then (
          match buf.[j], st.line_start, st.symbol with
          | _, _, Terminal -> Ok (st, [])
          | _, false, S -> assert false (* unreachable state *)
          | '>', true, S ->
            loop
              { st with
                line_start = false
              ; started_first_item = true
              ; symbol = Description ""
              }
              accu
              (j + 1)
              (j + 1)
          | _, true, Comment _ | _, true, Description _ ->
            assert false (* unreachable states *)
          | '>', true, Sequence { empty = true } ->
            fail st "Expected sequence, not description"
          | '>', true, Sequence { empty = false } ->
            loop
              { st with line_start = false; symbol = Description "" }
              accu
              (j + 1)
              (j + 1)
          | ((';' | '#') as c), true, S ->
            assert (i = j && i = 0);
            if allowed_comment_char c
            then
              loop { st with line_start = false; symbol = Comment "" } accu (i + 1) (j + 1)
            else failf st "Character %c not allowed for comments" c
          | (';' | '#'), true, Sequence _ -> fail st "Comment after first item"
          | '\n', true, (Sequence _ | S) ->
            if st.fmt.allow_empty_lines
            then loop (newline st) (`Empty_line :: accu) (j + 1) (j + 1)
            else fail st "Empty line"
          | c, true, S -> failf st "Unexpected character %c at beginning of line" c
          | '\n', false, Comment c ->
            let c' = String.sub buf ~pos:i ~len:(j - i) in
            loop (newline st ~sym:S) (`Comment (c ^ c') :: accu) (j + 1) (j + 1)
          | '\n', false, Description d ->
            let d' = String.sub buf ~pos:i ~len:(j - i) in
            loop
              (newline st ~sym:(Sequence { empty = true }))
              (`Description (d ^ d') :: accu)
              (j + 1)
              (j + 1)
          | '\n', false, Sequence _ ->
            let seq = String.sub buf ~pos:i ~len:(j - i) in
            loop
              (newline st ~sym:(Sequence { empty = false }))
              (`Partial_sequence seq :: accu)
              (j + 1)
              (j + 1)
          | _, false, (Comment _ | Description _ | Sequence { empty = false }) ->
            loop st accu i (j + 1)
          | _, false, Sequence { empty = true } -> assert false (* unreachable state *)
          | _, true, Sequence { empty = true } ->
            loop
              { st with line_start = false; symbol = Sequence { empty = false } }
              accu
              i
              (j + 1)
          | _, true, Sequence { empty = false } ->
            loop { st with line_start = false } accu i (j + 1))
        else (
          match st.symbol with
          | S | Terminal -> Ok (st, accu)
          | Comment c ->
            let c' = String.sub buf ~pos:i ~len:(j - i) in
            Ok ({ st with symbol = Comment (c ^ c') }, accu)
          | Description d ->
            let d' = String.sub buf ~pos:i ~len:(j - i) in
            Ok ({ st with symbol = Description (d ^ d') }, accu)
          | Sequence _ as sym ->
            let symbol, res =
              if i = j
              then sym, accu
              else (
                let seq = String.sub buf ~pos:i ~len:(j - i) in
                Sequence { empty = false }, `Partial_sequence seq :: accu)
            in
            Ok ({ st with symbol }, res))
      in
      loop st [] 0 0 |> Result.map ~f:(fun (st, res) -> st, List.rev res)
  ;;
end

let unparser0 = function
  | `Comment c -> "#" ^ c
  | `Empty_line -> ""
  | `Description d -> ">" ^ d
  | `Partial_sequence s -> s
;;

(* This could probably be optimized *)
let rev_concat xs = String.concat ~sep:"" (List.rev xs)

module Parser = struct
  type state =
    { state0 : Parser0.state
    ; symbol : symbol
    }

  and symbol =
    | Init
    | Item of string * string list
    | Terminal

  let initial_state ?fmt () = { state0 = Parser0.initial_state ?fmt (); symbol = Init }

  let step_aux (sym, accu) item0 =
    match item0, sym with
    | _, Terminal -> Terminal, accu
    | (`Comment _ | `Empty_line), _ -> sym, accu
    | `Description d, Init -> Item (d, []), accu
    | `Description _, Item (_, []) ->
      assert false (* should be detected by Parser0.step *)
    | `Description d', Item (d, xs) ->
      let item = item ~description:d ~sequence:(rev_concat xs) in
      Item (d', []), item :: accu
    | `Partial_sequence _, Init -> assert false (* should be detected by Parser0.step *)
    | `Partial_sequence s, Item (d, xs) -> Item (d, s :: xs), accu
  ;;

  let step_final input ((symbol, items) as res) =
    match input, symbol with
    | Some _, _ | None, (Init | Terminal) -> res
    | None, Item (d, xs) ->
      Terminal, item ~description:d ~sequence:(rev_concat xs) :: items
  ;;

  let step st input =
    Parser0.step st.state0 input
    |> Result.map ~f:(fun (state0, items0) ->
         let init = st.symbol, [] in
         let symbol, items =
           List.fold_left items0 ~init ~f:step_aux |> step_final input
         in
         { state0; symbol }, List.rev items)
  ;;
end

let unparser item = Printf.sprintf ">%s\n%s\n" item.description item.sequence

module Test = struct
  let fasta_of_strings ~initial_state ~step xs =
    let open Result.Monad_infix in
    List.fold
      xs
      ~init:(Ok (initial_state, []))
      ~f:(fun accu s ->
        match accu with
        | Ok (st, items) -> step st (Some s) >>| fun (st', items') -> st', items @ items'
        | e -> e)
    >>= fun (st, items) -> step st None >>| fun (_, final_items) -> items @ final_items
  ;;

  module Parser0 = struct
    type parsing_result = (item0 list, parser_error) Result.t [@@deriving sexp]

    let failure i msg = Error (`Fasta_parser_error (i, msg))

    let cases : (string list * parsing_result) list =
      [ [ ">A" ], failure 0 "Missing sequence in last item"
      ; [ "#A" ], Ok [ `Comment "A" ]
      ; [ ">A\nA" ], Ok [ `Description "A"; `Partial_sequence "A" ]
      ; [ ">"; "A\n"; "A" ], Ok [ `Description "A"; `Partial_sequence "A" ]
      ; ( [ ">"; "A\n"; "AD\nB" ]
        , Ok [ `Description "A"; `Partial_sequence "AD"; `Partial_sequence "B" ] )
      ; [ ">"; "A\n"; "AD\n#B" ], failure 2 "Comment after first item"
      ; ( [ "#ook"; "\n"; "\n"; ">"; "A\n"; "AD\nB" ]
        , Ok
            [ `Comment "ook"
            ; `Empty_line
            ; `Description "A"
            ; `Partial_sequence "AD"
            ; `Partial_sequence "B"
            ] )
      ]
    ;;

    let fasta_of_strings xs =
      let fmt = fmt ~allow_empty_lines:true () in
      let initial_state = Parser0.initial_state ~fmt () in
      fasta_of_strings xs ~initial_state ~step:Parser0.step
    ;;

    let%test _ =
      (* let printer xs = Sexp.to_string ([%sexp_of: parsing_result] xs) in *)
      cases
      |> List.for_all ~f:(fun (input, res) ->
           let parsed = fasta_of_strings input in
           Stdlib.( = ) res parsed)
    ;;
  end

  module Parser = struct
    type parsing_result = (item list, parser_error) Result.t [@@deriving sexp]

    let failure = Parser0.failure

    let cases =
      [ [ ">A" ], failure 0 "Missing sequence in last item"
      ; [ "#A" ], Ok []
      ; [ ">A\nA" ], Ok [ item ~description:"A" ~sequence:"A" ]
      ; [ ">"; "A\n"; "A" ], Ok [ item ~description:"A" ~sequence:"A" ]
      ; [ ">"; "A\n"; "AD\nB" ], Ok [ item ~description:"A" ~sequence:"ADB" ]
      ; [ ">"; "A\n"; "AD\n#B" ], failure 2 "Comment after first item"
      ; ( [ "#ook"; "\n"; "\n"; ">"; "A\n"; "AD\nB" ]
        , Ok [ item ~description:"A" ~sequence:"ADB" ] )
      ]
    ;;

    let fasta_of_strings xs =
      let fmt = fmt ~allow_empty_lines:true () in
      let initial_state = Parser.initial_state ~fmt () in
      fasta_of_strings xs ~initial_state ~step:Parser.step
    ;;

    let%test _ =
      (* let printer xs = Sexp.to_string ([%sexp_of: parsing_result] xs) in *)
      cases
      |> List.for_all ~f:(fun (input, res) ->
           let parsed = fasta_of_strings input in
           Stdlib.( = ) res parsed)
    ;;
  end
end
