type item =
  { description : string
  ; sequence : string
  }
[@@deriving sexp]

(* FIXME: should check there is no newline in the arguments *)
let item ~description ~sequence = { description; sequence }

type item0 =
  [ `Description of string
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
    { line : int
    ; line_start : bool
    ; started_first_item : bool
    ; symbol : symbol
    }

  and symbol =
    | Start (* start of description *)
    | Description of string
    | Sequence of { empty : bool }
    | Terminal

  let initial_state () =
    { line = 0; line_start = true; started_first_item = false; symbol = Start }
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
      | Start -> Ok ({ st with symbol = Terminal }, [])
      | Description _ -> fail st "Missing sequence in last item"
      | Sequence { empty = true } -> fail st "Missing sequence in last item"
      | Sequence { empty = false } -> Ok ({ st with symbol = Terminal }, [])
      | Terminal -> Ok (st, []))
    | Some buf ->
      let n = String.length buf in
      let rec loop st accu i j =
        if j < n
        then (
          match buf.[j], st.line_start, st.symbol with
          | _, _, Terminal -> Ok (st, [])
          | _, false, Start -> assert false (* unreachable state *)
          | '>', true, Start ->
            loop
              { st with
                line_start = false
              ; started_first_item = true
              ; symbol = Description ""
              }
              accu
              (j + 1)
              (j + 1)
          | _, true, Description _ -> assert false (* unreachable states *)
          | '>', true, Sequence { empty = true } ->
            fail st "Expected sequence, not description"
          | '>', true, Sequence { empty = false } ->
            loop
              { st with line_start = false; symbol = Description "" }
              accu
              (j + 1)
              (j + 1)
          | '\n', true, (Sequence _ | Start) -> fail st "Empty line"
          | c, true, Start -> failf st "Unexpected character %c at beginning of line" c
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
          | _, false, (Description _ | Sequence { empty = false }) ->
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
          | Start | Terminal -> Ok (st, accu)
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

  let initial_state () = { state0 = Parser0.initial_state (); symbol = Init }

  let step_aux (sym, accu) item0 =
    match item0, sym with
    | _, Terminal -> Terminal, accu
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
      let symbol, items = List.fold_left items0 ~init ~f:step_aux |> step_final input in
      { state0; symbol }, List.rev items)
  ;;
end

let unparser item = Printf.sprintf ">%s\n%s\n" item.description item.sequence
