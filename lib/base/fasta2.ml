module Parser = struct
  type item =
    [ `Description of string
    | `Sequence of string
    ]
  [@@deriving sexp]

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

  type error = [ `Fasta_parser_error of int * string ] [@@deriving sexp]

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

  let step (st : state) (buf : string option) : (state * item list, error) Result.t =
    match buf with
    | None -> (
      match st.symbol with
      | Start -> Ok ({ st with symbol = Terminal }, [])
      | Description _ -> fail st "Missing sequence in last item"
      | Sequence { empty = true } -> fail st "Missing sequence in last item"
      | Sequence { empty = false } -> Ok ({ st with symbol = Terminal }, [])
      | Terminal -> Ok (st, []))
    | Some buf ->
      let n = String.length buf in
      let rec loop (st : state) (accu : item list) (i : int) (j : int) =
        match j < n with
        | true -> (
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
              (`Sequence seq :: accu)
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
        | false -> (
          match st.symbol with
          | Start | Terminal -> Ok (st, accu)
          | Description d ->
            let d' = String.sub buf ~pos:i ~len:(j - i) in
            Ok ({ st with symbol = Description (d ^ d') }, accu)
          | Sequence _ as sym ->
            let symbol, res =
              match Int.equal i j with
              | true -> sym, accu
              | false ->
                let seq = String.sub buf ~pos:i ~len:(j - i) in
                Sequence { empty = false }, `Sequence seq :: accu
            in
            Ok ({ st with symbol }, res))
      in
      loop st [] 0 0 |> Result.map ~f:(fun (st, res) -> st, List.rev res)
  ;;
end
