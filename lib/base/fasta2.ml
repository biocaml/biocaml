module Parser = struct
  module Item = struct
    type t =
      [ `Description of string
      | `Sequence of string
      ]
    [@@deriving sexp]
  end

  type state =
    | Start (* start of description *)
    | Description of string
    | Sequence of { empty : bool }
    | Terminal

  type t =
    { line : int
    ; line_start : bool
    ; started_first_item : bool
    ; state : state
    }

  module Error = struct
    type t = [ `Fasta_parser_error of int * string ] [@@deriving sexp]
  end

  let fail st msg = Error (`Fasta_parser_error (st.line, msg))

  let failf st fmt =
    let k x = fail st x in
    Printf.ksprintf k fmt
  ;;

  let init = { line = 0; line_start = true; started_first_item = false; state = Start }

  let newline ?state t =
    { t with
      line = t.line + 1
    ; line_start = true
    ; state =
        (match state with
         | None -> t.state
         | Some s -> s)
    }
  ;;

  let step (parser : t) (buf : string option) : (t * Item.t list, Error.t) Result.t =
    match buf with
    | None -> (
      match parser.state with
      | Start -> Ok ({ parser with state = Terminal }, [])
      | Description _ -> fail parser "Missing sequence in last item"
      | Sequence { empty = true } -> fail parser "Missing sequence in last item"
      | Sequence { empty = false } -> Ok ({ parser with state = Terminal }, [])
      | Terminal -> Ok (parser, []))
    | Some buf ->
      let n = String.length buf in
      let rec loop (parser : t) (accu : Item.t list) (i : int) (j : int) =
        match j < n with
        | true -> (
          match buf.[j], parser.line_start, parser.state with
          | _, _, Terminal -> Ok (parser, [])
          | _, false, Start -> assert false (* unreachable state *)
          | '>', true, Start ->
            loop
              { parser with
                line_start = false
              ; started_first_item = true
              ; state = Description ""
              }
              accu
              (j + 1)
              (j + 1)
          | _, true, Description _ -> assert false (* unreachable states *)
          | '>', true, Sequence { empty = true } ->
            fail parser "Expected sequence, not description"
          | '>', true, Sequence { empty = false } ->
            loop
              { parser with line_start = false; state = Description "" }
              accu
              (j + 1)
              (j + 1)
          | '\n', true, (Sequence _ | Start) -> fail parser "Empty line"
          | c, true, Start ->
            failf parser "Unexpected character %c at beginning of line" c
          | '\n', false, Description d ->
            let d' = String.sub buf ~pos:i ~len:(j - i) in
            loop
              (newline parser ~state:(Sequence { empty = true }))
              (`Description (d ^ d') :: accu)
              (j + 1)
              (j + 1)
          | '\n', false, Sequence _ ->
            let seq = String.sub buf ~pos:i ~len:(j - i) in
            loop
              (newline parser ~state:(Sequence { empty = false }))
              (`Sequence seq :: accu)
              (j + 1)
              (j + 1)
          | _, false, (Description _ | Sequence { empty = false }) ->
            loop parser accu i (j + 1)
          | _, false, Sequence { empty = true } -> assert false (* unreachable state *)
          | _, true, Sequence { empty = true } ->
            loop
              { parser with line_start = false; state = Sequence { empty = false } }
              accu
              i
              (j + 1)
          | _, true, Sequence { empty = false } ->
            loop { parser with line_start = false } accu i (j + 1))
        | false -> (
          match parser.state with
          | Start | Terminal -> Ok (parser, accu)
          | Description d ->
            let d' = String.sub buf ~pos:i ~len:(j - i) in
            Ok ({ parser with state = Description (d ^ d') }, accu)
          | Sequence _ as state ->
            let state, res =
              match Int.equal i j with
              | true -> state, accu
              | false ->
                let seq = String.sub buf ~pos:i ~len:(j - i) in
                Sequence { empty = false }, `Sequence seq :: accu
            in
            Ok ({ parser with state }, res))
      in
      loop parser [] 0 0 |> Result.map ~f:(fun (parser, res) -> parser, List.rev res)
  ;;
end
