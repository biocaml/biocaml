module Parser = struct
  module Item = struct
    type t =
      [ `Description of string
      | `Sequence of string
      ]
    [@@deriving sexp]
  end

  (* State describes what needs to be done on next step:
     - Description_start: need to start parsing description
     - Description: need to continue parsing description
     - Sequence_start: need to start parsing sequence
     - Sequence: need to continue parsing sequence
  *)
  type state =
    | Description_start
    | Description
    | Sequence_start
    | Sequence

  (* Parser is defined by:
     - line: number of the line that will be parsed on next step
     - state: the current state of the parser
  *)
  type t =
    { line : int
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

  let init = { line = 1; state = Description_start }

  let step (parser : t) (buf : string) : (t * Item.t list, Error.t) Result.t =
    let n = String.length buf in
    let rec loop (parser : t) (accu : Item.t list) (i : int) (j : int) =
      match j < n with
      | true -> (
        match parser.state, buf.[j] with
        | Description_start, '>' ->
          loop { parser with state = Description } accu (j + 1) (j + 1)
        | Description_start, c -> failf parser "Expected '>' but got %c" c
        | Description, '\n' ->
          let description = String.sub buf ~pos:i ~len:(j - i) in
          let accu = `Description description :: accu in
          loop { parser with state = Sequence_start } accu (j + 1) (j + 1)
        | Description, _ -> loop parser accu i (j + 1)
        | Sequence_start, '>' -> fail parser "Unexpected '>' at start of sequence"
        | Sequence_start, '\n' -> fail parser "Unexpected empty line at start of sequence"
        | Sequence_start, _ -> loop parser accu i (j + 1)
        | Sequence, '\n' ->
          let sequence = String.sub buf ~pos:i ~len:(j - i) in
          let accu = `Sequence sequence :: accu in
          let parser = { parser with state = Sequence } in
          loop parser accu (j + 1) (j + 1)
        | Sequence, '>' ->
          let sequence = String.sub buf ~pos:i ~len:(j - i) in
          let accu = `Sequence sequence :: accu in
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
        | Description ->
          let description = String.sub buf ~pos:i ~len:(j - i) in
          let accu = `Description description :: accu in
          Ok ({ parser with state = Description_start }, accu)
        | Sequence ->
          let sequence = String.sub buf ~pos:i ~len:(j - i) in
          let accu = `Sequence sequence :: accu in
          Ok ({ parser with state = Sequence_start }, accu))
    in
    loop parser [] 0 0 |> Result.map ~f:(fun (parser, res) -> parser, List.rev res)
  ;;
end
