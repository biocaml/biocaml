open Core_kernel

type item = {
  name: string;
  sequence: string;
  comment: string;
  qualities: string;
} [@@deriving sexp]

let split_name s =
  match String.lsplit2 s ~on:' ' with
  | None -> s, None
  | Some (x,y) -> x, Some y


(******************************************************************************)
(* Illumina-specific formats                                                  *)
(******************************************************************************)
module Illumina = struct
  type surface = [`Top | `Bottom]
  type tile = {surface:surface; swath:int; number:int}

  let tile_of_string s =
    let open Result.Monad_infix in
    if String.length s <> 4
      || not (String.for_all s ~f:Char.is_digit)
    then
      error "invalid tile" s sexp_of_string
    else (
      (match s.[0] with
      | '1' -> Ok `Top
      | '2' -> Ok `Bottom
      | x -> error "invalid surface" x sexp_of_char
      ) >>= fun surface ->

      (match s.[1] with
      | '1' -> Ok 1
      | '2' -> Ok 2
      | '3' -> Ok 3
      | x -> error "invalid swath" x sexp_of_char
      ) >>= fun swath ->

      (
        String.(sub s ~pos:2 ~len:(length s - 2))
        |> fun x -> (
          try Ok (Int.of_string x)
          with Failure _ -> error "tile number not an int" s sexp_of_string
        )
        |> function
          | Error _ as e -> e
          | Ok x ->
            if x <= 0
            then error "invalid tile number" x sexp_of_int
            else Ok x
      ) >>= fun number ->

      Ok {surface; swath; number}
    )

  let tile_to_string t =
    sprintf "%c%d%02d"
      (match t.surface with `Top -> '1' | `Bottom -> '2')
      t.swath
      t.number

  type sequence_id = {
    instrument : string;
    run_number : int;
    flowcell_id : string;
    lane : int;
    tile : tile;
    x_pos : int;
    y_pos : int;
    read : int;
    is_filtered : bool;
    control_number : int;
    index : string
  }

  let sequence_id_of_string s =
    let open Result.Monad_infix in
    let i name value =
      try Ok (Int.of_string value)
      with Failure _ ->
        error (sprintf "%s not an int" name) value sexp_of_string
    in
    let b name value = match value with
      | "Y" -> Ok true
      | "N" -> Ok false
      | _ -> error (sprintf "%s must be Y or N" name) value sexp_of_string
    in
    match String.lsplit2 s ~on:' ' with
    | Some (x,y) -> (
      match String.split x ~on:':', String.split y ~on:':' with
      | [instrument;run_number;flowcell_id;lane;tile;x_pos;y_pos],
        [read;is_filtered;control_number;index] ->
        i "run_number" run_number >>= fun run_number ->
        i "lane" lane >>= fun lane ->
        tile_of_string tile >>= fun tile ->
        i "x_pos" x_pos >>= fun x_pos ->
        i "y_pos" y_pos >>= fun y_pos ->
        i "read" read >>= fun read ->
        b "is_filtered" is_filtered >>= fun is_filtered ->
        i "control_number" control_number >>= fun control_number ->
        Ok {instrument; run_number; flowcell_id; lane; tile; x_pos; y_pos;
            read; is_filtered; control_number; index}
      | _ -> error "invalid Illumina sequence identifier" s sexp_of_string
    )
    | _ -> error "invalid Illumina sequence identifier" s sexp_of_string

end


(******************************************************************************)
(* Printing                                                                   *)
(******************************************************************************)
let item_to_string r =
  sprintf "@%s\n%s\n+%s\n%s\n" r.name r.sequence r.comment r.qualities


(******************************************************************************)
(* Parsing                                                                    *)
(******************************************************************************)
let name_of_line ?(pos=Pos.unknown) line =
  let line = (line : Line.t :> string) in
  let n = String.length line in
  if n = 0 || line.[0] <> '@' then
    error
      "invalid name"
      (pos, line)
      [%sexp_of: Pos.t * string ]
  else
    Ok (String.sub line ~pos:1 ~len:(n-1))

let sequence_of_line ?pos:_ line =
  (line : Line.t :> string)

let comment_of_line ?(pos=Pos.unknown) line =
  let line = (line : Line.t :> string) in
  let n = String.length line in
  if n = 0 || line.[0] <> '+' then
    error
      "invalid comment"
      (pos, line)
      [%sexp_of: Pos.t * string ]
  else
    Ok (String.sub line ~pos:1 ~len:(n-1))

let qualities_of_line ?(pos=Pos.unknown) ?sequence line =
  let line = (line : Line.t :> string) in
  match sequence with
  | None -> Ok line
  | Some sequence ->
    let m = String.length sequence in
    let n = String.length line in
    if m <> n then
      error
        "length of sequence and qualities differ"
        (pos, sequence, line)
        [%sexp_of: Pos.t * string * string ]
    else
      Ok line


(******************************************************************************)
(* Input/Output                                                               *)
(******************************************************************************)
module MakeIO (Future : Future.S) = struct
  open Future

  let read_item ic : item Or_error.t Reader.Read_result.t Deferred.t =
    Reader.read_line ic >>= function
    | `Eof -> return `Eof
    | `Ok line ->
      match name_of_line (Line.of_string_unsafe line) with
      | Error _ as e -> return (`Ok e)
      | Ok name ->
        Reader.read_line ic >>= function
        | `Eof ->
          return (`Ok (Error (Error.of_string "incomplete input")))
        | `Ok line ->
          let sequence = sequence_of_line (Line.of_string_unsafe line) in
          Reader.read_line ic >>= function
          | `Eof ->
            return (`Ok (Error (Error.of_string "incomplete input")))
          | `Ok line ->
            match comment_of_line (Line.of_string_unsafe line) with
            | Error _ as e -> return (`Ok e)
            | Ok comment ->
              Reader.read_line ic >>= function
              | `Eof ->
                return (`Ok (Error (Error.of_string "incomplete input")))
              | `Ok line ->
                match
                  qualities_of_line ~sequence (Line.of_string_unsafe line)
                with
                | Error _ as e -> return (`Ok e)
                | Ok qualities ->
                  return (`Ok (Ok {name; sequence; comment; qualities}))

  let read ic =
    Reader.read_all ic read_item

  let write_item (w : Writer.t) (x : item) : unit Deferred.t =
    let open Writer in
    write_char w '@' >>= fun () ->
    write_line w x.name >>= fun () ->
    write_line w x.sequence >>= fun () ->
    write_char w '+' >>= fun () ->
    write_line w x.comment >>= fun () ->
    write_line w x.qualities

  let write w pipe_r =
    Pipe.iter pipe_r ~f:(write_item w)

  let write_file ?perm ?append file pipe_r =
    Writer.with_file ?perm ?append file ~f:(fun w -> write w pipe_r)

end
include MakeIO(Future_unix)
