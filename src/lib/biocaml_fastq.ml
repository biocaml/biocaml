open Core.Std
open Biocaml_internal_utils
module Lines = Biocaml_lines

type item = {
  name: string;
  sequence: string;
  comment: string;
  qualities: string;
} with sexp

let split_name s =
  match String.lsplit2 s ~on:' ' with
  | None -> s, None
  | Some (x,y) -> x, Some y


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
      <:sexp_of< Pos.t * string >>
  else
    Ok (String.sub line ~pos:1 ~len:(n-1))

let sequence_of_line ?(pos=Pos.unknown) line =
  (line : Line.t :> string)

let comment_of_line ?(pos=Pos.unknown) line =
  let line = (line : Line.t :> string) in
  let n = String.length line in
  if n = 0 || line.[0] <> '+' then
    error
      "invalid comment"
      (pos, line)
      <:sexp_of< Pos.t * string >>
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
        <:sexp_of< Pos.t * string * string >>
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
include MakeIO(Future_std)
