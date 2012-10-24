open Biocaml_internal_pervasives

exception Error of string

type t = int
with sexp

let round_float_to_int x =
  if Float.mod_float x 1.0 < 0.5
  then Int.of_float (Float.round_down x)
  else Int.of_float (Float.round_up x)

let to_int t = t

let to_probability t =
  10.0 ** (Float.of_int t /. -10.0)

let int_of_offset = function `offset33 -> 33 | `offset64 -> 64

let to_ascii ?(offset=`offset33) t =
  let offset = int_of_offset offset in
  let x = t + offset in
  if offset <= x && x <= 126 then
    Some (Char.of_int_exn x)
  else
    None

let to_ascii_exn ?(offset=`offset33) t =
  match to_ascii ~offset t with
  | Some s -> s
  | None ->
    Error (sprintf "%d with offset %d cannot be encoded as a visible \
                    ASCII character" t (int_of_offset offset)) |! raise

let of_int_exn x =
  if x >= 0 then x
  else Error (sprintf "invalid PHRED score %d" x) |! raise

let of_ascii ?(offset=`offset33) x =
  let offset = int_of_offset offset in
  let c = Char.to_int x in
  if offset <= c && c <= 126 then
    Some (c - offset)
  else
    None

let of_ascii_exn ?(offset=`offset33) x =
  match of_ascii ~offset x with
  | Some s -> s
  | None ->
    Error (sprintf "%c with offset %d is not a valid score" x
             (int_of_offset offset)) |! raise

let of_probability ?(f = round_float_to_int) x =
  if 0.0 <= x && x <= 1.0 then
    Some (f (-10. *. log10 x))
  else
    None

let of_probability_exn ?(f = round_float_to_int) x =
  match of_probability ~f x with
  | Some s -> s
  | None ->
    Error (sprintf "invalid probability %0.17g" x) |! raise

let of_solexa_score ?(f = round_float_to_int) x =
  f (10. *. log10((10. ** (Float.of_int x /. 10.)) +. 1.))

let to_solexa_score ?(f = round_float_to_int) t =
  f (10. *. log10((10. ** (Float.of_int t /. 10.)) -. 1.))
