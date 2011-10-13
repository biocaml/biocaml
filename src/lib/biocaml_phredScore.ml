open Batteries_uni;; open Printf

exception Error of string

type t = int

let to_int t = t

let to_probability t =
  10.0 ** (float_of_int t /. -10.0)

let to_ascii ?(offset=33) t =
  let x = t + offset in
  if offset <= x && x <= 126 then
    Char.chr x
  else
    Error (sprintf "%d with offset %d cannot be encoded as a visible ASCII character" t offset) |> raise

let of_int x =
  if x >= 0 then x
  else Error (sprintf "invalid PHRED score %d" x) |> raise

let of_ascii ?(offset=33) x =
  let c = Char.code x in
  if offset <= c && c <= 126 then
    c - offset
  else
    Error (sprintf "%c with offset %d is not a valid score" x offset) |> raise

let of_probability
    ?(f = fun x ->
      if mod_float x 1.0 < 0.5
      then int_of_float (floor x)
      else int_of_float (ceil x)
    )
    x =
  if 0.0 <= x && x <= 1.0 then
    f (-10. *. log10 x)
  else
    Error (sprintf "invalid probability %0.17g" x) |> raise
