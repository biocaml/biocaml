open Core_kernel

exception Error of string

type t = int

let offset = 64

let to_probability t =
  let z = 10. ** (Float.of_int t /. -10.) in
  z /. (1. +. z)

let to_ascii t =
  let x = t + offset in
  if 33 <= x && x <= 126 then
    Char.of_int_exn x
  else
    Error (sprintf "%d cannot be encoded as a visible ASCII character" t) |> raise

let of_ascii x =
  let c = Char.to_int x in
  if 33 <= c && c <= 126 then
    c - offset
  else
    Error (sprintf "%c is not a valid score" x) |> raise

let of_probability ?(f = Float.iround_nearest_exn) x =
  if 0.0 <= x && x <= 1.0 then
    f (-10. *. Float.log10(x /. (1. -. x)))
  else
    Error (sprintf "invalid probability %0.17g" x) |> raise
