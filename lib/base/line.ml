open Sexplib.Std
open Base

type t = string [@@deriving sexp]

let empty = ""

let rightmost x =
  match String.rsplit2 x ~on:'\n' with
  | None -> (None, x)
  | Some (b, a) -> (Some b, a)

let parse_string s = String.split ~on:'\n' s


(* This is adapted from janestreet's Base, slightly more efficient
   (and less general) *)
let split str ~on:c =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      String.sub str ~pos:0 ~len:last_pos :: acc
    else
    if Char.(str.[pos] = c) then
      let pos1 = pos + 1 in
      let sub_str = String.sub str ~pos:pos1 ~len:(last_pos - pos1) in
      loop (sub_str :: acc) pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)

let append x y = x ^ y

let to_string x = x
