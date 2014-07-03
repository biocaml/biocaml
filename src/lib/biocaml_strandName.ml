open Core.Std

type t = Minus | Plus

let compare s t =
  match (s,t) with
    | (Minus,Minus) -> 0
    | (Minus,Plus) -> -1
    | (Plus,Minus) -> 1
    | (Plus,Plus) -> 0

let equal s t = compare s t  = 0

exception Bad of string
let raise_bad msg = raise (Bad msg)

let of_string_exn = function
  | "-" -> Minus
  | "+" -> Plus
  | s -> raise_bad (sprintf "unrecognized strand %s" s)

let of_string s =
  try Some (of_string_exn s)
  with Bad _ -> None
    
let minus_plus = function
  | Minus -> "-"
  | Plus -> "+"

let rev_fwd = function
  | Minus -> "rev"
  | Plus -> "fwd"

let to_string = minus_plus
