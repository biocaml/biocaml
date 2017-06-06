open Core_kernel

type t = char

let minus = '-'
let plus = '+'

let of_string s = match String.lowercase s with
  | "-" | "rev" -> Ok minus
  | "+" | "fwd" -> Ok plus
  | _ -> error "unknown strand name" s sexp_of_string

let minus_plus = Fn.id

let rev_fwd = function
  | '-' -> "rev"
  | '+' -> "fwd"
  | _ -> assert false
