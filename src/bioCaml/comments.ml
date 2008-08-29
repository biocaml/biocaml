open TylesBase
open Printf

type t = string list
    (* each string must be all whitespace or begin with '#' *)
    
exception Bad of string
let raise_bad msg = raise (Bad msg)
  
let concat t1 t2 = t1@t2
  
let of_string s =
  let is_space c = match c with ' ' | '\t' | '\r' -> true | _ -> false in
  let parse_line s =
    let n = String.length s in
    if n > 0 && s.[0] = '#' then s
    else if String.for_all is_space s then s
    else raise_bad "comment line must begin with # or contain all whitespace"
  in
  let sl = if s = "" then [""] else Str.split_delim (Str.regexp "\n") s in
  List.map parse_line sl
    
let to_string t = String.concat "\n" t
