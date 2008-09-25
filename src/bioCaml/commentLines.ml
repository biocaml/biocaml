open TylesBase
open Printf

type t = char * string list
    (* each string must be all whitespace or begin with specified char *)
    
exception Bad of string
let raise_bad msg = raise (Bad msg)
  
let concat (c1,t1) (c2,t2) = 
  if c1 = c2 
  then c1, t1@t2
  else raise_bad (sprintf "start characters %c and %c differ" c1 c2)
  
let of_string start_char s =
  let is_space c = match c with ' ' | '\t' | '\r' -> true | _ -> false in
  let parse_line s =
    let n = String.length s in
    if n > 0 && s.[0] = start_char then s
    else if String.for_all is_space s then s
    else raise_bad (sprintf "comment line must begin with %c or contain all whitespace" start_char)
  in
  let sl = if s = "" then [""] else Str.split_delim (Str.regexp "\n") s in
  start_char,List.map parse_line sl
    
let to_string (_,t) = String.concat "\n" t

let start_char (c,_) = c
