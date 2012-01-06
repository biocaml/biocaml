open Batteries;; open Printf

type t = char * string list
    (* char is the char that each non-whitespace comment line must begin with.
       
       Each string must:       
       - be all whitespace or begin with the start char
       - must not contain the terminating newline
    *)
    
exception Invalid of string
  
let enum (c,sl) = c,List.enum sl

let empty c = c,[]

let concat (ac,al) (bc,bl) =
  if ac <> bc then
    Invalid (sprintf "start characters %c and %c differ" ac bc) |> raise
  else
    ac, al@bl

let is_space c = match c with ' ' | '\t' | '\r' -> true | _ -> false

let of_string ?(comment_char='#') s =
  let parse_line s =
    let n = String.length s in
    if (n > 0 && s.[0] = comment_char)
      || (s |> String.enum |> Enum.for_all is_space)
    then
      s
    else
      Invalid (sprintf "comment line must begin with %c or contain all whitespace" comment_char)
      |> raise
  in
  comment_char,
  s |> IO.input_string |> IO.lines_of |> Enum.map parse_line |> List.of_enum


let to_string = snd |- String.concat "\n"

let comment_char = fst

let is_comments ?(comment_char='#') s =
  try of_string ~comment_char s |> ignore; true
  with Invalid _ -> false
