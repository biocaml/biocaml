open Batteries_uni;; open Printf

type t = char * string list
    (* each string must be all whitespace or begin with specified char *)
    
exception Invalid of string
  
let enum (_,sl) = List.enum sl

let concat ?(comment_char='#') ts = match ts with
  | [] -> comment_char,[]
  | (c,sl)::ts ->
      let rec loop ans = function
        | [] -> ans
        | (c',sl')::ts ->
            if c = c' then loop (ans@sl') ts
            else raise (Invalid (sprintf "start characters %c and %c differ" c c'))
      in
      c, loop sl ts
  
let of_string ?(comment_char='#') s =
  let is_space c = match c with ' ' | '\t' | '\r' -> true | _ -> false in
  let parse_line s =
    let n = String.length s in
    if n > 0 && s.[0] = comment_char then s
    else if s |> String.enum |> Enum.for_all is_space then s
    else raise (Invalid (sprintf "comment line must begin with %c or contain all whitespace" comment_char))
  in
  comment_char, s |> IO.input_string |> IO.lines_of |> Enum.map parse_line |> List.of_enum
  
let to_string (_,t) =
  let cout = IO.output_string() in
  List.iter (IO.write_line cout) t;
  let ans : string = IO.close_out cout in
  ans

let to_string (_,t) = String.concat "\n" t

let comment_char (c,_) = c

let is_comments ?(comment_char='#') s =
  try of_string ~comment_char s |> ignore; true
  with Invalid _ -> false
