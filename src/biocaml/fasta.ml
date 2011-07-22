open Batteries_uni;; open Printf

exception Error of string

type record = string * string

let enum_input inp =
  let e = IO.lines_of inp in

  let open Comments in
  let comment_char = '#' in
  let e1,e2 = Enum.span (is_comments ~comment_char) e in
  let comments =
    Enum.fold (fun x y -> concat x (of_string y)) (empty comment_char) e1
  in

  let open Enum in
  let rec make_from_enum (e : string t) = make
    ~next:(fun () ->
      let a = match get e with
        | None -> raise No_more_elements
        | Some a -> a
      in
      let b = match get e with
        | None -> Invalid "expected sequence line after '>' line but reached end-of-input" |> raise
        | Some b -> b
      in
      a,b
    )

    ~count:(fun () ->
      let n = count e in
      if n mod 2 = 0 then n/2
      else Invalid "underlying enum does not contain multiple of 2 records" |> raise
    )

    ~clone:(fun () -> make_from_enum (clone e))
  in
  comments, make_from_enum e2
