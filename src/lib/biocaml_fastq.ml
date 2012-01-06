open Batteries;; open Printf

exception Invalid of string

type record = string * string * string * string

let enum_input cin =
  let e = IO.lines_of cin in
  let open Enum in
  let rec make_from_enum (e : string t) = make
    ~next:(fun () ->
      let a = match get e with
        | None -> raise No_more_elements
        | Some a -> a
      in
      let b = match get e with
        | None -> raise (Invalid "expected sequence line after '@' line but reached end-of-input")
        | Some b -> b
      in
      let c = match get e with
        | None -> raise (Invalid "expected '+' line after sequence line but reached end-of-inpout")
        | Some c -> c
      in
      let d = match get e with
        | None -> raise (Invalid "expected quality score after '+' line but reached end-of-input")
        | Some d -> d
      in
      a,b,c,d
    )
    
    ~count:(fun () ->
      let n = count e in
      if n mod 4 = 0 then n/4
      else raise (Invalid "underlying enum does not contain multiple of 4 items")
    )
    
    ~clone:(fun () -> make_from_enum (clone e))
  in
  make_from_enum e
