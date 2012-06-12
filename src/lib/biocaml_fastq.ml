open Biocaml_std

type record = {
  name: string;
  sequence: string;
  comment: string;
  qualities: string;
} 

type parser = {
  mutable unfinished_line : string option;
  lines : string Queue.t;
  mutable parsed_lines : int;
}

let parser () = {unfinished_line = None; lines = Queue.create (); parsed_lines = 0}

let feed_line p s =
  Queue.push s p.lines

let feed_string p s =
  let lines = BatString.nsplit s "\n" in 
  let rec faux = function
    | [] -> assert false
    | [ "" ] -> (* last char was a "\n" *) ()
    | [ s ] -> (* some remaining stuff *)
      p.unfinished_line <- Some s;
    | h :: t ->
      Queue.push h p.lines;
      faux t
  in
  match p.unfinished_line, lines with
  | _, [] -> ()
  | None, l -> faux l
  | Some s, h :: t ->
    p.unfinished_line <- None;
    faux ((s ^ h) :: t)

  

let next p =
  if Queue.length p.lines < 4 then
    `nothing_ready
  else (
    let name_line = Queue.pop p.lines in
    let sequence = Queue.pop p.lines in
    let comment_line = Queue.pop p.lines in
    let qualities = Queue.pop p.lines in
    if String.length name_line = 0 || name_line.[0] <> '@'
    then `error (`wrong_name_line (p.parsed_lines + 1, name_line))
    else if String.length comment_line = 0 || comment_line.[0] <> '+'
    then `error (`wrong_comment_line (p.parsed_lines + 3, comment_line))
    else if String.length sequence <> String.length qualities
    then `error (`sequence_and_qualities_do_not_match (p.parsed_lines + 2,
                                                       sequence, qualities))
    else (
      p.parsed_lines <- p.parsed_lines + 4;
      `record { name = String.sub name_line 1 (String.length name_line - 1);
                comment = String.sub name_line 1 (String.length name_line - 1);
                sequence; qualities }
    ))
      

(*

open Batteries;; open Printf


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
*)
