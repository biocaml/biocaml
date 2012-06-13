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
    `not_ready
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
                comment = String.sub comment_line 1 (String.length comment_line - 1);
                sequence; qualities }
    ))
      

type printer = {
  records : record Queue.t;
  buffer : Buffer.t;
  clear_buffer: Buffer.t -> unit;
}

let printer ?(buffer:[`clear of int | `reset of int]= `reset 1024)  () =
  let buffer, clear_buffer =
    match buffer with
    | `clear s -> (Buffer.create s, Buffer.clear)
    | `reset s -> (Buffer.create s, Buffer.reset) in
  {
    records = Queue.create ();
    buffer; clear_buffer;
  }
  
let feed_record p r = Queue.push r p.records

let get_string p =
  let rec faux () =
    if Queue.is_empty p.records then ()
    else (
      let r = Queue.pop p.records in
      Buffer.add_string p.buffer
        (sprintf "@%s\n%s\n+%s\n%s\n" r.name r.sequence r.comment r.qualities);
      faux ()
    ) in
  faux ();
  let ret = Buffer.contents p.buffer in
  p.clear_buffer p.buffer;
  ret
  


class type ['input, 'output, 'error] transform =
object
  method feed: 'input -> unit
  method next: [ `output of 'output | `not_ready | `error of 'error ]
end

type parser_error =
[ `sequence_and_qualities_do_not_match of int * string * string
| `wrong_comment_line of int * string
| `wrong_name_line of int * string ]

class fastq_parser =
object
  val parser = parser ()
  method feed s =
    feed_string parser s
  method next:  [ `output of record | `not_ready | `error of parser_error ] =
    match next parser with
    | `record r -> `output r
    | `error e -> `error e
    | `not_ready -> `not_ready
end
  
type empty
class fastq_printer =
object
  val printer = printer ()
  method feed r = feed_record printer r
  method next :  [ `output of string | `not_ready | `error of empty ] =
    match (get_string printer) with
    | "" -> `not_ready
    | s -> `output s
end
  
  

class trimmer (specification: [`beginning of int|`ending of int]) =
object
  val records =  Queue.create ()
  method feed r = Queue.push r records 
  method next: 
    [ `output of record | `not_ready | `error of [`invalid_size of int] ] =
    if Queue.length records > 0 then
      let r = Queue.pop records in
      let rlgth = String.length r.sequence in
      begin match specification with
      | `beginning i when i < rlgth ->
        `output 
          { r with sequence = String.sub r.sequence ~pos:i ~len:(rlgth - i);
              qualities = String.sub r.qualities ~pos:i ~len:(rlgth - i) }
      | `ending i when i < rlgth ->
        `output 
          { r with sequence = String.sub r.sequence ~pos:0 ~len:(rlgth - i);
              qualities = String.sub r.qualities ~pos:0 ~len:(rlgth - i) }
      | _ ->
        `error (`invalid_size rlgth)
      end
    else
      `not_ready

end

let compose ta tb =
object
  method feed (i: 'a) : unit =
    ta#feed i
  method next : [ `output of 'e | `not_ready
                | `error of [`left of 'c | `right of 'f ] ] =
    match ta#next with
    | `output o ->
      tb#feed o;
      begin match tb#next with
      | `output o -> `output o
      | `not_ready -> `not_ready
      | `error e -> `error (`right e)
      end
    | `not_ready -> `not_ready
    | `error e -> `error (`left e)
end 
  
let mix ta tb ~f =
object 
  val mutable a_buffer = None
  method feed (a, b) =
    ta#feed a;
    tb#feed b
  method next : [ `output of 'e | `not_ready
                | `error of [`left of 'c | `right of 'f ] ] =
    begin match a_buffer with
    | None ->
      begin match ta#next with
      | `output oa ->
        begin match tb#next with
        | `output ob -> `output (f oa ob)
        | `not_ready ->
          a_buffer <- Some oa;
          `not_ready
        | `error e -> `error (`right e)
        end
      | `not_ready -> `not_ready
      | `error e -> `error (`left e)
      end
    | Some oa ->
      begin match tb#next with
      | `output ob -> `output (f oa ob)
      | `not_ready -> `not_ready
      | `error e -> `error (`right e)
      end
    end
end
  

  
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
