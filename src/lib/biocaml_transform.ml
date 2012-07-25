open Biocaml_internal_pervasives


type ('input, 'output, 'error) t = {
  name: string option;
  next: unit -> [ `output of 'output | `end_of_stream
                | `error of 'error | `not_ready ];
  feed: 'input -> unit;
  stop: unit -> unit;
}

let make ?name ~next ~feed ~stop () = {name; next; feed; stop }

exception Feeding_stopped_transformation of string
  
let feed t i = t.feed i
let next t = t.next ()
let stop t = t.stop ()

let make_stoppable ?name ~feed ~next () =
  let stopped = ref false in
  make ?name ()
    ~feed:(fun x ->
      if !stopped then
        feed x
      else
        raise (Feeding_stopped_transformation Option.(value ~default:"" name)))
    ~next:(fun () -> next !stopped)
    ~stop:(fun () -> stopped := true)



  
module Line_oriented = struct

  type parser = {
    mutable unfinished_line : string option;
    lines : string Queue.t;
    mutable parsed_lines : int;
    filename : string option;
  }

  let parser ?filename () =
    {unfinished_line = None;
     lines = Queue.create ();
     parsed_lines = 0;
     filename}

  let feed_line p s =
    Queue.enqueue p.lines s
      
  let feed_string p s =
    let lines = String.split s ~on:'\n' in 
    let rec faux = function
      | [] -> assert false
      | [ "" ] -> (* last char was a "\n" *) ()
      | [ s ] -> (* there is a partial line at the end *)
        p.unfinished_line <- Some s;
      | h :: t ->
        Queue.enqueue p.lines h;
        faux t
    in
    match p.unfinished_line, lines with
    | _, [] -> ()
    | None, l -> faux l
    | Some s, h :: t ->
      p.unfinished_line <- None;
      faux ((s ^ h) :: t)
        
  let queued_lines p = Queue.length p.lines
  let next_line p =
    let l = Queue.dequeue p.lines in
    if l <> None then (
      p.parsed_lines <- p.parsed_lines + 1;
    );
    l
      
  exception No_next_line
  let next_line_exn p =
    match next_line p with
    | Some s -> s
    | None -> raise No_next_line

  let current_position p =
    Biocaml_pos.make ?file:p.filename ~line:p.parsed_lines ()

  let is_empty p =
    Queue.is_empty p.lines && p.unfinished_line = None

  let finish p =
    if is_empty p then `ok else `error (Queue.to_list p.lines, p.unfinished_line)
      
end

module Printer_queue = struct

  type 'a t = {
    records : 'a Queue.t;
    buffer : Buffer.t;
    clear_buffer: Buffer.t -> unit;
    to_string: 'a -> string;
  }

  let make ?(buffer:[`clear of int | `reset of int]= `reset 1024) ~to_string () =
    let buffer, clear_buffer =
      match buffer with
      | `clear s -> (Buffer.create s, Buffer.clear)
      | `reset s -> (Buffer.create s, Buffer.reset) in
    {
      records = Queue.create ();
      buffer; clear_buffer;
      to_string;
    }
  
  let feed p r = Queue.enqueue p.records r

  let flush p =
    let rec faux () =
      match Queue.dequeue p.records with
      | Some r ->
        Buffer.add_string p.buffer (p.to_string r);
        faux ()
      | None -> () in
    faux ();
    let ret = Buffer.contents p.buffer in
    p.clear_buffer p.buffer;
    ret

  let is_empty p = Queue.is_empty p.records
end

class type ['input, 'output, 'error] transform =
object
  method feed: 'input -> unit
  method stop: unit
  method next: [ `output of 'output | `not_ready
               | `error of 'error
               | `end_of_stream]
end

type ('input, 'output, 'error) stream_transform = ('input, 'output, 'error) transform
let feed t i = t#feed i
let stop t = t#stop
let next t = t#next
        
let on_input tr ~f =
object
  method feed x = tr#feed (f x)
  method stop = tr#stop
  method next = tr#next
end
let on_output tr ~f =
object
  method feed x = tr#feed x
  method stop = tr#stop
  method next =
    match tr#next with
    | `output o -> `output (f o)
    | `not_ready -> `not_ready
    | `error e -> `error e
    | `end_of_stream -> `end_of_stream
end
let on_error tr ~f = 
object
  method feed x = tr#feed x
  method stop = tr#stop
  method next =
    match tr#next with
    | `output o -> `output o
    | `not_ready -> `not_ready
    | `error e -> `error (f e)
    | `end_of_stream -> `end_of_stream
end
    
let compose ta tb =
object
  method feed (i: 'a) : unit =
    ta#feed i
  method stop = ta#stop
  method next =
    (* : [ `output of 'e | `not_ready | `error of [`left of 'c | `right of 'f ] ] = *)
    let call_tb_next () =
      begin match tb#next with
      | `output o -> `output o
      | `not_ready -> `not_ready
      | `error e -> `error (`right e)
      | `end_of_stream -> `end_of_stream
      end
    in
    match ta#next with
    | `output o -> tb#feed o; call_tb_next ()
    | `not_ready -> call_tb_next ()
    | `error e -> `error (`left e)
    | `end_of_stream -> tb#stop; call_tb_next ()
      
end 
  
let mix ta tb ~f =
object 
  val mutable a_buffer = None
  method feed (a, b) =
    ta#feed a;
    tb#feed b
  method stop = ta#stop; tb#stop
  method next =
    (* : [ `output of 'e | `not_ready | `error of [`left of 'c | `right of 'f ] ] = *)
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
        | `end_of_stream -> `error (`end_of_left_stream)
        end
      | `not_ready -> `not_ready
      | `error e -> `error (`left e)
      | `end_of_stream ->
        begin match tb#next with
        | `end_of_stream -> `end_of_stream
        |  _ -> `error (`end_of_right_stream)
        end
      end
    | Some oa ->
      begin match tb#next with
      | `output ob -> `output (f oa ob)
      | `not_ready -> `not_ready
      | `error e -> `error (`right e)
      | `end_of_stream -> `error (`end_of_right_stream)
      end
    end
end
(*
let with_termination transform =
object
  val mutable terminated = false
  method feed i =
    begin match i with
    | `input input ->
      if not terminated then transform#feed input else ()
    | `termination -> terminated <- true
    end
  method next =
    if not terminated then (
      match transform#next with
      | `output o -> `output (`output o)
      | `not_ready -> `not_ready
      | `error e -> `error e
    ) else (
      let rec loop acc =
        match transform#next with
        | `output o -> loop (o :: acc)
        | `not_ready -> `output (`terminated (List.rev acc))
        | `error e -> `error e
      in
      loop []
    )
  method is_empty = transform#is_empty
end
*) 
      
let enum_transformation ~error_to_exn tr en =
  let rec loop_until_ready tr en =
    match tr#next with
    | `output o -> o
    | `error e -> raise (error_to_exn e)
    | `end_of_stream -> raise BatEnum.No_more_elements
    | `not_ready ->
      begin match BatEnum.get en with
      | None -> tr#stop; loop_until_ready tr en
      | Some s ->
        tr#feed s;
        loop_until_ready tr en
      end
  in
  BatEnum.from (fun () -> loop_until_ready tr en)
    
    
let stream_transformation ~error_to_exn tr en =
  let rec loop_until_ready tr en =
    match tr#next with
    | `output o -> Some o
    | `error e -> raise (error_to_exn e)
    | `end_of_stream -> None
    | `not_ready ->
      begin match Stream.next en with
      | None -> tr#stop; loop_until_ready tr en
      | Some s ->
        tr#feed s;
        loop_until_ready tr en
      end
  in
  Stream.from (fun _ -> loop_until_ready tr en)
    

  
