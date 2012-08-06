open Biocaml_internal_pervasives


type ('input, 'output, 'error) t = {
  name: string option;
  next: unit -> [ `output of 'output | `end_of_stream
                | `error of 'error | `not_ready ];
  feed: 'input -> unit;
  stop: unit -> unit;
}

type no_error

let make ?name ~next ~feed ~stop () = {name; next; feed; stop }

exception Feeding_stopped_transformation of string
  
let feed t i = t.feed i
let next t = t.next ()
let stop t = t.stop ()
let name t = t.name

let make_stoppable ?name ~feed ~next () =
  let stopped = ref false in
  make ?name ()
    ~feed:(fun x ->
      if not !stopped then
        feed x
      else
        raise (Feeding_stopped_transformation Option.(value ~default:"" name)))
    ~next:(fun () -> next !stopped)
    ~stop:(fun () -> stopped := true)

let identity ?name () =
  let q = Queue.create () in
  make_stoppable ?name ~feed:(Queue.enqueue q) ()
    ~next:(fun stopped ->
      match Queue.dequeue q with
      | Some o -> `output o
      | None -> if stopped then `end_of_stream else `not_ready)

let on_input t ~f =
  { t with feed = fun x -> t.feed (f x) }
let on_output t ~f =
  { t with next = fun () ->
      match t.next () with
      | `output o -> `output (f o)
      | `not_ready -> `not_ready
      | `error e -> `error e
      | `end_of_stream -> `end_of_stream }
let on_error t ~f = 
  { t with next = fun () ->
    match t.next () with
    | `output o -> `output o
    | `not_ready -> `not_ready
    | `error e -> `error (f e)
    | `end_of_stream -> `end_of_stream }

let compose ta tb =
  let name =
    sprintf "(compose <%s> <%s>)"
      Option.(value ~default:"" (name ta))
      Option.(value ~default:"" (name tb)) in
  make ~name ()
    ~feed:(fun i -> feed ta i)
    ~stop:(fun () -> stop ta)
    ~next:(fun () ->
      let call_tb_next () =
        begin match next tb with
        | `output o -> `output o
        | `not_ready -> `not_ready
        | `error e -> `error (`right e)
        | `end_of_stream -> `end_of_stream
        end
      in
      match next ta with
      | `output o -> feed tb o; call_tb_next ()
      | `not_ready -> call_tb_next ()
      | `error e -> `error (`left e)
      | `end_of_stream -> stop tb; call_tb_next ())

let partially_compose left right ~destruct ~reconstruct =
  let name =
    sprintf "(part-compose <%s> <%s>)"
      Option.(value ~default:"" (name left))
      Option.(value ~default:"" (name right)) in
  make ~name ()
    ~feed:(fun i -> feed left i)
    ~stop:(fun () -> stop left)
    ~next:(fun () ->
      let call_right_next () =
        begin match next right with
        | `output o -> `output (reconstruct (`Done o))
        | `not_ready -> `not_ready
        | `error e -> `error (`right e)
        | `end_of_stream -> `end_of_stream
        end
      in
      match next left with
      | `output o ->
        begin match destruct o with
        | `Yes y -> feed right y; call_right_next ()
        | `No n -> `output (reconstruct (`Filtered n))
        end
      | `not_ready -> call_right_next ()
      | `error e -> `error (`left e)
      | `end_of_stream -> stop right; call_right_next ())
            
let mix ta tb ~f =
  let a_buffer = ref None in
  let name =
    sprintf "(mix <%s> <%s>)"
      Option.(value ~default:"" (name ta))
      Option.(value ~default:"" (name tb)) in
  make ~name ()
    ~feed:(fun (a, b) -> feed ta a; feed tb b)
    ~stop:(fun () -> stop ta; stop tb)
    ~next:(fun () ->
      begin match !a_buffer with
      | None ->
        begin match next ta with
        | `output oa ->
        begin match next tb with
        | `output ob -> `output (f oa ob)
        | `not_ready ->
          a_buffer := Some oa;
          `not_ready
        | `error e -> `error (`right e)
        | `end_of_stream -> `error (`end_of_left_stream)
        end
        | `not_ready -> `not_ready
        | `error e -> `error (`left e)
        | `end_of_stream ->
          begin match next tb with
          | `end_of_stream -> `end_of_stream
          |  _ -> `error (`end_of_right_stream)
          end
        end
      | Some oa ->
        begin match next tb with
        | `output ob -> `output (f oa ob)
        | `not_ready -> `not_ready
        | `error e -> `error (`right e)
        | `end_of_stream -> `error (`end_of_right_stream)
        end
      end)
    
let split_and_merge ta tb ~split ~merge =
  let name = sprintf "(merge <%s> <%s>)"
    Option.(value ~default:"" (name ta))
    Option.(value ~default:"" (name tb)) in
  make ~name ()
    ~feed:(fun z ->
      match split z with
      | `left a -> feed ta a
      | `right b -> feed tb b)
    ~stop:(fun () -> stop ta; stop tb)
    ~next:(fun () ->
      match next ta with
      | `output o -> `output (merge (`left o))
      | `not_ready | `end_of_stream ->
        begin match next tb with
        | `output o -> `output (merge (`right o))
        | `not_ready -> `not_ready
        | `error e -> `error (`right e)
        | `end_of_stream -> `end_of_stream
        end
      | `error e -> `error (`left e))
  
    
let stream_transformation ~error_to_exn tr en =
  let rec loop_until_ready tr en =
    match next tr with
    | `output o -> Some o
    | `error e -> raise (error_to_exn e)
    | `end_of_stream -> None
    | `not_ready ->
      begin match Stream.next en with
      | None -> stop tr; loop_until_ready tr en
      | Some s ->
        feed tr s;
        loop_until_ready tr en
      end
  in
  Stream.from (fun _ -> loop_until_ready tr en)
    
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

module Pull_based = struct

  type 'a stream = unit -> 'a  

  let of_feeder feeder tr =
    let rec feed_and_take () =
      match next tr with
      | `output o -> `output o
      | `error e -> `error e
      | `not_ready ->
        begin match feeder () with
        | Some s -> feed tr s; feed_and_take ()
        | None -> stop tr; feed_and_take ()
        end
      | `end_of_stream -> `end_of_stream in
    feed_and_take

    
  let of_in_channel  ?(buffer_size=4096) ic tr =
    let buf = String.create buffer_size in
    let read_string () =
      match In_channel.input ~buf ic ~pos:0 ~len:buffer_size with
      | 0 -> None
      | len -> Some (String.sub buf ~pos:0 ~len) in
    of_feeder read_string tr
    
  let of_file ?buffer_size file tr =
    let ic = In_channel.create file in
    let next = of_in_channel ?buffer_size ic tr in
    (fun () ->
      let n = next () in
      if n = `end_of_stream then In_channel.close ic;
      n)

  let next t = t ()

  let to_stream_exn ~error_to_exn t =
    Stream.from (fun _ ->
      match t () with
      | `error e -> raise (error_to_exn e)
      | `output o -> Some o
      | `end_of_stream -> None)

  let to_stream_result t =
    Stream.from (fun _ ->
      match t () with
      | `error e -> Some (Error e)
      | `output o -> Some (Ok o)
      | `end_of_stream -> None)
    
end

  
