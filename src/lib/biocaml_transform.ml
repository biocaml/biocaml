open Biocaml_internal_pervasives

type ('input, 'output) t = {
  name: string option;
  next: unit -> [ `output of 'output | `end_of_stream | `not_ready ];
  feed: 'input -> unit;
  stop: unit -> unit;
}

let make_general ?name ~next ~feed ~stop () = {name; next; feed; stop }

exception Feeding_stopped_transformation of string

let feed t i = t.feed i
let next t = t.next ()
let stop t = t.stop ()
let name t = t.name

let make ?name ~feed ~next () =
  let stopped = ref false in
  make_general ?name ()
    ~feed:(fun x ->
      if not !stopped then
        feed x
      else
        raise (Feeding_stopped_transformation Option.(value ~default:"" name)))
    ~next:(fun () -> next !stopped)
    ~stop:(fun () -> stopped := true)

let make_result ?name ~feed ~next () =
  let stopped = ref false in
  let one_error_has_occured = ref false in
  make_general ?name ()
    ~feed:(fun x ->
      if not !stopped then
        feed x
      else
        raise (Feeding_stopped_transformation Option.(value ~default:"" name)))
    ~next:(fun () ->
      if !one_error_has_occured
      then `end_of_stream
      else
        begin match next !stopped with
        | `output (Error _) as e -> one_error_has_occured := true;  e
        | other -> other
        end)
    ~stop:(fun () -> stopped := true)

let identity ?name () =
  let q = Queue.create () in
  make ?name ~feed:(Queue.enqueue q) ()
    ~next:(fun stopped ->
      match Queue.dequeue q with
      | Some o -> `output o
      | None -> if stopped then `end_of_stream else `not_ready)

let stream_transformation tr en =
  let rec loop_until_ready tr en =
    match next tr with
    | `output o -> Some o
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

let on_input t ~f =
  { t with feed = fun x -> t.feed (f x) }

let on_output t ~f =
  { t with next = fun () ->
      match t.next () with
      | `output o -> `output (f o)
      | `not_ready -> `not_ready
      | `end_of_stream -> `end_of_stream }

let compose ta tb =
  let name =
    sprintf "(compose <%s> <%s>)"
      Option.(value ~default:"" (name ta))
      Option.(value ~default:"" (name tb)) in
  make_general ~name ()
    ~feed:(fun i -> feed ta i)
    ~stop:(fun () -> stop ta)
    ~next:(fun () ->
      let call_tb_next () =
        begin match next tb with
        | `output o -> `output o
        | `not_ready -> `not_ready
        | `end_of_stream -> `end_of_stream
        end
      in
      match next ta with
      | `output o -> feed tb o; call_tb_next ()
      | `not_ready -> call_tb_next ()
      | `end_of_stream -> stop tb; call_tb_next ())

let mix ta tb ~f =
  let a_buffer = ref None in
  let name =
    sprintf "(mix <%s> <%s>)"
      Option.(value ~default:"" (name ta))
      Option.(value ~default:"" (name tb)) in
  make_general ~name ()
    ~feed:(fun (a, b) -> feed ta a; feed tb b)
    ~stop:(fun () -> stop ta; stop tb)
    ~next:(fun () ->
      begin match !a_buffer with
      | None ->
        begin match next ta with
        | `output oa ->
          begin match next tb with
          | `output ob -> `output (f oa ob)
          | `not_ready -> a_buffer := Some oa; `not_ready
          | `end_of_stream -> `end_of_stream
          end
        | `not_ready -> `not_ready
        | `end_of_stream -> `end_of_stream
        end
      | Some oa ->
        begin match next tb with
        | `output ob -> `output (f oa ob)
        | `not_ready -> `not_ready
        | `end_of_stream -> `end_of_stream
        end
      end)

let filter_compose left right ~destruct ~reconstruct =
  let name =
    sprintf "(part-compose <%s> <%s>)"
      Option.(value ~default:"" (name left))
      Option.(value ~default:"" (name right)) in
  make_general ~name ()
    ~feed:(fun i -> feed left i)
    ~stop:(fun () -> stop left)
    ~next:(fun () ->
      let call_right_next () =
        begin match next right with
        | `output o -> `output (reconstruct (`Done o))
        | `not_ready -> `not_ready
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
      | `end_of_stream -> stop right; call_right_next ())

let split_and_merge ta tb ~split ~merge =
  let name = sprintf "(merge <%s> <%s>)"
    Option.(value ~default:"" (name ta))
    Option.(value ~default:"" (name tb)) in
  make_general ~name ()
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
        | `end_of_stream -> `end_of_stream
        end)

let compose_results ~on_error ta tb =
  let name =
    sprintf "(compose_results <%s> <%s>)"
      Option.(value ~default:"" (name ta))
      Option.(value ~default:"" (name tb)) in
  make_general ~name ()
    ~feed:(fun i -> feed ta i)
    ~stop:(fun () -> stop ta)
    ~next:(fun () ->
      let call_tb_next () =
        begin match next tb with
        | `output (Ok o) -> `output (Ok o)
        | `output (Error o) -> `output (Error (on_error (`right o)))
        | `not_ready -> `not_ready
        | `end_of_stream -> `end_of_stream
        end
      in
      match next ta with
      | `output (Ok o) -> feed tb o; call_tb_next ()
      | `output (Error o) -> `output (Error (on_error (`left o)))
      | `not_ready -> call_tb_next ()
      | `end_of_stream -> stop tb; call_tb_next ())

let compose_results_merge_error ta tb =
  compose_results ta tb
    ~on_error:(function `left e -> `left e | `right e -> `right e)

let compose_result_left ta tb =
  let name =
    sprintf "(compose_result_left <%s> <%s>)"
      Option.(value ~default:"" (name ta))
      Option.(value ~default:"" (name tb)) in
  make_general ~name ()
    ~feed:(fun i -> feed ta i)
    ~stop:(fun () -> stop ta)
    ~next:(fun () ->
      let call_tb_next () =
        begin match next tb with
        | `output o -> `output (Ok o)
        | `not_ready -> `not_ready
        | `end_of_stream -> `end_of_stream
        end
      in
      match next ta with
      | `output (Ok o) -> feed tb o; call_tb_next ()
      | `output (Error o) -> `output (Error o)
      | `not_ready -> call_tb_next ()
      | `end_of_stream -> stop tb; call_tb_next ())


module Line_oriented = struct

  type parsing_buffer = {
    mutable unfinished_line : string option;
    lines : string Queue.t;
    mutable parsed_lines : int;
    filename : string option;
  }

  let parsing_buffer ?filename () =
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

  let contents p = Queue.to_list p.lines, p.unfinished_line

  let empty p = (Queue.clear p.lines; p.unfinished_line <- None)

  let lines () =
    let buf = parsing_buffer () in
    make ~name:"lines"
      ~feed:(feed_string buf)
      ~next:(function
        | true -> (match next_line buf with
            | Some line -> `output line
            | None -> (match contents buf with
                | [], None -> `end_of_stream
                | [], Some unfinished_line ->
                    (empty buf; `output unfinished_line)
                | _ -> assert false
              )
          )
        | false -> (match next_line buf with
            | None -> `not_ready
            | Some line -> `output line
          )
      )
      ()

  let make ?name ?filename ~next ~on_error () =
    let lo_parser = parsing_buffer ?filename () in
    make ?name ()
      ~feed:(feed_string lo_parser)
      ~next:(fun stopped ->
        match next lo_parser with
        | `output (Ok r) -> `output (Ok r)
        | `output (Error r) -> `output (Error (on_error (`next r)))
        | `not_ready ->
          if stopped then (
            if is_empty lo_parser then
              `end_of_stream
            else
              let l,o = contents lo_parser in
              `output
                (Error
                   (on_error
                      (`incomplete_input (current_position lo_parser, l, o))))
          ) else
            `not_ready)

  let make_merge_error =
    make
      ~on_error:(function
        | `next e -> e
        | `incomplete_input e -> `incomplete_input e)

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
      | `not_ready ->
        begin match feeder () with
        | Some s -> feed tr s; feed_and_take ()
        | None -> stop tr; feed_and_take ()
        end
      | `end_of_stream -> `end_of_stream
    in
    feed_and_take

  let of_in_channel  ?(buffer_size=4096) ic tr =
    let buf = String.create buffer_size in
    let read_string () =
      match In_channel.input ~buf ic ~pos:0 ~len:buffer_size with
      | 0 -> None
      | len -> Some (String.sub buf ~pos:0 ~len)
    in
    of_feeder read_string tr

  let of_file ?buffer_size file tr =
    let ic = In_channel.create file in
    let next = of_in_channel ?buffer_size ic tr in
    (fun () ->
      let n = next () in
      if n = `end_of_stream then In_channel.close ic;
      n
    )

  let next t = t ()

  let to_stream_exn ~error_to_exn t =
    Stream.from (fun _ ->
      match t () with
      | `output (Ok o) -> Some o
      | `output (Error e) -> raise (error_to_exn e)
      | `end_of_stream -> None)

  let to_stream_result t =
    Stream.from (fun _ ->
      match t () with
      | `output o -> Some o
      | `end_of_stream -> None)

end
