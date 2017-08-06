open Core_kernel
open CFStream

type ('input, 'output) t = {
  name: string option;
  next: unit -> [ `output of 'output | `end_of_stream | `not_ready ];
  feed: 'input -> unit;
  stop: unit -> unit;
}

let make_general ?name ~next ~feed ~stop () = {name; next; feed; stop }

exception Feeding_stopped_transform of string

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
        raise (Feeding_stopped_transform Option.(value ~default:"" name)))
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
        raise (Feeding_stopped_transform Option.(value ~default:"" name)))
    ~next:(fun () ->
      if !one_error_has_occured
      then `end_of_stream
      else
        begin match next !stopped with
        | `output (Error _) as e -> one_error_has_occured := true;  e
        | other -> other
        end)
    ~stop:(fun () -> stopped := true)

let of_function ?name f =
  let q = Queue.create () in
  make ?name ~feed:(Queue.enqueue q) ()
    ~next:(fun stopped ->
      match Queue.dequeue q with
      | Some o -> `output (f o)
      | None -> if stopped then `end_of_stream else `not_ready)

let identity ?name () = of_function ?name ident

let to_stream_fun tr en =
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

let in_channel_strings_to_stream ?(buffer_size=65536) ic tr =
  to_stream_fun tr (Stream.strings_of_channel ~buffer_size ic)

let stream_to_out_channel xs tr oc =
  Stream.iter (to_stream_fun tr xs) ~f:(Out_channel.output_string oc)

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

let mix ta tb =
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
          | `output ob -> `output (`both (oa, ob))
          | `not_ready -> a_buffer := Some oa; `not_ready
          | `end_of_stream -> `end_of_stream
          end
        | `not_ready -> `not_ready
        | `end_of_stream ->
          begin match next tb with
          | `output ob -> `output (`right ob)
          | `not_ready -> `not_ready
          | `end_of_stream -> `end_of_stream
          end
        end
      | Some oa ->
        begin match next tb with
        | `output ob -> a_buffer := None; `output (`both (oa, ob))
        | `not_ready -> `not_ready
        | `end_of_stream -> a_buffer := None; `output (`left oa)
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
        | `output o -> `output (reconstruct (`transformed o))
        | `not_ready -> `not_ready
        | `end_of_stream -> `end_of_stream
        end
      in
      match next left with
      | `output o ->
        begin match destruct o with
        | `transform y -> feed right y; call_right_next ()
        | `bypass n -> `output (reconstruct (`bypassed n))
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

let on_ok tr ~f =
  on_output tr ~f:(function
  | Ok o -> Ok (f o)
  | Error e -> Error e)

let on_error tr ~f =
  on_output tr ~f:(function
  | Ok o -> Ok o
  | Error e -> Error (f e))

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

class type ['input, 'output] object_t = object
  method next: [ `output of 'output | `end_of_stream | `not_ready ]
  method feed:  'input -> unit
  method stop: unit
end
let to_object tr =
object
  method next = next tr
  method feed s = feed tr s
  method stop = stop tr
end
let of_object o =
  make_general ~name:(sprintf "of_object_%d" (Oo.id o))
    ~next:(fun () -> o#next)
    ~feed:(fun s -> o#feed s)
    ~stop:(fun () -> o#stop) ()
