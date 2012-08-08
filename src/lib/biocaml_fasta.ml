open Biocaml_internal_pervasives
module Pos = Biocaml_pos
module Transform = Biocaml_transform

type 'a data = [
| `comment of string | `name of string
| `partial_sequence of 'a
]
  
type error = [
| `empty_line of Pos.t
| `incomplete_input of Pos.t * string list * string option
| `malformed_partial_sequence of string
| `unnamed_sequence of string ]

let rec next ~parse_sequence
    ?(pedantic=true) ?(sharp_comments=true) ?(semicolon_comments=false) p =
  let open Transform.Line_oriented in
  match next_line p with
  | Some "" ->
    if pedantic then `error (`empty_line (current_position p)) else `not_ready
  | Some l when sharp_comments && String.is_prefix l ~prefix:"#" ->
    `output (`comment String.(sub l ~pos:1 ~len:(length l - 1)))
  | Some l when semicolon_comments && String.is_prefix l ~prefix:";" ->
    `output (`comment String.(sub l ~pos:1 ~len:(length l - 1)))
  | Some l when String.is_prefix l ~prefix:">" ->
    `output (`name String.(sub l ~pos:1 ~len:(length l - 1)))
  | Some l ->
    parse_sequence ~pedantic l
  | None -> 
    `not_ready
    

let generic_parser ~parse_sequence
    ?filename ?pedantic ?sharp_comments ?semicolon_comments () =
  let name = sprintf "fasta_parser:%s" Option.(value ~default:"<>" filename) in
  let next = next ~parse_sequence ?pedantic ?sharp_comments ?semicolon_comments in
  Transform.Line_oriented.stoppable_parser ~name ?filename ~next ()
    

let parse_string_sequence ~pedantic l =
  if pedantic && String.exists l
    ~f:(function 'A' .. 'Z' | '*' | '-' -> false | _ -> true)
  then `error (`malformed_partial_sequence l)
  else `output (`partial_sequence l)

let sequence_parser = generic_parser ~parse_sequence:parse_string_sequence


let parse_float_sequence ~pedantic l =
  let exploded = String.split ~on:' ' l in
  try
    `output (`partial_sequence 
                (List.filter_map exploded (function
                | "" -> None
                | s -> Some (Float.of_string s))))
  with
    e -> `error (`malformed_partial_sequence l)

let score_parser = generic_parser ~parse_sequence:parse_float_sequence
  
let printer ~to_string ?comment_char () =
  let module PQ = Transform.Printer_queue in
  let printer =
    PQ.make ~to_string:(function
    | `comment c ->
      Option.value_map comment_char ~default:"" ~f:(fun o -> sprintf "%c%s\n" o c)
    | `name n -> ">" ^ n ^ "\n"
    | `partial_sequence s -> (to_string s) ^ "\n") () in
  Transform.make_stoppable ~name:"fasta_printer" ()
    ~feed:(fun r -> PQ.feed printer r)
    ~next:(fun stopped ->
      match (PQ.flush printer) with
      | "" -> if stopped then `end_of_stream else `not_ready
      | s -> `output s)

let sequence_printer = printer ~to_string:ident

let score_printer = printer ~to_string:(fun l ->
  String.concat ~sep:" " (List.map l Float.to_string))


let generic_aggregator ~flush ~add ~is_empty () =
  let current_name = ref None in
  let result = Queue.create () in
  Transform.make_stoppable ~name:"fasta_aggregator" ()
    ~feed:(function
    | `name n ->
      Queue.enqueue result (!current_name, flush ());
      current_name := Some n;
    | `partial_sequence s -> add s
    | `comment c -> ())
    ~next:(fun stopped ->
      match Queue.dequeue result with
      | None ->
        if stopped
        then 
          begin match !current_name with
          | None -> `end_of_stream
          | Some name ->
            current_name := None;
            `output (name, flush ())
          end
        else `not_ready
      | Some (None, stuff) when is_empty stuff -> `not_ready
      | Some (None, non_empty) ->
        `error (`unnamed_sequence non_empty)
      | Some (Some name, seq) ->
        `output (name, seq))

let sequence_aggregator () =
  let current_sequence = Buffer.create 42 in
  generic_aggregator 
    ~flush:(fun () ->
      let s = Buffer.contents current_sequence in
      Buffer.clear current_sequence;
      s)
    ~add:(fun s -> Buffer.add_string current_sequence s)
    ~is_empty:(fun s -> s = "")
    ()

let score_aggregator () =
  let scores = Queue.create () in
  generic_aggregator
    ~flush:(fun () ->
      let l = Queue.to_list scores in
      Queue.clear scores;
      List.concat l)
    ~add:(fun l -> Queue.enqueue scores l)
    ~is_empty:((=) [])
    ()  

let sequence_slicer ?(line_width=80) () =
  let queue = Queue.create () in
  Transform.make_stoppable ~name:"fasta_slicer" ()
    ~feed:(fun (name, seq) ->
      Queue.enqueue queue (`name name);
      let rec loop idx =
        if idx + line_width >= String.length seq then (
          Queue.enqueue queue
            (`partial_sequence String.(sub seq idx (length seq - idx)));
        ) else (
          Queue.enqueue queue
            (`partial_sequence String.(sub seq idx line_width));
          loop (idx + line_width);
        ) in
      loop 0)
    ~next:(fun stopped ->
      match Queue.dequeue queue with
      | Some s -> `output s
      | None -> if stopped then `end_of_stream else `not_ready)
    
let score_slicer ?(group_by=10) () =
  let queue = Queue.create () in
  Transform.make_stoppable ~name:"fasta_slicer" ()
    ~feed:(fun (name, seq) ->
      Queue.enqueue queue (`name name);
      let rec loop l =
        match List.split_n l group_by with
        | finish, [] -> 
          Queue.enqueue queue (`partial_sequence finish);
        | some, rest ->
          Queue.enqueue queue (`partial_sequence some);
          loop rest
      in
      loop seq)
    ~next:(fun stopped ->
      match Queue.dequeue queue with
      | Some s -> `output s
      | None -> if stopped then `end_of_stream else `not_ready)
    

module Excn = struct
  exception Error of error

  let sequence_stream_of_in_channel ?filename ?pedantic ?sharp_comments ?semicolon_comments inp =
    let flip f x y = f y x in
    (sequence_parser ?filename ?pedantic ?sharp_comments ?semicolon_comments ())
    |! flip Transform.compose (sequence_aggregator ())
    |! Transform.on_error ~f:(function `left x -> x | `right x -> x)
    |! Transform.Pull_based.of_in_channel inp
    |! Transform.Pull_based.to_stream_exn ~error_to_exn:(fun err -> Error err)

end
