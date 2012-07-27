open Biocaml_internal_pervasives

type 'a data = [
| `comment of string | `name of string
| `partial_sequence of 'a
]
  
type parse_error = [
| `empty_line of Biocaml_pos.t
| `malformed_partial_sequence of string ]

let rec next ~parse_sequence
    ?(pedantic=true) ?(sharp_comments=true) ?(semicolon_comments=false) p =
  let open Biocaml_transform.Line_oriented in
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
  let module LOP =  Biocaml_transform.Line_oriented  in
  let lo_parser = LOP.parser ?filename () in
  Biocaml_transform.make_stoppable ~name ()
    ~feed:(LOP.feed_string lo_parser)
    ~next:(fun stopped ->
      match next ~parse_sequence
        ?pedantic ?sharp_comments ?semicolon_comments lo_parser with
      | `output r -> `output r
      | `error e -> `error e
      | `not_ready ->
        if stopped then (
          match LOP.finish lo_parser with
          | `ok -> `end_of_stream
          | `error (l, o) ->
            failwithf "incomplete fasta input? %S %S"
              (String.concat ~sep:"<RET>" l) Option.(value ~default:"" o) ()
        ) else
          `not_ready)
    

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
  let module PQ = Biocaml_transform.Printer_queue in
  let printer =
    PQ.make ~to_string:(function
    | `comment c ->
      Option.value_map comment_char ~default:"" ~f:(fun o -> sprintf "%c%s\n" o c)
    | `name n -> ">" ^ n ^ "\n"
    | `partial_sequence s -> (to_string s) ^ "\n") () in
  Biocaml_transform.make_stoppable ~name:"fasta_printer" ()
    ~feed:(fun r -> PQ.feed printer r)
    ~next:(fun stopped ->
      match (PQ.flush printer) with
      | "" -> if stopped then `end_of_stream else `not_ready
      | s -> `output s)

type empty
let sequence_printer = printer ~to_string:ident

let score_printer = printer ~to_string:(fun l ->
  String.concat ~sep:" " (List.map l Float.to_string))

let sequence_aggregator () =
  let current_name = ref None in
  let current_sequence = Buffer.create 42 in
  let result = Queue.create () in
  Biocaml_transform.make_stoppable ~name:"fasta_aggregator" ()
    ~feed:(function
    | `name n ->
      Queue.enqueue result (!current_name, Buffer.contents current_sequence);
      current_name := Some n;
      Buffer.clear current_sequence;
    | `partial_sequence s ->
      Buffer.add_string current_sequence s
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
            `output (name, Buffer.contents current_sequence)
          end
        else `not_ready
      | Some (None, "") -> `not_ready
      | Some (None, non_empty) ->
        `error (`unnamed_sequence non_empty)
      | Some (Some name, seq) ->
        `output (name, seq))
