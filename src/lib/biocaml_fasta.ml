open Biocaml_internal_pervasives
open With_result
module Pos = Biocaml_pos

type char_seq = string with sexp
type int_seq = int list with sexp

type 'a item = {
  header : string;
  sequence : 'a;
} with sexp

module Error = struct
  type string_to_raw_item = [
  | `empty_line of Pos.t
  | `incomplete_input of Pos.t * string list * string option
  | `malformed_partial_sequence of string
  ]
  with sexp

  type t = [
    string_to_raw_item
  | `unnamed_char_seq of char_seq
  | `unnamed_int_seq of int_seq
  ]
  with sexp
end

module Transform = struct
  type 'a raw_item = [
  | `comment of string
  | `header of string
  | `partial_sequence of 'a
  ]
  with sexp
    
  (** The {i next} function used to construct the transform in
      [generic_parser]. *)
  let rec next ~parse_sequence
      ?(pedantic=true) ?(sharp_comments=true) ?(semicolon_comments=false) p =
    let open Biocaml_transform.Line_oriented in
    match next_line p with
    | Some "" ->
      if pedantic
      then output_error (`empty_line (current_position p))
      else 
        next ~parse_sequence ~pedantic ~sharp_comments ~semicolon_comments p
    | Some l when sharp_comments && String.is_prefix l ~prefix:"#" ->
      output_ok (`comment String.(sub l ~pos:1 ~len:(length l - 1)))
    | Some l when semicolon_comments && String.is_prefix l ~prefix:";" ->
      output_ok (`comment String.(sub l ~pos:1 ~len:(length l - 1)))
    | Some l when String.is_prefix l ~prefix:">" ->
      output_ok (`header String.(sub l ~pos:1 ~len:(length l - 1)))
    | Some l ->
      parse_sequence ~pedantic l
    | None -> 
      `not_ready
            
  (** Return a transform converting strings to [raw_item]s, given a
      function [parse_sequence] for parsing either [char_seq]s or
      [int_seq]s. *)
  let generic_parser ~parse_sequence
      ?filename ?pedantic ?sharp_comments ?semicolon_comments () =
    let name =
      sprintf "fasta_parser:%s" Option.(value ~default:"<>" filename) in
    let next =
      next ~parse_sequence ?pedantic ?sharp_comments ?semicolon_comments in
    Biocaml_transform.Line_oriented.make ~name ?filename ~next ()
      ~on_error:(function `next e -> e
      | `incomplete_input e -> `incomplete_input e)
      
  let string_to_char_seq_raw_item =
    generic_parser ~parse_sequence:(fun ~pedantic l ->
      if pedantic && String.exists l
        ~f:(function 'A' .. 'Z' | '*' | '-' -> false | _ -> true)
      then output_error (`malformed_partial_sequence l)
      else output_ok (`partial_sequence l)
    )

  let string_to_int_seq_raw_item =
    generic_parser ~parse_sequence:(fun ~pedantic l ->
        let exploded = String.split ~on:' ' l in
        try
          output_ok (`partial_sequence 
                        (List.filter_map exploded (function
                          | "" -> None
                          | s -> Some (Int.of_string s))))
        with _ -> output_error (`malformed_partial_sequence l)
    )
      
  (** Return a transform for converting [raw_item]s to strings, given
      a function [to_string] for converting either [char_seq]s or
      [int_seq]s. *)
  let generic_printer ~to_string ?comment_char () =
    let module PQ = Biocaml_transform.Printer_queue in
    let printer =
    PQ.make ~to_string:(function
    | `comment c ->
      Option.value_map comment_char ~default:"" ~f:(fun o -> sprintf "%c%s\n" o c)
    | `header n -> ">" ^ n ^ "\n"
    | `partial_sequence s -> (to_string s) ^ "\n") () in
    Biocaml_transform.make ~name:"fasta_printer" ()
      ~feed:(fun r -> PQ.feed printer r)
      ~next:(fun stopped ->
        match (PQ.flush printer) with
        | "" -> if stopped then `end_of_stream else `not_ready
        | s -> `output s)
      
  let char_seq_raw_item_to_string = generic_printer ~to_string:ident

  let int_seq_raw_item_to_string = generic_printer ~to_string:(fun l ->
    String.concat ~sep:" " (List.map l Int.to_string))

  (** Return transform for aggregating [raw_item]s into [item]s given
      methods for working with buffers of [char_seq]s or [int_seq]s. *)
  let generic_aggregator ~flush ~add ~is_empty ~unnamed_sequence () =
    let current_name = ref None in
    let result = Queue.create () in
    Biocaml_transform.make ~name:"fasta_aggregator" ()
      ~feed:(function
      | `header n ->
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
              output_ok {header=name; sequence=flush ()}
            end
          else `not_ready
        | Some (None, stuff) when is_empty stuff -> `not_ready
        | Some (None, non_empty) ->
          output_error (unnamed_sequence non_empty)
        | Some (Some name, seq) ->
          output_ok {header=name; sequence=seq})

  let char_seq_raw_item_to_item () =
    let current_sequence = Buffer.create 42 in
    generic_aggregator 
      ~flush:(fun () ->
        let s = Buffer.contents current_sequence in
        Buffer.clear current_sequence;
        s)
      ~add:(fun s -> Buffer.add_string current_sequence s)
      ~is_empty:(fun s -> s = "")
      ~unnamed_sequence:(fun x -> `unnamed_char_seq x)
      ()

  let int_seq_raw_item_to_item () =
    let scores = Queue.create () in
    generic_aggregator
      ~flush:(fun () ->
        let l = Queue.to_list scores in
        Queue.clear scores;
        List.concat l)
      ~add:(fun l -> Queue.enqueue scores l)
      ~is_empty:((=) [])
      ~unnamed_sequence:(fun x -> `unnamed_int_seq x)
      ()  

  let char_seq_item_to_raw_item ?(items_per_line=80) () =
    let queue = Queue.create () in
    Biocaml_transform.make ~name:"fasta_slicer" ()
      ~feed:(fun {header=hdr; sequence=seq} ->
        Queue.enqueue queue (`header hdr);
        let rec loop idx =
          if idx + items_per_line >= String.length seq then (
            Queue.enqueue queue
              (`partial_sequence String.(sub seq idx (length seq - idx)));
          ) else (
            Queue.enqueue queue
              (`partial_sequence String.(sub seq idx items_per_line));
            loop (idx + items_per_line);
          ) in
        loop 0)
      ~next:(fun stopped ->
        match Queue.dequeue queue with
        | Some s -> `output s
        | None -> if stopped then `end_of_stream else `not_ready)
      
  let int_seq_item_to_raw_item ?(items_per_line=27) () =
    let queue = Queue.create () in
    Biocaml_transform.make ~name:"fasta_slicer" ()
      ~feed:(fun {header=hdr; sequence=seq} ->
        Queue.enqueue queue (`header hdr);
        let rec loop l =
        match List.split_n l items_per_line with
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
end      

module Result = struct

  let in_channel_to_char_seq_item_stream ?filename ?pedantic
      ?sharp_comments ?semicolon_comments inp =
    let x = Transform.string_to_char_seq_raw_item
      ?filename ?pedantic ?sharp_comments ?semicolon_comments () in
    let y = Transform.char_seq_raw_item_to_item () in
    Biocaml_transform.(
      compose_results x y ~on_error:(function `left x -> x | `right x -> x)
      |! Pull_based.of_in_channel inp
      |! Pull_based.to_stream_result
    )

  let in_channel_to_int_seq_item_stream ?filename ?pedantic
      ?sharp_comments ?semicolon_comments inp =
    let x = Transform.string_to_int_seq_raw_item
      ?filename ?pedantic ?sharp_comments ?semicolon_comments () in
    let y = Transform.int_seq_raw_item_to_item () in
    Biocaml_transform.(
      compose_results x y ~on_error:(function `left x -> x | `right x -> x)
      |! Pull_based.of_in_channel inp
      |! Pull_based.to_stream_result
    )

end

exception Error of Error.t

let error_to_exn err = Error err

let in_channel_to_char_seq_item_stream ?filename ?pedantic
    ?sharp_comments ?semicolon_comments inp =
  Stream.result_to_exn ~error_to_exn (
    Result.in_channel_to_char_seq_item_stream ?filename ?pedantic
      ?sharp_comments ?semicolon_comments inp
  )

let in_channel_to_int_seq_item_stream ?filename ?pedantic
    ?sharp_comments ?semicolon_comments inp =
  Stream.result_to_exn ~error_to_exn (
    Result.in_channel_to_int_seq_item_stream ?filename ?pedantic
      ?sharp_comments ?semicolon_comments inp
  )
