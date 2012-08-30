open Biocaml_internal_pervasives
open With_result
module Pos = Biocaml_pos

module Error = struct
  type string_to_item = [
  | `empty_line of Biocaml_pos.t
  | `incomplete_input of Biocaml_pos.t * string list * string option
  | `malformed_partial_sequence of string
  ]
  with sexp

  type t = [
    string_to_item
  | `unnamed_sequence of string
  | `unnamed_scores of int list
  ]
  with sexp
end

module Transform = struct
  type 'a item = [
  | `comment of string
  | `header of string
  | `partial_sequence of 'a
  ]
  with sexp
    
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
        

  let generic_parser ~parse_sequence
      ?filename ?pedantic ?sharp_comments ?semicolon_comments () =
    let name =
      sprintf "fasta_parser:%s" Option.(value ~default:"<>" filename) in
    let next =
      next ~parse_sequence ?pedantic ?sharp_comments ?semicolon_comments in
    Biocaml_transform.Line_oriented.make_stoppable ~name ?filename ~next ()
      ~on_error:(function `next e -> e
      | `incomplete_input e -> `incomplete_input e)
      

  let parse_string_sequence ~pedantic l =
    if pedantic && String.exists l
      ~f:(function 'A' .. 'Z' | '*' | '-' -> false | _ -> true)
    then output_error (`malformed_partial_sequence l)
    else output_ok (`partial_sequence l)
      
  let string_to_sequence_item =
    generic_parser ~parse_sequence:parse_string_sequence


  let parse_int_sequence ~pedantic l =
    let exploded = String.split ~on:' ' l in
    try
      output_ok (`partial_sequence 
                    (List.filter_map exploded (function
                    | "" -> None
                    | s -> Some (Int.of_string s))))
    with
      e -> output_error (`malformed_partial_sequence l)
        
  let string_to_score_item =
    generic_parser ~parse_sequence:parse_int_sequence
  
  let printer ~to_string ?comment_char () =
    let module PQ = Biocaml_transform.Printer_queue in
    let printer =
    PQ.make ~to_string:(function
    | `comment c ->
      Option.value_map comment_char ~default:"" ~f:(fun o -> sprintf "%c%s\n" o c)
    | `header n -> ">" ^ n ^ "\n"
    | `partial_sequence s -> (to_string s) ^ "\n") () in
    Biocaml_transform.make_stoppable ~name:"fasta_printer" ()
      ~feed:(fun r -> PQ.feed printer r)
      ~next:(fun stopped ->
        match (PQ.flush printer) with
        | "" -> if stopped then `end_of_stream else `not_ready
        | s -> `output s)
      
  let sequence_item_to_string = printer ~to_string:ident

  let score_item_to_string = printer ~to_string:(fun l ->
    String.concat ~sep:" " (List.map l Int.to_string))
    
  let generic_aggregator ~flush ~add ~is_empty () =
    let current_name = ref None in
    let result = Queue.create () in
    Biocaml_transform.make_stoppable ~name:"fasta_aggregator" ()
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
              output_ok (name, flush ())
            end
          else `not_ready
        | Some (None, stuff) when is_empty stuff -> `not_ready
        | Some (None, non_empty) ->
          output_error (`unnamed_sequence non_empty)
        | Some (Some name, seq) ->
          output_ok (name, seq))

  let sequence_item_to_aggregated () =
    let current_sequence = Buffer.create 42 in
    generic_aggregator 
      ~flush:(fun () ->
        let s = Buffer.contents current_sequence in
        Buffer.clear current_sequence;
        s)
      ~add:(fun s -> Buffer.add_string current_sequence s)
      ~is_empty:(fun s -> s = "")
      ()

  let score_item_to_aggregated () =
    let scores = Queue.create () in
    generic_aggregator
      ~flush:(fun () ->
        let l = Queue.to_list scores in
        Queue.clear scores;
        List.concat l)
      ~add:(fun l -> Queue.enqueue scores l)
      ~is_empty:((=) [])
      ()  

  let aggregated_to_sequence_item ?(line_width=80) () =
    let queue = Queue.create () in
    Biocaml_transform.make_stoppable ~name:"fasta_slicer" ()
      ~feed:(fun (hdr, seq) ->
        Queue.enqueue queue (`header hdr);
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
      
  let aggregated_to_score_item ?(group_by=10) () =
    let queue = Queue.create () in
    Biocaml_transform.make_stoppable ~name:"fasta_slicer" ()
      ~feed:(fun (hdr, seq) ->
        Queue.enqueue queue (`header hdr);
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
end      

module Exceptionful = struct
  exception Error of Error.t
  open Transform
  
  let sequence_stream_of_in_channel ?filename ?pedantic
      ?sharp_comments ?semicolon_comments inp =
    Biocaml_transform.bind_result
      (string_to_sequence_item
         ?filename ?pedantic ?sharp_comments ?semicolon_comments ())
      (sequence_item_to_aggregated ())
      ~on_error:(function `left x -> x | `right x -> x)
    |! Biocaml_transform.Pull_based.of_in_channel inp
    |! Biocaml_transform.Pull_based.to_stream_exn
        ~error_to_exn:(fun err -> Error err)

  let score_stream_of_in_channel ?filename ?pedantic
      ?sharp_comments ?semicolon_comments inp =
    Biocaml_transform.bind_result
      (string_to_score_item
         ?filename ?pedantic ?sharp_comments ?semicolon_comments ())
      (score_item_to_aggregated ())
      ~on_error:(function `left x -> x
      | `right (`unnamed_sequence xs) -> `unnamed_scores xs)
    |! Biocaml_transform.Pull_based.of_in_channel inp
    |! Biocaml_transform.Pull_based.to_stream_exn ~error_to_exn:(fun err -> Error err)

end
