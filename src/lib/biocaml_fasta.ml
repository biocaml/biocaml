open Biocaml_internal_pervasives
open Result
module Lines = Biocaml_lines

type char_seq = string with sexp
type int_seq = int list with sexp

type 'a item = {
  header : string;
  sequence : 'a;
} with sexp

type 'a raw_item = [
  | `comment of string
  | `header of string
  | `partial_sequence of 'a
]
with sexp

module Tags = struct

  type t = {
    forbid_empty_lines: bool;
    only_header_comment: bool;
    sharp_comments: bool;
    semicolon_comments: bool;
    max_items_per_line: int option;
    sequence: [ `int_sequence | `char_sequence of char list option ]
  }
  with sexp

  let char_sequence_default =
    { forbid_empty_lines = false;
      only_header_comment = false;
      sharp_comments = true;
      semicolon_comments = true;
      max_items_per_line = None;
      sequence = `char_sequence None }

  let int_sequence_default = { char_sequence_default with sequence = `int_sequence }

  let is_char_sequence t = t.sequence <> `int_sequence
  let is_int_sequence t = t.sequence = `int_sequence

  let forbid_empty_lines  tags = tags.forbid_empty_lines
  let only_header_comment tags = tags.only_header_comment
  let sharp_comments      tags = tags.sharp_comments
  let semicolon_comments  tags = tags.semicolon_comments
  let impose_sequence_alphabet tags =
    match tags.sequence with
    | `int_sequence -> None
    | `char_sequence (Some alphb) -> Some (fun c -> List.mem alphb c)
    | `char_sequence None -> None

  let max_items_per_line (t: t) =
    let default =
      match t.sequence with
      | `int_sequence -> 25
      | `char_sequence _ -> 72 in
    Option.value ~default t.max_items_per_line

  let comment_char (t: t) =
    if t.sharp_comments
    then Some '#'
    else if t.semicolon_comments
    then Some ';'
    else None

  let to_string t = sexp_of_t t |> Sexplib.Sexp.to_string
  let of_string s =
    try Ok (Sexplib.Sexp.of_string s |> t_of_sexp)
    with e -> Error (`tags_of_string e)


end

module Error = struct
  type string_to_raw_item = [
    | `empty_line of Pos.t
    | `incomplete_input of Pos.t * string list * string option
    | `malformed_partial_sequence of Pos.t * string
    | `sequence_is_too_long of Pos.t * string
  ]
  with sexp

  type t = [
    | string_to_raw_item
    | `unnamed_char_seq of char_seq
    | `unnamed_int_seq of int_seq
  ]
  with sexp
end

module Transform = struct

  let string_to_raw_item ~string_to_partial_sequence
      ?filename ?(tags=Tags.char_sequence_default) () =
    let name = sprintf "fasta_parser:%s" Option.(value ~default:"<>" filename) in
    let chopl s = String.(sub s ~pos:1 ~len:(length s - 1)) in
    let open Lines.Buffer in
    let buffer = make ?filename () in
    let feed = feed_string buffer in
    let rec next stopped = match (next_line buffer :> string option) with
      | None -> if stopped then `end_of_stream else `not_ready
      | Some line ->
        let open Tags in
        if String.for_all line ~f:Char.is_whitespace then
          if tags.forbid_empty_lines then
            output_error (`empty_line (current_position buffer))
          else
            next stopped
        else if tags.sharp_comments && line.[0] = '#' then
          output_ok (`comment (chopl line))
        else if tags.semicolon_comments && line.[0] = ';' then
          output_ok (`comment (chopl line))
        else if line.[0] = '>' then
          output_ok (`header (chopl line))
        else
          string_to_partial_sequence ~pos:(current_position buffer) line
    in
    Biocaml_transform.make ~name ~feed ~next ()

  let string_to_char_seq_raw_item
      ?filename ?(tags=Tags.char_sequence_default) () =
    let check_alphabet ~pos s = match tags.Tags.sequence with
      | `int_sequence
      | `char_sequence None ->
        output_ok (`partial_sequence s)
      | `char_sequence (Some alphabet) ->
        if String.for_all s ~f:(List.mem alphabet) then
          output_ok (`partial_sequence s)
        else
          output_error (`malformed_partial_sequence (pos,s))
    in
    let string_to_partial_sequence ~pos s =
      match tags.Tags.max_items_per_line with
      | Some n ->
        if String.length s > n then
          output_error (`sequence_is_too_long (pos,s))
        else
          check_alphabet ~pos s
      | None -> check_alphabet ~pos s
    in
    string_to_raw_item
      ~string_to_partial_sequence
      ?filename
      ~tags
      ()

  let string_to_int_seq_raw_item
      ?filename ?(tags=Tags.char_sequence_default) () =
    let string_to_partial_sequence ~pos s =
      try
        let il = List.filter_map (String.split ~on:' ' s) ~f:(function
          | "" -> None
          | s -> Some (Int.of_string s))
        in
        match tags.Tags.max_items_per_line with
        | Some n ->
          if List.length il > n then
            output_error (`sequence_is_too_long (pos,s))
          else
            output_ok (`partial_sequence il)
        | None -> output_ok (`partial_sequence il)
      with _ -> output_error (`malformed_partial_sequence (pos,s))
    in
    string_to_raw_item
      ~string_to_partial_sequence
      ?filename
      ~tags
      ()

  let raw_item_to_string_pure ?comment_char alpha_to_string =
    function
    | `comment c ->
      Option.value_map comment_char
        ~default:"" ~f:(fun o -> sprintf "%c%s\n" o c)
    | `header n -> ">" ^ n ^ "\n"
    | `partial_sequence s -> (alpha_to_string s) ^ "\n"

  (** Return a transform for converting [raw_item]s to strings, given
      a function [to_string] for converting either [char_seq]s or
      [int_seq]s. *)
  let generic_printer ~to_string ~tags () =
    let comment_char = Tags.comment_char tags in
    Biocaml_transform.of_function
      (raw_item_to_string_pure ?comment_char to_string)

  let char_seq_raw_item_to_string  ?(tags=Tags.char_sequence_default) =
    generic_printer ~to_string:ident ~tags

  let int_seq_to_string_pure = fun l ->
    String.concat ~sep:" " (List.map l Int.to_string)

  let int_seq_raw_item_to_string ?(tags=Tags.int_sequence_default) =
    generic_printer ~to_string:int_seq_to_string_pure ~tags

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

  let char_seq_item_to_raw_item ?(tags=Tags.char_sequence_default) () =
    let items_per_line = Tags.max_items_per_line tags in
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

  let int_seq_item_to_raw_item ?(tags=Tags.int_sequence_default) () =
    let items_per_line = Tags.max_items_per_line tags in
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

module Random = struct

  type specification = [
    | `non_sequence_probability of float
    | `tags of Tags.t
  ]
  with sexp

  type specification_list = specification list with sexp

  let specification_of_string s =
    try Ok (specification_list_of_sexp (Core.Std.Sexp.of_string s))
    with e -> Error (`fasta (`parse_specification e))

  let get_tags specification =
    List.find_map specification (function `tags t -> Some t | _ -> None)

  let unit_to_random_char_seq_raw_item specification =
    let open Result in
    let tags =
      get_tags specification
      |> Option.value ~default:Tags.char_sequence_default in
    begin match tags.Tags.sequence with
    | `char_sequence intags ->
      let has_comments =
        Tags.sharp_comments tags || Tags.semicolon_comments tags in
      let impose_sequence_alphabet = Tags.impose_sequence_alphabet tags in
      let only_header_comment  = Tags.only_header_comment tags in
      let max_items_per_line = Tags.max_items_per_line tags in
      let non_sequence_probability =
        List.find_map specification
          (function `non_sequence_probability p -> Some p | _ -> None)
        |> Option.value ~default:0.2 in
      let random_letter: 'a -> Char.t =
        match impose_sequence_alphabet with
        | Some f ->
          (fun _ ->
            let rec pick n =
              if (f n) then n else pick (Random.int 127 |> Char.of_int_exn) in
            pick (Random.int 127 |> Char.of_int_exn))
        | None -> (fun _ -> Random.int 26 + 65 |> Char.of_int_exn) in
      let header_or_comment =
        let first_time = ref true in
        fun id ->
          if !first_time
          then (
            begin match Random.int 3 with
            | 0 when has_comments -> `comment "Some random comment"
            | _ ->
              first_time := false;
              ksprintf (fun s -> `header s) "Sequence %d" id
            end
          ) else (
            begin match Random.int 5 with
            | 0 when has_comments && not only_header_comment ->
              `comment "Some random comment"
            | _ ->  ksprintf (fun s -> `header s) "Sequence %d" id
            end
          ) in
      let next_raw_item =
        let sequence_allowed = ref false in
        let seq_num = ref 0 in
        fun () ->
          if !sequence_allowed then
            begin  match Random.float 1. with
            | f when f <= non_sequence_probability ->
              incr seq_num; header_or_comment !seq_num
            | _ ->
              let items_per_line = 1 + Random.int max_items_per_line in
              `partial_sequence (String.init items_per_line random_letter)
            end
          else
            begin match header_or_comment !seq_num with
            | `header _ as h -> sequence_allowed := true; h
            | other -> other
            end
      in
      let todo = ref 0 in
      return (Biocaml_transform.make ()
          ~next:(fun stopped ->
            match !todo, stopped with
            | 0, true -> `end_of_stream
            | 0, false -> `not_ready
            | n, _  when n < 0 -> assert false
            | n, _ ->
              decr todo;
              `output (next_raw_item ()))
          ~feed:(fun () -> incr todo))
    | `int_sequence ->
      fail (`inconsistent_tags `int_sequence)
    end

end


let char_seq_raw_item_to_string =
  Transform.raw_item_to_string_pure ident

let int_seq_raw_item_to_string =
  Transform.(raw_item_to_string_pure int_seq_to_string_pure)


let in_channel_to_char_seq_raw_item_stream ?(buffer_size=65536) ?filename ?tags inp =
  let x = Transform.string_to_char_seq_raw_item ?filename ?tags () in
  Biocaml_transform.(in_channel_strings_to_stream ~buffer_size inp x)

let in_channel_to_int_seq_raw_item_stream ?(buffer_size=65536) ?filename ?tags inp =
  let x = Transform.string_to_int_seq_raw_item ?filename ?tags () in
  Biocaml_transform.(in_channel_strings_to_stream ~buffer_size inp x)

let in_channel_to_char_seq_item_stream ?(buffer_size=65536) ?filename ?tags inp =
  let x = Transform.string_to_char_seq_raw_item ?filename ?tags () in
  let y = Transform.char_seq_raw_item_to_item () in
  Biocaml_transform.(
    compose_results x y ~on_error:(function `left x -> x | `right x -> x)
    |! in_channel_strings_to_stream ~buffer_size inp
  )

let in_channel_to_int_seq_item_stream ?(buffer_size=65536) ?filename ?tags inp =
  let x = Transform.string_to_int_seq_raw_item ?filename ?tags () in
  let y = Transform.int_seq_raw_item_to_item () in
  Biocaml_transform.(
    compose_results x y ~on_error:(function `left x -> x | `right x -> x)
    |! in_channel_strings_to_stream ~buffer_size inp
  )

exception Error of Error.t

let error_to_exn err = Error err

let in_channel_to_char_seq_raw_item_stream_exn ?(buffer_size=65536) ?filename
    ?tags inp =
  Stream.result_to_exn ~error_to_exn (
    in_channel_to_char_seq_raw_item_stream ?filename ?tags inp
  )

let in_channel_to_int_seq_raw_item_stream_exn ?(buffer_size=65536) ?filename
    ?tags inp =
  Stream.result_to_exn ~error_to_exn (
    in_channel_to_int_seq_raw_item_stream ?filename ?tags inp
  )

let in_channel_to_char_seq_item_stream_exn ?(buffer_size=65536) ?filename
    ?tags inp =
  Stream.result_to_exn ~error_to_exn (
    in_channel_to_char_seq_item_stream ?filename ?tags inp
  )

let in_channel_to_int_seq_item_stream_exn ?(buffer_size=65536) ?filename
    ?tags inp =
  Stream.result_to_exn ~error_to_exn (
    in_channel_to_int_seq_item_stream ?filename ?tags inp
  )

let char_seq_items_to_out_channel ?tags items oc =
  let t =
    Biocaml_transform.compose
      (Transform.char_seq_item_to_raw_item ?tags ())
      (Transform.char_seq_raw_item_to_string ?tags ())
  in
  Biocaml_transform.stream_to_out_channel items t oc
