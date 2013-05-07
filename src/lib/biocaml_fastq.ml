open Biocaml_internal_pervasives
open Result
module Pos = Biocaml_pos

type item = {
  name: string;
  sequence: string;
  comment: string;
  qualities: string;
}
with sexp

module Error = struct
  type t =
      [ `sequence_and_qualities_do_not_match of Pos.t * string * string
      | `wrong_comment_line of Pos.t * string
      | `wrong_name_line of Pos.t * string
      | `incomplete_input of Pos.t * string list * string option]
  with sexp

  let string_sample s n =
    let l = String.length s in
    if n >= l then s else
      String.sub s ~pos:0 ~len:n ^ "..."

  let t_to_string = function
    | `sequence_and_qualities_do_not_match (pos, s,q) ->
        sprintf "[%s]: sequence and qualities do not match (%d Vs %d characters)"
          (Pos.to_string pos) String.(length s) String.(length q)
    | `wrong_comment_line (pos, line) ->
        sprintf "[%s]: wrong comment line: %S"
          (Pos.to_string pos) (string_sample line 14)
    | `wrong_name_line (pos, line) ->
        sprintf "[%s]: wrong name line: %S"
          (Pos.to_string pos) (string_sample line 14)
    | `incomplete_input (pos, sl, so) ->
        sprintf "[%s]: end-of-stream reached with incomplete input: %S"
          (Pos.to_string pos)
          (String.concat ~sep:"\n" sl ^ Option.value ~default:"" so)

end

module Transform = struct
  let string_to_item ?filename () =
    let name = sprintf "fastq_parser:%s" Option.(value ~default:"<>" filename) in
    Biocaml_transform.Line_oriented.make_merge_error
      ~name ?filename ~next:(fun p ->
        let open Biocaml_transform.Line_oriented in
        if queued_lines p < 4 then
          `not_ready
        else (
          let name_line    = next_line_exn p in
          if String.length name_line = 0 || name_line.[0] <> '@'
          then output_error (`wrong_name_line (current_position p, name_line))
          else
            let sequence     = next_line_exn p in
            let comment_line = next_line_exn p in
            if String.length comment_line = 0 || comment_line.[0] <> '+'
            then output_error (`wrong_comment_line (current_position p, comment_line))
            else
              let qualities    = next_line_exn p in
              if String.length sequence <> String.length qualities
              then output_error
                (`sequence_and_qualities_do_not_match (current_position p,
                                                      sequence, qualities))
              else (
                output_ok {
                  name = String.sub name_line 1 (String.length name_line - 1);
                  comment = String.sub comment_line 1 (String.length comment_line - 1);
                  sequence; qualities }
              ))
      ) ()

  let item_to_string_pure r =
    sprintf "@%s\n%s\n+%s\n%s\n" r.name r.sequence r.comment r.qualities

  let item_to_string () =
    let module PQ = Biocaml_transform.Printer_queue in
    let printer =
      Biocaml_transform.Printer_queue.make ~to_string:item_to_string_pure () in
    Biocaml_transform.make ~name:"fastq_printer" ()
      ~feed:(fun r -> PQ.feed printer r)
      ~next:(fun stopped ->
        match (PQ.flush printer) with
        | "" -> if stopped then `end_of_stream else `not_ready
        | s -> `output s)

  let trim (specification: [`beginning of int|`ending of int]) =
    let items =  Queue.create () in
    let name =
      sprintf "(fastq_trimmer %s)"
        (match specification with
        | `beginning i -> sprintf "B:%d" i
        | `ending i -> sprintf "E:%d" i) in
    Biocaml_transform.make ~name ()
      ~feed:(fun r -> Queue.enqueue items r)
      ~next:(fun stopped ->
        begin match Queue.dequeue items with
        | Some r ->
          let rlgth = String.length r.sequence in
          begin match specification with
          | `beginning i when i < rlgth ->
            output_ok
              { r with sequence = String.sub r.sequence ~pos:i ~len:(rlgth - i);
                qualities = String.sub r.qualities ~pos:i ~len:(rlgth - i) }
          | `ending i when i < rlgth ->
            output_ok
              { r with sequence = String.sub r.sequence ~pos:0 ~len:(rlgth - i);
                qualities = String.sub r.qualities ~pos:0 ~len:(rlgth - i) }
          | _ ->
            output_error (`invalid_size rlgth)
          end
        | None -> if stopped then `end_of_stream else `not_ready
        end)
end

let in_channel_to_item_stream ?(buffer_size=65536) ?filename inp =
  Transform.string_to_item ?filename ()
  |! Biocaml_transform.in_channel_strings_to_stream ~buffer_size inp

exception Error of Error.t

let error_to_exn err = Error err

let in_channel_to_item_stream_exn ?(buffer_size=65536) ?filename inp =
  Stream.result_to_exn ~error_to_exn (
    in_channel_to_item_stream ~buffer_size ?filename inp
  )

let item_to_string = Transform.item_to_string_pure
