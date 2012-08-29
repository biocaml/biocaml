open Biocaml_internal_pervasives
open With_result

type record = {
  name: string;
  sequence: string;
  comment: string;
  qualities: string;
}
with sexp


let next p =
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


let printer_queue =
  Biocaml_transform.Printer_queue.make ~to_string:(fun r ->
    sprintf "@%s\n%s\n+%s\n%s\n" r.name r.sequence r.comment r.qualities)


type string_to_record_error =
[ `sequence_and_qualities_do_not_match of Biocaml_pos.t * string * string
| `wrong_comment_line of Biocaml_pos.t * string
| `wrong_name_line of Biocaml_pos.t * string
| `incomplete_input of Biocaml_pos.t * string list * string option]
with sexp

let string_sample s n =
  let l = String.length s in
  if n >= l then s else
    String.sub s ~pos:0 ~len:n ^ "..."

let string_of_parser_error = function
| `sequence_and_qualities_do_not_match (pos, s,q) ->
  sprintf "[%s]: sequence and qualities do not match (%d Vs %d characters)"
    (Biocaml_pos.to_string pos) String.(length s) String.(length q)
| `wrong_comment_line (pos, line) ->
  sprintf "[%s]: wrong comment line: %S"
    (Biocaml_pos.to_string pos) (string_sample line 14)
| `wrong_name_line (pos, line) ->
  sprintf "[%s]: wrong name line: %S"
    (Biocaml_pos.to_string pos) (string_sample line 14)
| `incomplete_input (pos, sl, so) ->
  sprintf "[%s]: end-of-stream reached with incomplete input: %S"
    (Biocaml_pos.to_string pos)
    (String.concat ~sep:"\n" sl ^ Option.value ~default:"" so)

module Transform = struct
  let string_to_record ?filename () =
    let name = sprintf "fastq_parser:%s" Option.(value ~default:"<>" filename) in
    Biocaml_transform.Line_oriented.make_stoppable_merge_error
      ~name ?filename ~next ()


  let record_to_string () =
    let module PQ = Biocaml_transform.Printer_queue in
    let printer = printer_queue () in
    Biocaml_transform.make_stoppable ~name:"fastq_printer" ()
      ~feed:(fun r -> PQ.feed printer r)
      ~next:(fun stopped ->
        match (PQ.flush printer) with
        | "" -> if stopped then `end_of_stream else `not_ready
        | s -> `output s)

  let trim (specification: [`beginning of int|`ending of int]) =
    let records =  Queue.create () in
    let name =
      sprintf "(fastq_trimmer %s)"
        (match specification with
        | `beginning i -> sprintf "B:%d" i
        | `ending i -> sprintf "E:%d" i) in
    Biocaml_transform.make_stoppable ~name ()
      ~feed:(fun r -> Queue.enqueue records r)
      ~next:(fun stopped ->
        begin match Queue.dequeue records with
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

