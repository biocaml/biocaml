open Biocaml_internal_pervasives

type record = {
  name: string;
  sequence: string;
  comment: string;
  qualities: string;
} 


let next p =
  let open Biocaml_transform.Line_oriented in
  if queued_lines p < 4 then
    `not_ready
  else (
    let name_line    = next_line_exn p in
    if String.length name_line = 0 || name_line.[0] <> '@'
    then `error (`wrong_name_line (current_position p, name_line))
    else 
      let sequence     = next_line_exn p in
      let comment_line = next_line_exn p in
      if String.length comment_line = 0 || comment_line.[0] <> '+'
      then `error (`wrong_comment_line (current_position p, comment_line))
      else 
        let qualities    = next_line_exn p in
        if String.length sequence <> String.length qualities
        then `error (`sequence_and_qualities_do_not_match (current_position p,
                                                           sequence, qualities))
        else (
          `record {
            name = String.sub name_line 1 (String.length name_line - 1);
            comment = String.sub comment_line 1 (String.length comment_line - 1);
            sequence; qualities }
    ))
      

let printer =
  Biocaml_transform.Printer_queue.make ~to_string:(fun r ->
    sprintf "@%s\n%s\n+%s\n%s\n" r.name r.sequence r.comment r.qualities)


type parser_error =
[ `sequence_and_qualities_do_not_match of Biocaml_pos.t * string * string
| `wrong_comment_line of Biocaml_pos.t * string
| `wrong_name_line of Biocaml_pos.t * string
| `incomplete_input of Biocaml_pos.t * string list * string option]

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

open Biocaml_transform.Line_oriented 
let fastq_parser ?filename ():
    (string, record, parser_error) Biocaml_transform.transform =
object
  val parser = parser ?filename ()
  val mutable stopped = false
  method feed s =
    if not stopped then
      feed_string parser s
    else
      eprintf "fastq_parser: called feed after stop\n%!"
  method stop = stopped <- true
  method next =
    match next parser with
    | `record r -> `output r
    | `error e -> `error e
    | `not_ready ->
      if stopped then (
        match finish parser with
        | `ok -> `end_of_stream
        | `error (l, o) ->
          `error (`incomplete_input (current_position parser, l, o))
      ) else
        `not_ready
end
  
type empty
open Biocaml_transform.Printer_queue 
let fastq_printer (): (record, string, empty) Biocaml_transform.transform =
object
  val printer = printer ()
  method feed r = feed printer r
  method stop = ()
  method next =
    match (flush printer) with
    | "" -> `not_ready
    | s -> `output s
end

let trimmer (specification: [`beginning of int|`ending of int]) =
object
  val records =  Queue.create ()
  method feed r = Queue.enqueue records r
  method stop = ()
  method next = 
    begin match Queue.dequeue records with
    | Some r ->
      let rlgth = String.length r.sequence in
      begin match specification with
      | `beginning i when i < rlgth ->
        `output 
          { r with sequence = String.sub r.sequence ~pos:i ~len:(rlgth - i);
              qualities = String.sub r.qualities ~pos:i ~len:(rlgth - i) }
      | `ending i when i < rlgth ->
        `output 
          { r with sequence = String.sub r.sequence ~pos:0 ~len:(rlgth - i);
              qualities = String.sub r.qualities ~pos:0 ~len:(rlgth - i) }
      | _ ->
        `error (`invalid_size rlgth)
      end
    | None -> `not_ready
    end

end

  
(* ************************************************************************** *)
(* Non-cooperative / Unix *)


exception Error of parser_error 
let enum_parser ?filename e =
  let transfo = fastq_parser ?filename () in
  Biocaml_transform.enum_transformation (fun e -> Error e) transfo e
(*
#require "biocaml";;
open Batteries;;
let i = File.lines_of "01.fastq";;
let j = Enum.map  (sprintf "%s\n") i;;
let e = Biocaml_fastq.enum_parser j;;
List.of_enum e;;
*) 
