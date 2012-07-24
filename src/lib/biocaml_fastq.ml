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
| `wrong_name_line of Biocaml_pos.t * string ]

open Biocaml_transform.Line_oriented 
class fastq_parser ?filename () =
object
  val parser = parser ?filename ()
  method feed s =
    feed_string parser s
  method next:  [ `output of record | `not_ready | `error of parser_error ] =
    match next parser with
    | `record r -> `output r
    | `error e -> `error e
    | `not_ready -> `not_ready
  method is_empty = is_empty parser
end
  
type empty
open Biocaml_transform.Printer_queue 
class fastq_printer =
object
  val printer = printer ()
  method feed r = feed printer r
  method next :  [ `output of string | `not_ready | `error of empty ] =
    match (flush printer) with
    | "" -> `not_ready
    | s -> `output s
  method is_empty = is_empty printer
end

class trimmer (specification: [`beginning of int|`ending of int]) =
object
  val records =  Queue.create ()
  method feed r = Queue.enqueue records r
  method next: 
    [ `output of record | `not_ready | `error of [`invalid_size of int] ] =
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
  method is_empty = Queue.is_empty records

end

  
(* ************************************************************************** *)
(* Non-cooperative / Unix *)


exception Error of parser_error 
let enum_parser ?filename e =
  let transfo = new fastq_parser ?filename () in
  Biocaml_transform.enum_transformation (fun e -> Error e) transfo e
(*
#require "biocaml";;
open Batteries;;
let i = File.lines_of "01.fastq";;
let j = Enum.map  (sprintf "%s\n") i;;
let e = Biocaml_fastq.enum_parser j;;
List.of_enum e;;
*) 
