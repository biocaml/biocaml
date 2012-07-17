open Biocaml_std

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
end

class trimmer (specification: [`beginning of int|`ending of int]) =
object
  val records =  Queue.create ()
  method feed r = Queue.push r records 
  method next: 
    [ `output of record | `not_ready | `error of [`invalid_size of int] ] =
    if Queue.length records > 0 then
      let r = Queue.pop records in
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
    else
      `not_ready

end

  
(* ************************************************************************** *)
(* Non-cooperative / Unix *)
  

exception Invalid of string

let to_tuple r = (r.name, r.sequence, r.comment, r.qualities)
  
let enum_input cin =
  let e = IO.lines_of cin in
  let parser = parser () in
  let open Enum in
  let get_or_raise e exn =
    match get e with
    | None -> raise exn
    | Some s -> s in
  let invalidf fmt = ksprintf (fun s -> raise (Invalid s)) fmt in
  let rec make_from_enum (e : string t) = make
    ~next:(fun () ->
      let a = get_or_raise e No_more_elements in
      let b =
        get_or_raise e
          (Invalid "expected sequence line after '@' line but reached end-of-input")
      in
      let c =
        get_or_raise e
          (Invalid "expected '+' line after sequence line but reached end-of-inpout")
      in
      let d =
        get_or_raise e
          (Invalid "expected quality score after '+' line but reached end-of-input")
      in
      feed_line parser a;
      feed_line parser b;
      feed_line parser c;
      feed_line parser d;
      begin match next parser with
      | `record o -> o
      | `not_ready -> raise (Invalid "Parser in wrong state: not_ready")
      | `error error ->
        let prpos = Biocaml_pos.to_string in
        begin match error with        
        | `sequence_and_qualities_do_not_match (pos, seq, qualities) ->
          invalidf "%s: sequence and qualities do not match: %d Vs %d."
            (prpos pos) (String.length seq) (String.length qualities)
        | `wrong_comment_line (pos, _) ->
          invalidf "%s: Wrong comment line." (prpos pos)
        | `wrong_name_line (pos, _) ->
          invalidf "%s: Wrong sequence-id line." (prpos pos)
        end
      end
    )
    
    ~count:(fun () ->
      let n = count e in
      if n mod 4 = 0 then n/4
      else raise (Invalid "underlying enum does not contain multiple of 4 items")
    )
    
    ~clone:(fun () -> make_from_enum (clone e))
  in
  make_from_enum e
(*
#require "biocaml";;
open Batteries;;
let i = File.open_in "01.fastq";;
let e = Biocaml_fastq.enum_input i;;
List.of_enum e;;

*)

    
