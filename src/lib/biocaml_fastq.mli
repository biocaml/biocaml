(** FASTQ data. *)


type record = {
  name: string;
  sequence: string;
  comment: string;
  qualities: string;
} 

type parser
val parser: unit -> parser
  
val feed_line: parser -> string -> unit

val feed_string: parser -> string -> unit

val next :
  parser ->
  [> `error of
      [> `sequence_and_qualities_do_not_match of int * string * string
      | `wrong_comment_line of int * string
      | `wrong_name_line of int * string ]
  | `not_ready
  | `record of record ]

    
type printer

val printer: ?buffer:[`clear of int | `reset of int] -> unit -> printer
  
val feed_record: printer -> record -> unit

val get_string: printer -> string

(**  {3 Classy Interface } *)

class type ['input, 'output, 'error] transform =
object
  method feed: 'input -> unit
  method next: [ `output of 'output | `not_ready | `error of 'error ]
end

type parser_error =
[ `sequence_and_qualities_do_not_match of int * string * string
| `wrong_comment_line of int * string
| `wrong_name_line of int * string ]

class fastq_parser: [string, record, parser_error] transform

type empty
class fastq_printer: [record, string, empty] transform
  
class trimmer:
  [ `beginning of int | `ending of int ] ->
    [record, record, [`invalid_size of int]] transform


val compose:
  ( 'input_left, 'middle, 'error_left) transform ->
  ( 'middle, 'output_right, 'error_right) transform ->
  ( 'input_left, 'output_right, [ `left of 'error_left | `right of 'error_right ] )
    transform
    
val mix :
  ( 'input_left, 'output_left, 'error_left) transform ->
  ( 'input_right, 'output_right, 'error_right) transform ->
  f:('output_left -> 'output_right -> 'output_f) ->
  ( 'input_left * 'input_right, 'output_f,
    [ `left of 'error_left | `right of 'error_right ] ) transform
  

(** Non-cooperative functions. *)

exception Invalid of string


val enum_input : BatIO.input -> record BatEnum.t
(** Returns enumeration of fastq records in given input. *)

