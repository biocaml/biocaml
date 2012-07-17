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
  
open Biocaml_transform
                          
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



(** Non-cooperative functions. *)

exception Invalid of string


val enum_input : BatIO.input -> record BatEnum.t
(** Returns enumeration of fastq records in given input. *)

