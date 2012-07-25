(** FASTQ data. *)


type record = {
  name: string;
  sequence: string;
  comment: string;
  qualities: string;
} 


type parser_error =
[ `sequence_and_qualities_do_not_match of Biocaml_pos.t * string * string
| `wrong_comment_line of Biocaml_pos.t * string
| `wrong_name_line of Biocaml_pos.t * string
| `incomplete_input of Biocaml_pos.t * string list * string option]

val string_of_parser_error: parser_error -> string
val next :
  Biocaml_transform.Line_oriented.parser ->
  [> `error of parser_error | `not_ready | `record of record ]

val printer:
  ?buffer:[`clear of int | `reset of int] -> unit ->
  record Biocaml_transform.Printer_queue.t

  
val fastq_parser:
  ?filename:string -> unit -> (string, record, parser_error) Biocaml_transform.t

val fastq_printer: unit -> (record, string, [>  ]) Biocaml_transform.t
  
val trimmer:
  [ `beginning of int | `ending of int ] ->
  (record, record, [`invalid_size of int]) Biocaml_transform.t


(** {3 Non-cooperative functions} *)

exception Error of parser_error 

val stream_parser: ?filename:string -> string Stream.t -> record Stream.t
(** Stream transformation for [Stream.t]. *)
