(** FASTQ data. *)


open Biocaml_transform

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

val next :
  Line_oriented.parser ->
  [> `error of parser_error | `not_ready | `record of record ]
    

val printer:
  ?buffer:[`clear of int | `reset of int] -> unit -> record Printer_queue.t


(**  {3 Classy Interface } *)
  

val fastq_parser: ?filename:string -> unit ->
  (string, record, parser_error) transform

type empty
val fastq_printer: unit -> (record, string, empty) transform
  
val trimmer:
  [ `beginning of int | `ending of int ] ->
  (record, record, [`invalid_size of int]) transform



(** {3 Non-cooperative functions} *)


exception Error of parser_error 
val enum_parser: ?filename:string -> string BatEnum.t -> record BatEnum.t
(** Stream transformation for [BatEnum.t]. *)
