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
  | `nothing_ready
  | `record of record ]
