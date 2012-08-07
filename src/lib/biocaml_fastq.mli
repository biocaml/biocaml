(** FASTQ data. *)


type record = {
  name: string;
  sequence: string;
  comment: string;
  qualities: string;
} 
(** The type of FASTQ records. *)

type parser_error =
[ `sequence_and_qualities_do_not_match of Biocaml_pos.t * string * string
| `wrong_comment_line of Biocaml_pos.t * string
| `wrong_name_line of Biocaml_pos.t * string
| `incomplete_input of Biocaml_pos.t * string list * string option]
(** Parsing errors: {ul
    {li [`sequence_and_qualities_do_not_match (_, _, _)] means that the
    sequence and quality score line lengths do not match for a given
    record.}
    {li [`wrong_name_line _] and [`wrong_comment_line _] mean that the
    "sequence name" lines do not start with ['@'] and ['+']
    respectively.}
    {li [`incomplete_input (_, sl, l)] means that the end of the stream
    has been reach with an incomplete record (containing [sl] lines and
    [l] potential unfinished line).}
    } *)

val string_of_parser_error: parser_error -> string
(** Transform a [parser_error] to a human-readable string. *)

val parser:
  ?filename:string -> unit -> (string, record, parser_error) Biocaml_transform.t
(** Create a full {i stoppable} [Biocaml_transform.t] from arbitrary strings to
    [record] values.*)

val printer: unit -> (record, string, [>  ]) Biocaml_transform.t
(** Create a full {i stoppable} [Biocaml_transform.t] from [record]
    values to strings. *)
  
val trimmer:
  [ `beginning of int | `ending of int ] ->
  (record, record, [`invalid_size of int]) Biocaml_transform.t
(** Create a full {i stoppable} [Biocaml_transform.t] that trims FASTQ
    records. *)

(** {3 Non-cooperative functions} *)

exception Error of parser_error 
(** The parsing-error exception. *)


val stream_parser: ?filename:string -> string Stream.t -> record Stream.t
(** Create a stream-transformation for parsing. *)
