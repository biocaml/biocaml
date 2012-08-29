(** FASTQ data. *)


type record = {
  name: string;
  sequence: string;
  comment: string;
  qualities: string;
}
with sexp
(** The type of FASTQ records. *)

type string_to_record_error =
[ `sequence_and_qualities_do_not_match of Biocaml_pos.t * string * string
| `wrong_comment_line of Biocaml_pos.t * string
| `wrong_name_line of Biocaml_pos.t * string
| `incomplete_input of Biocaml_pos.t * string list * string option]
with sexp
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

val string_of_parser_error: string_to_record_error -> string
(** Transform a [parser_error] to a human-readable string. *)

module Transform: sig
  val string_to_record:
    ?filename:string -> unit ->
    (string, (record, string_to_record_error) Core.Result.t) Biocaml_transform.t
(** Create a full {i stoppable} [Biocaml_transform.t] from arbitrary strings to
    [record] values.*)

  val record_to_string: unit -> (record, string) Biocaml_transform.t
(** Create a full {i stoppable} [Biocaml_transform.t] from [record]
    values to strings. *)

  val trim:
    [ `beginning of int | `ending of int ] ->
    (record, (record, [`invalid_size of int]) Core.Result.t) Biocaml_transform.t
(** Create a full {i stoppable} [Biocaml_transform.t] that trims FASTQ
    records. *)
end




