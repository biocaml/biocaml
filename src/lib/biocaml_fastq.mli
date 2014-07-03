(** FASTQ data. *)
open Core.Std
open Biocaml_internal_utils

type item = {
  name: string;
  sequence: string;
  comment: string;
  qualities: string;
} with sexp


module Err : module type of Biocaml_fastq_error
exception Parse_error of Err.parsing
exception Err of Err.t


(** {2 Input/Output } *)

(** Parse an input-channel into a stream of [item] results. *)
val in_channel_to_item_stream :
  ?buffer_size:int ->
  ?filename:string ->
  in_channel ->
  (item, [> Err.parsing]) Result.t Stream.t

(** Returns a stream of [item]s. [Stream.next] will raise [Err _] in
    case of any error. *)
val in_channel_to_item_stream_exn :
  ?buffer_size:int ->
  ?filename:string ->
  in_channel ->
  item Stream.t

module MakeIO (Future : Future.S) : sig
  open Future

  val read : Reader.t -> item Or_error.t Pipe.Reader.t

  val read_file
    : ?buf_len:int
    -> string
    -> item Or_error.t Pipe.Reader.t Deferred.t

  val write : Writer.t -> item Pipe.Reader.t -> unit Deferred.t

  val write_file
    : ?perm:int
    -> ?append:bool
    -> string
    -> item Pipe.Reader.t
    -> unit Deferred.t

end
include module type of MakeIO(Future_std)


(** {2 Printing } *)

(** This function converts [item] values to strings that can be dumped
    to a file, i.e. they contain full-lines, including {i all}
    end-of-line characters. *)
val item_to_string: item -> string


(** {2 Parsing }

    Parsing functions. Mostly needed only internally. Each function takes:

    - [line] - The line to parse.

    - [pos] - Optional position of the line used in error
    reporting. The column should always be 1 because by definition a
    line starts at the beginning.

    Functions ending in _exn raise Parse_error.
*)

val name_of_line : ?pos:Pos.t -> Line.t -> string Or_error.t
val sequence_of_line : ?pos:Pos.t -> Line.t -> string
val comment_of_line : ?pos:Pos.t -> Line.t -> string Or_error.t

(** [qualities sequence line] parses given qualities [line] in the
    context of a previously parsed [sequence]. The [sequence] is
    needed to assure the correct number of quality scores are
    provided. If not provided, this check is omitted. *)
val qualities_of_line :
  ?pos:Pos.t ->
  ?sequence:string ->
  Line.t ->
  string Or_error.t


(** {2 Transforms } *)

module Transform: sig
  (** Lower-level transforms. *)

  val string_to_item:
    ?filename:string -> unit ->
    (string, (item, [> Err.parsing]) Result.t) Biocaml_transform.t
  (** Create a [Biocaml_transform.t] from arbitrary strings to
      [item] values.*)

  val item_to_string: unit -> (item, string) Biocaml_transform.t
  (** Create a [Biocaml_transform.t] from [item] values to strings. *)

  val trim:
    [ `beginning of int | `ending of int ] ->
    (item, (item, [> `invalid_size of int]) Result.t) Biocaml_transform.t
  (** Create a [Biocaml_transform.t] that trims FASTQ items. *)

  val fasta_pair_to_fastq:
    ?phred_score_offset:[ `offset33 | `offset64 ] ->
    unit ->
    (Biocaml_fasta.char_seq Biocaml_fasta.item *
       Biocaml_fasta.int_seq Biocaml_fasta.item,
     (item,
      [> Err.fasta_pair_to_fastq ]) Result.t)
      Biocaml_transform.t
  (** Create a transform that builds [item] records thanks
      to sequences from [Fasta.(char_seq item)] values
      and qualities converted from
      [Fasta.(int_seq item)] values. The default Phred score encoding
      is [`offset33] (like in {!Biocaml_phred_score}). *)

  val fastq_to_fasta_pair :
    ?phred_score_offset:[ `offset33 | `offset64 ] ->
    unit ->
    (item,
     (Biocaml_fasta.char_seq Biocaml_fasta.item *
        Biocaml_fasta.int_seq Biocaml_fasta.item,
      [> `cannot_convert_ascii_phred_score of string ]) Result.t)
      Biocaml_transform.t
  (** Create a transform that split a FASTQ item into to FASTA items
      (i.e. the inverse of {!fasta_pair_to_fastq}). *)

end
