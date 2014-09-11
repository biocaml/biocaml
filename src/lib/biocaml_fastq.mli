(** FASTQ files. The
    {{:http://en.wikipedia.org/wiki/FASTQ_format}FASTQ file format} is
    repeated sequence of 4 lines:

    {v
    @name
    sequence
    +comment
    qualities
    ...
    v}

    The name line begins with an @ character, which is omitted in the
    parsed {!item} type provided by this module. Any spaces after the
    @ are retained, but the specification implies that there shouldn't
    be any such spaces. Trailing whitespace is also retained since you
    should not normally have such files.

    The comment line, which begins with a +, is handled similarly. The
    purpose of the comment line is unclear and it is rarely
    used. Also, "comment" may not be the correct term for this line.

    The qualities line is returned as a plain string, but it is
    required to be decodable as either Phred or Solexa scores. Modules
    [Phred_score] and [Solexa_score] can be used to parse as needed.

    Older FASTQ files allowed the sequence and qualities strings to
    span multiple lines. This is discouraged and is not supported by
    this module.
*)
open Core.Std
open Biocaml_internal_utils

type item = {
  name: string;
  sequence: string;
  comment: string;
  qualities: string;
} with sexp


(******************************************************************************)
(** {2 Input/Output } *)
(******************************************************************************)
module MakeIO (Future : Future.S) : sig
  open Future

  val read : Reader.t -> item Or_error.t Pipe.Reader.t

  val write : Writer.t -> item Pipe.Reader.t -> unit Deferred.t

  val write_file
    : ?perm:int
    -> ?append:bool
    -> string
    -> item Pipe.Reader.t
    -> unit Deferred.t

end
include module type of MakeIO(Future_std)


(******************************************************************************)
(** {2 Low-level Printing} *)
(******************************************************************************)
(** This function converts [item] values to strings that can be dumped
    to a file, i.e. they contain full-lines, including {i all}
    end-of-line characters. *)
val item_to_string: item -> string


(******************************************************************************)
(** {2 Low-level Parsing} *)
(******************************************************************************)
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
