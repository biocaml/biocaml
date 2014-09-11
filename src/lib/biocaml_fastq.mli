(** FASTQ data. *)
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
(** {2 Printing} *)
(******************************************************************************)
(** This function converts [item] values to strings that can be dumped
    to a file, i.e. they contain full-lines, including {i all}
    end-of-line characters. *)
val item_to_string: item -> string


(******************************************************************************)
(** {2 Parsing} *)
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
