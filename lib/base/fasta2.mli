(** FASTA files. Support for the FASTA file format.

    The FASTA format has evolved over time. Our aim here is to support
    the format that is in common use in recent times as follows:

    {v
    >description
    sequence
    >description
    sequence
    ...
    v}

    The first character in the file must be a '>' character (empty files
    are not valid). Everything following the '>' up until the end of the
    line is the [description], which the main parser returns as is (the
    initial '>' and final newline characters are excluded). Various
    specifications exist for formats of the description itself, but we
    simply return the raw string, allowing users to call additional
    functions to further parse as needed for their use case. We do require
    the [description] to be non-empty and disallow '>' characters within the
    description.

    The line following the [description] is the start of the [sequence],
    which can span multiple lines. Generally the [sequence] should contain
    nucleotides or amino acids, but there are so many variations on what
    specific characters are allowed that we do not attempt to define or
    enforce any such rules. We simply return the raw string, with the newline
    characters omitted. We do disallow '>' characters within the sequence.
    We do not support "\r\n" line endings and so disallow '\r' characters
    to avoid accidentally including them in the sequence.

    The end of the sequence is marked by a new line that begins with a '>',
    indicating the start of a new [description], or end-of-file
    for the last sequence. Thus, a FASTA file is logically a non-empty list
    of items, where each item is a pair of a [description] and a [sequence].

    We do not support several legacy features: initial lines starting with
    semicolon characters, empty lines between items, arbitrary spaces in
    sequences, or comment lines anywhere.

    The main {!Parser} operates in a streaming fashion so minimal memory is
    required to traverse a file. This is true even for FASTA files with
    very long lines (though the recommendation is that lines should not
    be longer than 80 characters, and most FASTA files follow this
    recommendation).

    The function {!of_string} is built on top of {!Parser} and parses a
    full file in-memory and is a good choice for small files since it is
    easy to use. It may also be the right choice for large files if you
    have sufficient memory and need to access the sequences repeatedly.
*)
open! Import

module Item : sig
  type t =
    { description : string
    ; sequence : string
    }
  [@@deriving sexp]
end

type t = Item.t list

module Error : sig
  type t = [ `Fasta_parser_error of int * string ] [@@deriving sexp]
end

(* [of_string content] parses the full [content] of a FASTA file in-memory. *)
val of_string : string -> (t, Error.t) Result.t

(** The [Parser] interface is harder to use but allows processing large files
    in a streaming fashion. *)
module Parser : sig
  module Item : sig
    type t =
      [ `Description of string
      | `Partial_sequence of string
      ]
    [@@deriving sexp]
  end

  type state

  val init : state

  (** [step st buf] parses the input [buf] and returns the items parsed from it.
      It also returns an updated parser state that must be fed back to the
      next call to [step]. Your last call to [step] must be followed by a call
      to {!eof}. See below. *)
  val step : state -> string -> (state * Item.t list, Error.t) Result.t

  (** [eof st] must be called on the parser state returned from your final call
      to {!step}. This checks whether the final state is valid for the end of a
      file. For example, this function will return an error if a description line
      has been parsed, but EOF is reached without a subsequent sequence line. *)
  val eof : state -> (unit, Error.t) Result.t
end
