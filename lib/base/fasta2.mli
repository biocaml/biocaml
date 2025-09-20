(** FASTA files. Support for the FASTA file format.

    The FASTA format is not standardized. Our aim here is to support the format
    that is in common use in recent times as follows:

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
    specifications exist for formats of the description itself, but the
    main parser returns the raw string, allowing users to call additional
    functions to further parse as needed for their use case. We do require
    the [description] to be non-empty.

    The line following the [description] is the start of the [sequence],
    which can span multiple lines. Generally the [sequence] should contain
    nucleotides or amino acids, but there are so many variations on what
    specific characters are allowed that we do not attempt to define or
    enforce any such rules. The main parser treats the sequence as a raw
    string, with the newline characters omitted. We require the [sequence]
    to be non-empty.

    The end of the sequence is marked by a line that begins with a '>',
    indicating the start of a new [description] and [sequence], or end-of-file
    for the last sequence. Thus, a FASTA file is logically a non-empty list
    of items, where each item is a pair of a [description] and a [sequence].

    We do not support several legacy features: initial lines starting with
    semicolon characters, empty lines between items, arbitrary spaces in
    sequences, or comment lines anywhere.

    The main parser operates in a streaming fashion so minimal memory is
    required to traverse a file. This is true even for FASTA files with
    very long lines (though the recommendation is that lines should not
    be longer than 80 characters, and most FASTA files follow this
    recommendation).

    The functions {!of_string} and {!of_lines} parse a full file in-memory
    and are a good choice for small files since they are easy to use. They
    may also be the right choice for large files if you have sufficient
    memory.
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

  type t

  val init : t
  val step : t -> [ `Some of string | `Eof ] -> (t * Item.t list, Error.t) Result.t
end
