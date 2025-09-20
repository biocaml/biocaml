(** FASTA files. Support for the FASTA file format.

    The FASTA format is not standardized. Our aim here is to support the format
    that is in common use in recent times. We define FASTA files as being in
    the format:

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
*)

type item = private
  { description : string
  ; sequence : string
  }
[@@deriving sexp]

val item : description:string -> sequence:string -> item

(** Parse a space separated list of integers. *)
val sequence_to_int_list : string -> (int list, [> `Msg of string ]) Result.t

(** An [item0] is more raw than [item]. It is useful for parsing files
    with large sequences because you get the sequence in smaller
    pieces.

    - [`Description _] - Single description line without the initial
    '>' nor final newline.

    - [`Partial_sequence _] - Multiple sequential partial sequences
    comprise the sequence of a single [item].
*)
type item0 =
  [ `Description of string
  | `Partial_sequence of string
  ]
[@@deriving sexp]

type parser_error = [ `Fasta_parser_error of int * string ] [@@deriving sexp]

(** Low-level parsing

    This module provides a function that can be used to convert a
    stream of strings (representing consecutive chunks of a FASTA
    file) into a valid sequence of low-level items ({!items0}). This
    representation is especially relevant to deal with very long
    sequences (like chromosome) in constant memory.
*)
module Parser0 : sig
  type state

  val initial_state : unit -> state
  val step : state -> string option -> (state * item0 list, [> parser_error ]) Result.t
end

val unparser0 : item0 -> string

(** High-level parsing

    This module provides a function that can be used to convert a
    stream of strings (representing consecutive chunks of a FASTA
    file) into a sequence of FASTA items.
*)
module Parser : sig
  type state

  val initial_state : unit -> state
  val step : state -> string option -> (state * item list, [> parser_error ]) Result.t
end

val unparser : item -> string
