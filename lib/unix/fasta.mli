(** FASTA files. The FASTA family of file formats has different
    incompatible descriptions
    ({{:https://www.proteomecommons.org/tranche/examples/proteomecommons-fasta/fasta.jsp
    }1}, {{:http://zhanglab.ccmb.med.umich.edu/FASTA/}2},
    {{:http://en.wikipedia.org/wiki/FASTA_format}3},
    {{:http://blast.ncbi.nlm.nih.gov/blastcgihelp.shtml}4},
    etc.). Roughly FASTA files are in the format:

    {v
    # comment
    # comment
    ...
    >description
    sequence
    >description
    sequence
    ...
    v}

    Comment lines are allowed at the top of the file. Usually comments
    start with a '#' but sometimes with a ';' character. The {!fmt}
    properties allow configuring which is allowed during parsing and
    printing.

    Description lines begin with the '>' character. Various
    conventions are used for the content but there is no
    requirement. We simply return the string following the '>'
    character.

    Sequences are most often a sequence of characters denoting
    nucleotides or amino acids, and thus an [item]'s [sequence] field
    is set to a string. Sequences may span multiple lines.

    However, sequence lines sometimes are used to provide quality
    scores, either as space separated integers or as ASCII encoded
    scores. To support the former case, we provide the
    {!sequence_to_int_list} function. For the latter case, see modules
    [Phred_score] and [Solexa_score].

    FASTA files are used to provide both short sequences and very big
    sequences, e.g. a genome. In the latter case, the main API of this
    module, which returns each sequence as an in-memory string, might
    be too costly. Consider using instead the {!read0} function which
    does not merge multiple sequence lines into one string. This API
    is slightly more difficult to use but perhaps a worthwhile
    trade-off.

    Some FASTA files include very large sequences on a single
    line. This is discouraged and not well supported by this
    module. Functions in this module require memory proportional to
    the length of a line. Thus, a whole chromosomal sequence on a
    single line will consume a large amount of memory. This might not
    be a problem given the RAM on most computers.


    Format Specifiers:

    Variations in the format are controlled by the following settings,
    all of which have a default value. These properties are combined
    into the {!fmt} type for convenience and the defaults into
    {!default_fmt}.

    - [allow_sharp_comments]: Allow comment lines beginning with a '#'
    character. Default: true.

    - [allow_semicolon_comments]: Allow comment lines beginning with a
    ';' character. Default: false.

    Setting both [allow_sharp_comments] and [allow_semicolon_comments]
    allows both. Setting both to false disallows comment
    lines.

    - [allow_empty_lines]: Allow lines with only whitespace anywhere in
    the file. Default: false.

    - [comments_only_at_top]: Allow comments only at the top of the
    file. If false, comment lines can occur anywhere but only the ones
    at the top are returned. The rest are ignored. Default: true.

    - [max_line_length]: Require sequence lines to be shorter than given
    length. None means there is no restriction. Note this does not
    restrict the length of an [item]'s [sequence] field because this
    can span multiple lines. Default: None.

    - [alphabet]: Require sequence characters to be at most those in
    given string. None means any character is allowed. Default: None.
*)
open Core_kernel

(** A header is a list of comment lines. *)
type header = private string list

type item = private {
  description : string;
  sequence : string;
}

type fmt = {
  allow_sharp_comments : bool;
  allow_semicolon_comments : bool;
  allow_empty_lines : bool;
  comments_only_at_top : bool;
  max_line_length : int option;
  alphabet : string option;
}

val default_fmt : fmt

(** Parse a space separated list of integers. *)
val sequence_to_int_list : string -> int list Or_error.t


(******************************************************************************)
(** {2 Low-level Parsing} *)
(******************************************************************************)

(** An [item0] is more raw than [item]. It is useful for parsing files
    with large sequences because you get the sequence in smaller
    pieces.

    - [`Comment _] - Single comment line without the final
    newline. Initial comment char is retained.

    - [`Empty_line] - Got a line with only whitespace characters. The
    contents are not provided.

    - [`Description _] - Single description line without the initial
    '>' nor final newline.

    - [`Partial_sequence _] - Multiple sequential partial sequences
    comprise the sequence of a single [item].
*)
type item0 = private [<
| `Comment of string
| `Empty_line
| `Description of string
| `Partial_sequence of string
]

val parse_item0
  :  ?allow_sharp_comments:bool
  -> ?allow_semicolon_comments:bool
  -> ?allow_empty_lines:bool
  -> ?max_line_length:int
  -> ?alphabet:string
  -> Line.t
  -> item0 Or_error.t


(******************************************************************************)
(** {2 Input/Output } *)
(******************************************************************************)
val read0
  :  ?start:Pos.t
  -> ?allow_sharp_comments:bool
  -> ?allow_semicolon_comments:bool
  -> ?allow_empty_lines:bool
  -> ?max_line_length:int
  -> ?alphabet:string
  -> In_channel.t
  -> item0 Or_error.t Stream.t

val read
  :  ?start:Pos.t
  -> ?fmt:fmt
  -> In_channel.t
  -> (header * item Or_error.t Stream.t) Or_error.t

val with_file
  :  ?fmt:fmt
  -> string
  -> f:(header -> item Or_error.t Stream.t -> 'a Or_error.t)
  -> 'a Or_error.t
