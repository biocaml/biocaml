(** FASTA files.

    FASTA files are in the format:

    {v
    # comment
    # commment
    ...
    >header
    content
    >header
    content
    ...
    }

    where the content may span multiple lines.
    
    Header lines begin with the '>' character. It is often considered
    that all characters until the first whitespace define the {i name}
    of the content, and any characters beyond that define additional
    information in a format specific to the file provider.
    
    The content section is most often a sequence of characters
    denoting nucleotides, but also somtimes ASCII encoded quality
    scores, e.g. as supported by the {!module: PhredScore}
    module. Sometimes, the quality scores are provided as space
    separated integers.

    Thus, the FASTA format is really a family of formats with a fairly
    loose specification of the header and content formats. The only
    consistently followed meaning of the format is:

    - the file can begin with comment lines that begin with the '#'
    character and/or all white-space lines

    - the header lines begins with the '>' character, is followed
    optionally by whitespace, and then contains some string

    - each header line is followed by a content section, often just
    one line but allowed to span multiple lines

    - and this alternating pair of header/content lines can occur
    repeatedly.

    This module supports the full set of possibilities by providing
    two record types.

*)
open Batteries

exception Error of string

type record = string * string
    (** Header and content. Multiline content is concatenated into a
        single string. All characters other than newline are
        retained. *)

type recordi = string * int list
    (** Header and integers. Content is assumed to be space-separated
        integers, possibly spanning multiple lines. *)

val enum_input : IO.input -> Biocaml_comments.t * record Enum.t
  (** Returns comments and enumeration of fasta records in given input. *)

val enum_of_file : string -> Biocaml_comments.t * record Enum.t
  (** Returns comments and enumeration of fasta records in given path. *)

val enum_inputi : IO.input -> Biocaml_comments.t * recordi Enum.t
