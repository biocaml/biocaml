(** FASTA files.

    The FASTA  family of file formats has different incompatible descriptions
    ({{:https://www.proteomecommons.org/tranche/examples/proteomecommons-fasta/fasta.jsp
    }1},
    {{:http://zhanglab.ccmb.med.umich.edu/FASTA/}2},
    {{:http://en.wikipedia.org/wiki/FASTA_format}3}, etc.).
    
    FASTA files are in the format:

    {v
    # comment
    # comment
    ...
    >header
    content
    >header
    content
    ...
    v}

    where the content may span multiple lines, and a ';' may be used
    instead of '#' to start comments..
    
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
*)

type 'a data = [
| `comment of string | `name of string
| `partial_sequence of 'a
]
(** The type of the items of FASTA stream data (either [string data]
    or [float list data]). *)

type parse_error = [
| `empty_line of Biocaml_pos.t
| `malformed_partial_sequence of string ]
(** The possible parsing errors. *)

val sequence_parser :
  ?filename:string ->
  ?pedantic:bool ->
  ?sharp_comments:bool ->
  ?semicolon_comments:bool ->
  unit ->
  (string, string data, parse_error) Biocaml_transform.t
(** Parse a stream of strings as a sequence FASTA file.
    The [filename] is used only for error messages. If [pedantic] is [true]
    (default) the parser will report more errors (empty lines, non
    standard characters). The comment format is set with
    [sharp_comments] and/or [semicolon_comments]. *)

val score_parser :
  ?filename:string ->
  ?pedantic:bool ->
  ?sharp_comments:bool ->
  ?semicolon_comments:bool ->
  unit ->
  (string, float list data, parse_error) Biocaml_transform.t
(** Parse a stream of strings as a sequence FASTA file.
    See [sequence_parser]. *)

type empty

val sequence_printer :
  ?comment_char:char ->
  unit ->
  (string data, string, empty) Biocaml_transform.t
(** Print sequences. If [comment_char] is [None] comments will be ignored. *)
  
val score_printer :
  ?comment_char:char ->
  unit ->
  (float list data, string, empty) Biocaml_transform.t
(** Print scores. If [comment_char] is [None] comments will be ignored. *)
  
