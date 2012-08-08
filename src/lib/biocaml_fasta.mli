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
| `incomplete_input of Biocaml_pos.t * string list * string option
| `malformed_partial_sequence of string ]
(** The possible parsing errors. *)

module Excn : sig
  exception Parse_error of [ parse_error | `unnamed_sequence of string ]

  val sequence_stream_of_in_channel :
    ?filename:string ->
    ?pedantic:bool ->
    ?sharp_comments:bool ->
    ?semicolon_comments:bool ->
    in_channel ->
    (string * string) Stream.t
      (** [sequence_stream_of_file file] returns a stream of [(name,
          sequence)] pairs. Initial comments are not provided. @raise
          Failure in case of any errors. *)
end

val sequence_parser :
  ?filename:string ->
  ?pedantic:bool ->
  ?sharp_comments:bool ->
  ?semicolon_comments:bool ->
  unit ->
  (string, string data, parse_error) Biocaml_transform.t
(** Parse a stream of strings as a sequence FASTA file.
    The [filename] is used only for error messages. If [pedantic] is [true]
    (default) the parser will report more errors (Biocaml_transform.no_error lines, non
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

val sequence_printer :
  ?comment_char:char ->
  unit ->
  (string data, string, Biocaml_transform.no_error) Biocaml_transform.t
(** Print sequences. If [comment_char] is [None] comments will be ignored. *)
  
val score_printer :
  ?comment_char:char ->
  unit ->
  (float list data, string, Biocaml_transform.no_error) Biocaml_transform.t
(** Print scores. If [comment_char] is [None] comments will be ignored. *)
  
val sequence_aggregator:
  unit -> 
  (string data, string * string, [ `unnamed_sequence of string ]) Biocaml_transform.t
(** Aggregate a stream of FASTA [string data] into a [(name, sequence)] stream.
    The error [`unnamed_sequence _] means that the file did start with
    the name of a sequence. *)

val score_aggregator:
  unit -> 
  (float list data,
   string * float list,
   [ `unnamed_sequence of float list ]) Biocaml_transform.t
(** Like [sequence_aggregator] but for [float list data]. *)

val sequence_slicer: ?line_width:int -> unit ->
  (string * string, string data, Biocaml_transform.no_error) Biocaml_transform.t
(** Cut a stream of [(name, sequence)] into a stream of [string data]
    where line are cut at [line_width] characters (default 80). *)

val score_slicer: ?group_by:int -> unit ->
  (string * float list, float list data, Biocaml_transform.no_error)
    Biocaml_transform.t
(** Cut a stream of [(name, scores)] into a stream of [float list data]
    where lists are cut at [group_by] numbers (default 10). *)

