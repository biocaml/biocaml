(** SAM files and SAM-alignements high-level representation. *)
open Core.Std
open Biocaml_internal_utils

(** {2 Basic Types} *)

(** {3 Low-Level Items} *)

type raw_alignment = {
  qname : string;
  flag : int;
  rname : string;
  pos : int;
  mapq : int;
  cigar : string;
  rnext : string;
  pnext : int;
  tlen : int;
  seq : string;
  qual : string;
  optional : (string * char * string) list
}
(** The contents of an alignment line. *)

type raw_item = [
| `comment of string
| `header of string * (string * string) list
| `alignment of raw_alignment
]
(** The "items" of a parsed SAM file stream. *)

(** {3 High-Level Items} *)

type reference_sequence = {
  ref_name: string;
  ref_length: int;
  ref_assembly_identifier: string option;
  ref_checksum: string option;
  ref_species: string option;
  ref_uri: string option;
  ref_unknown: (string * string) list;
}
(** Definition of a reference sequence. *)

val reference_sequence :
  ?assembly_identifier:string ->
  ?checksum:string ->
  ?species:string ->
  ?uri:string ->
  ?unknown_data:(string * string) list ->
  string -> int -> reference_sequence
(** Create a reference sequence. *)

module Flags : sig
  (** Manipulate the alignment flags. *)

  type t = private int
  (** Flags are represented as “bit map”. *)

  val of_int: int -> t

  val has_multiple_segments            : t -> bool
  val each_segment_properly_aligned    : t -> bool
  val segment_unmapped                 : t -> bool
  val next_segment_unmapped            : t -> bool
  val seq_is_reverse_complemented      : t -> bool
  val next_seq_is_reverse_complemented : t -> bool
  val first_segment                    : t -> bool
  val last_segment                     : t -> bool
  val secondary_alignment              : t -> bool
  val not_passing_quality_controls     : t -> bool
  val pcr_or_optical_duplicate         : t -> bool

  include Sexpable.S with type t := t

end

type cigar_op = [
| `D of int
| `Eq of int
| `H of int
| `I of int
| `M of int
| `N of int
| `P of int
| `S of int
| `X of int ]
(** CIGAR operations. *)

type optional_content_value = [
| `array of char * optional_content_value array
| `char of char
| `float of float
| `int of int
| `string of string ]
(** Meta-value used to store “optional content”. *)

type optional_content = (string * char * optional_content_value) list
(** Alignment optional content. *)

type alignment = {
  query_template_name: string;
  flags: Flags.t;
  reference_sequence: [ `reference_sequence of reference_sequence
                      | `none
                      | `name of string ];
  position: int option;
  mapping_quality: int option;
  cigar_operations: cigar_op array;

  next_reference_sequence: [`qname | `none | `name of string
                 | `reference_sequence of reference_sequence ];
  next_position: int option;

  template_length: int option;

  sequence: [ `string of string | `reference | `none];
  quality: Biocaml_phred_score.t array;
  optional_content: optional_content;
}
(** High-level representation of a parsed alignment. *)

type item = [
| `comment of string
| `header_line of
    string * [`unknown | `unsorted | `queryname | `coordinate ] *
      (string * string) list
| `reference_sequence_dictionary of reference_sequence array
| `header of string * (string * string) list
| `alignment of alignment
]
(** High-level representation of a parsed entity. *)

(** {2 Error Types} *)

module Error : sig
  (** The possible errors. *)

  type optional_content_parsing = [
  | `wrong_optional of (string * char * string) list *
      [ `not_a_char of string
      | `not_a_float of string
      | `not_an_int of string
      | `unknown_type of char
      | `wrong_array of
          [ `not_a_char of string
          | `not_a_float of string
          | `not_an_int of string
          | `wrong_type of string
          | `unknown_type of char
          ]
      | `wrong_type of string
      ]
  ]
  (** Errors which can happen while parsing optional content. *)

  type string_to_raw = [
  | `incomplete_input of Pos.t * string list * string option
  | `invalid_header_tag of Pos.t * string
  | `invalid_tag_value_list of Pos.t * string list
  | `not_an_int of Pos.t * string * string
  | `wrong_alignment of Pos.t * string
  | `wrong_optional_field of Pos.t * string
  ]
  (** The possible errors one can get while parsing SAM files. *)

  type raw_to_item = [
  | `comment_after_end_of_header of int * string
  | `duplicate_in_reference_sequence_dictionary of reference_sequence array
  | `header_after_end_of_header of int * (string * (string * string) list)
  | `header_line_not_first of int
  | `header_line_without_version of (string * string) list
  | `header_line_wrong_sorting of string
  | `missing_ref_sequence_length of (string * string) list
  | `missing_ref_sequence_name of (string * string) list
  | `wrong_cigar_text of string
  | `wrong_flag of raw_alignment
  | `wrong_mapq of raw_alignment
  | `wrong_phred_scores of raw_alignment
  | `wrong_pnext of raw_alignment
  | `wrong_pos of raw_alignment
  | `wrong_qname of raw_alignment
  | `wrong_ref_sequence_length of (string * string) list
  | `wrong_tlen of raw_alignment
  | optional_content_parsing
  ]
  (** The possible errors one can get while lifting SAM raw_items to
      higher-level representations. (Note: [raw_to_item]
      explicitly contains [optional_content_parsing] but OCamldoc
      pastes it inline) *)

  type item_to_raw = [
    `wrong_phred_scores of alignment
  ]
  (** The error that may happen while downgrading the
      higher-level represtation of an alignment. *)

  type parse = [
  | string_to_raw
  | raw_to_item
  ]
  (** All possible parsing errors. It is defined as: {[
      type parse = [
      | string_to_raw
      | raw_to_item
      ]
      ]}*)

  type t = parse
  (** The union of all possible errors. *)

  (** {3 S-Expressions conversions for Errors} *)

  val optional_content_parsing_of_sexp : Sexplib.Sexp.t -> optional_content_parsing
  val sexp_of_optional_content_parsing : optional_content_parsing -> Sexplib.Sexp.t
  val string_to_raw_of_sexp : Sexplib.Sexp.t -> string_to_raw
  val sexp_of_string_to_raw : string_to_raw -> Sexplib.Sexp.t
  val raw_to_item_of_sexp : Sexplib.Sexp.t -> raw_to_item
  val sexp_of_raw_to_item : raw_to_item -> Sexplib.Sexp.t
  val item_to_raw_of_sexp : Sexplib.Sexp.t -> item_to_raw
  val sexp_of_item_to_raw : item_to_raw -> Sexplib.Sexp.t
  val parse_of_sexp : Sexplib.Sexp.t -> parse
  val sexp_of_parse : parse -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> parse
  val sexp_of_t : parse -> Sexplib.Sexp.t

end

exception Error of  Error.t
(** The only exception raised by [*_exn] functions in this module. *)

(** {2 Stream functions } *)

val in_channel_to_item_stream : ?buffer_size:int -> ?filename:string -> in_channel ->
  (item, [> Error.parse]) Result.t Stream.t
(** Parse an input-channel into a stream of high-level items. *)

val in_channel_to_raw_item_stream : ?buffer_size:int -> ?filename:string -> in_channel ->
  (raw_item, [> Error.parse]) Result.t Stream.t
(** Parse an input-channel into a stream of low-level (“raw”) items. *)

val in_channel_to_item_stream_exn : ?buffer_size:int -> ?filename:string -> in_channel ->
  item Stream.t
(** Like in_channel_to_item_stream but each call to [Stream.next] may
    raise [Error _] *)

val in_channel_to_raw_item_stream_exn : ?buffer_size:int -> ?filename:string -> in_channel ->
  raw_item Stream.t
(** Like in_channel_to_raw_item_stream but each call to [Stream.next] may
    raise [Error _] *)


(** {2 Low-level partial parsing} *)


(** Here we expose functions used both in {!Biocaml_sam.Transform} and
      {!Biocaml_bam.Transform} for parsing.
      It can be ignored by most users but can be useful.
*)

(** Parse CIGAR operations from a string. *)
val parse_cigar_text: string ->
  (cigar_op array, [> `wrong_cigar_text of string ]) Result.t


(** Parse optional content from a “tokenized” string. *)
val parse_optional_content: (string * char * string) list ->
  (optional_content, [> Error.optional_content_parsing]) Result.t

(** Parse a header line form a string. The first argument is used to
    pass the location to the error values
    (c.f. {!type:Error.string_to_raw}). *)
val parse_header_line:
  'a -> string ->
  ([> `comment of string
   | `header of string * (string * string) list ],
   [> `invalid_header_tag of 'a * string
   | `invalid_tag_value_list of 'a * string list ]) Result.t

(** Parse a header line into a more detailed type. *)
val expand_header_line:
  (string * string) list ->
  ([> `header_line of
        string * [ `coordinate | `queryname | `unknown | `unsorted ] *
          (string * string) list ],
   [> `header_line_without_version of (string * string) list
   | `header_line_wrong_sorting of string ]) Result.t

(** {2 Low-level Transforms} *)

module Transform: sig

  (** Low-level, threading-model agnostic transforms
      (c.f. {!Biocaml_transform}). *)

  val string_to_raw: ?filename:string -> unit ->
    (string, (raw_item, [> Error.string_to_raw]) Result.t) Biocaml_transform.t
  (** Create a parsing "stoppable" transform. *)

  val raw_to_string: unit ->
    (raw_item, string) Biocaml_transform.t
  (** Create a printing "stoppable" transform. *)

  val raw_to_item: unit ->
    (raw_item, (item,  [> Error.raw_to_item]) Result.t) Biocaml_transform.t
  (** Create a transform that lifts [raw_item]s to [item]s *)

  val item_to_raw: unit ->
    (item, (raw_item, [> Error.item_to_raw]) Result.t) Biocaml_transform.t
  (** Create a transform that downgrades [item]s to [raw_item]s *)

end

(** {2 S-Expressions } *)

val cigar_op_of_sexp : Sexplib.Sexp.t -> cigar_op
val sexp_of_cigar_op : cigar_op -> Sexplib.Sexp.t
val optional_content_value_of_sexp : Sexplib.Sexp.t -> optional_content_value
val sexp_of_optional_content_value : optional_content_value -> Sexplib.Sexp.t
val optional_content_of_sexp : Sexplib.Sexp.t -> optional_content
val sexp_of_optional_content : optional_content -> Sexplib.Sexp.t
val alignment_of_sexp : Sexplib.Sexp.t -> alignment
val sexp_of_alignment : alignment -> Sexplib.Sexp.t
val item_of_sexp : Sexplib.Sexp.t -> item
val sexp_of_item : item -> Sexplib.Sexp.t
