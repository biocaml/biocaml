(** SAM files. Documentation here assumes familiarity with the {{:
    http://samtools.github.io/hts-specs/SAMv1.pdf } SAM
    specification}. *)
open Core_kernel

(******************************************************************************)
(** {2 Types} *)
(******************************************************************************)

(******************************************************************************)
(** {3 Header Types} *)
(******************************************************************************)

(** Header item tags define the different types of header lines. The
    term "tag" in this context should not be confused with its use in
    "tag-value" pairs, which comprise the content of header items. *)
type header_item_tag = private [<
| `HD | `SQ | `RG | `PG | `CO
| `Other of string
] [@@deriving sexp]

(** A tag-value pair comprising the content of header items. Tag-value
    pairs occur in other places too, but this type is specifically for
    those in the header. *)
type tag_value = private string * string
[@@deriving sexp]

type sort_order = [ `Unknown | `Unsorted | `Query_name | `Coordinate ]
[@@deriving sexp]

type group_order = [ `None | `Query | `Reference ]
[@@deriving sexp]

(** @HD. A header consists of different types of lines. Confusingly, one of
    these types is called {i the} "header line", which is what this
    type refers to. It does not refer generically to any line within a
    header. *)
type header_line = private {
  version : string; (** VN *)
  sort_order : sort_order option; (** SO *)
  group_order: group_order option; (** GO *)
} [@@deriving sexp]

(** @SQ. Reference sequence. *)
type ref_seq = private {
  name : string; (** SN *)
  length : int; (** LN *)
  assembly : string option; (** AS *)
  md5 : string option; (** M5 *)
  species : string option; (** SP *)
  uri : string option; (** UR *)
} [@@deriving sexp]

type platform = [
| `Capillary | `LS454 | `Illumina | `Solid
| `Helicos | `Ion_Torrent | `Pac_Bio
] [@@deriving sexp]

(** @RG. *)
type read_group = private {
  id : string; (** ID *)
  seq_center : string option; (** CN *)
  description : string option; (** DS *)
  run_date : [`Date of string | `Time of string] option; (** DT *)
  flow_order : string option; (** FO *)
  key_seq : string option; (** KS *)
  library : string option; (** LB *)
  program : string option; (** PG *)
  predicted_median_insert_size : int option; (** PI *)
  platform : platform option; (** PL *)
  platform_unit : string option; (** PU *)
  sample : string option; (** SM *)
} [@@deriving sexp]

(** @PG. *)
type program = private {
  id : string; (** ID *)
  name : string option; (** PN *)
  command_line : string option; (** CL *)
  previous_id : string option; (** PP *)
  description : string option; (** DS *)
  version : string option; (** VN *)
} [@@deriving sexp]

type header_item = private [<
| `HD of header_line
| `SQ of ref_seq
| `RG of read_group
| `PG of program
| `CO of string
| `Other of string * tag_value list
] [@@deriving sexp]

(**
   - [sort_order]: Guaranteed to be [None] if [version = None].

   - [ref_seqs]: List of @SQ items. Order matters; it dictates
   alignment sorting order when [sort_order = `Coordinate].

   - [read_groups]: Unordered list of @RG items.

   - [programs]: List of @PG lines. Currently unordered, but we should
   topologically sort.

   - [comments]: Unordered list of @CO lines.
*)
type header = private {
  version : string option;
  sort_order : sort_order option;
  group_order : group_order option;
  ref_seqs : ref_seq list;
  read_groups : read_group list;
  programs : program list;
  comments : string list;
  others : (string * tag_value list) list;
}

val empty_header : header


(******************************************************************************)
(** {3 Alignment Types} *)
(******************************************************************************)
module Flags : sig

  (** Flags are represented as a "bit map". *)
  type t = private int
  [@@deriving sexp]

  val of_int: int -> t Or_error.t

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
  val supplementary_alignment          : t -> bool
end

(** CIGAR operations. *)
type cigar_op = private [<
  | `Alignment_match of int
  | `Insertion of int
  | `Deletion of int
  | `Skipped of int
  | `Soft_clipping of int
  | `Hard_clipping of int
  | `Padding of int
  | `Seq_match of int
  | `Seq_mismatch of int
] [@@deriving sexp]

(** The constructor encodes the TYPE and each carries its
    corresponding VALUE. *)
type optional_field_value = private [<
| `A of char
| `i of Int64.t
| `f of float
| `Z of string
| `H of string
| `B of char * string list
] [@@deriving sexp]

type optional_field = private {
  tag : string;
  value : optional_field_value
} [@@deriving sexp]

type rnext = private [< `Value of string | `Equal_to_RNAME]
[@@deriving sexp]

(** For [cigar] and [qual], empty list indicates no value, i.e. '*',
    was given. *)
type alignment = private {
  qname : string option; (** QNAME *)
  flags : Flags.t; (** FLAG *)
  rname : string option; (** RNAME *)
  pos : int option; (** POS *)
  mapq : int option; (** MAPQ *)
  cigar : cigar_op list; (** CIGAR *)
  rnext : rnext option; (** RNEXT *)
  pnext : int option; (** PNEXT *)
  tlen : int option; (** TLEN *)
  seq: string option; (** SEQ *)
  qual: Phred_score.t list; (** QUAL *)
  optional_fields : optional_field list;
} [@@deriving sexp]



(******************************************************************************)
(** {2 Input/Output } *)
(******************************************************************************)
module MakeIO (Future : Future.S) : sig
  open Future

  val read
    :  ?start:Pos.t
    -> Reader.t
    -> (header * alignment Or_error.t Pipe.Reader.t) Or_error.t Deferred.t

  val write
    :  Writer.t
    -> ?header:header
    -> alignment Pipe.Reader.t
    -> unit Deferred.t

  val write_file
    :  ?perm:int
    -> ?append:bool
    -> string
    -> ?header:header
    -> alignment Pipe.Reader.t
    -> unit Deferred.t
end

include module type of MakeIO(Future_unix)



(******************************************************************************)
(** {2 Low-level Parsers and Constructors} *)
(******************************************************************************)

(******************************************************************************)
(** {3 Low-level Header Parsers and Constructors} *)
(******************************************************************************)
val header_line
  :  version:string
  -> ?sort_order:sort_order
  -> ?group_order:group_order
  -> unit
  -> header_line Or_error.t

val ref_seq
  :  name:string
  -> length:int
  -> ?assembly:string
  -> ?md5:string
  -> ?species:string
  -> ?uri:string
  -> unit
  -> ref_seq Or_error.t

(** The [run_date] string will be parsed as a Date.t or Time.t,
    whichever is possible. If it is a time without a timezone, local
    timezone will be assumed. *)
val read_group
  :  id:string
  -> ?seq_center:string
  -> ?description:string
  -> ?run_date:string
  -> ?flow_order:string
  -> ?key_seq:string
  -> ?library:string
  -> ?program:string
  -> ?predicted_median_insert_size:int
  -> ?platform:platform
  -> ?platform_unit:string
  -> ?sample:string
  -> unit
  -> read_group Or_error.t

val header
   : ?version:string
  -> ?sort_order:sort_order
  -> ?group_order:group_order
  -> ?ref_seqs : ref_seq list
  -> ?read_groups : read_group list
  -> ?programs : program list
  -> ?comments : string list
  -> ?others : (string * tag_value list) list
  -> unit
  -> header Or_error.t

val parse_header_item_tag : string -> header_item_tag Or_error.t
val parse_tag_value : string -> tag_value Or_error.t
val parse_header_version : string -> string Or_error.t
val parse_sort_order : string -> sort_order Or_error.t
val parse_header_line : tag_value list -> header_line Or_error.t
val parse_ref_seq : tag_value list -> ref_seq Or_error.t
val parse_platform : string -> platform Or_error.t
val parse_read_group : tag_value list -> read_group Or_error.t
val parse_program : tag_value list -> program Or_error.t
val parse_header_item : Line.t -> header_item Or_error.t
val parse_header : string -> header Or_error.t


(******************************************************************************)
(** {3 Low-level Optional field Parsers and Constructors} *)
(******************************************************************************)
val cigar_op_alignment_match : int -> cigar_op Or_error.t
val cigar_op_insertion : int -> cigar_op Or_error.t
val cigar_op_deletion : int -> cigar_op Or_error.t
val cigar_op_skipped : int -> cigar_op Or_error.t
val cigar_op_soft_clipping : int -> cigar_op Or_error.t
val cigar_op_hard_clipping : int -> cigar_op Or_error.t
val cigar_op_padding : int -> cigar_op Or_error.t
val cigar_op_seq_match : int -> cigar_op Or_error.t
val cigar_op_seq_mismatch : int -> cigar_op Or_error.t


(******************************************************************************)
(** {3 Low-level Optional field Parsers and Constructors} *)
(******************************************************************************)
val optional_field_value_A : char -> optional_field_value Or_error.t
val optional_field_value_i : Int64.t -> optional_field_value
val optional_field_value_f : float -> optional_field_value
val optional_field_value_Z : string -> optional_field_value Or_error.t
val optional_field_value_H : string -> optional_field_value Or_error.t
val optional_field_value_B : char -> string list -> optional_field_value Or_error.t

val optional_field
  :  string
  -> optional_field_value
  -> optional_field Or_error.t

val parse_optional_field_value : string -> optional_field_value Or_error.t
val parse_optional_field : string -> optional_field Or_error.t

(******************************************************************************)
(** {3 Low-level Alignment Parsers and Constructors} *)
(******************************************************************************)
val alignment
  :  ?ref_seqs : String.Set.t
  -> ?qname : string
  -> flags : Flags.t
  -> ?rname : string
  -> ?pos : int
  -> ?mapq : int
  -> ?cigar : cigar_op list
  -> ?rnext : rnext
  -> ?pnext : int
  -> ?tlen : int
  -> ?seq: string
  -> ?qual : Phred_score.t list
  -> ?optional_fields : optional_field list
  -> unit
  -> alignment Or_error.t

val parse_qname : string -> string option Or_error.t
val parse_flags : string -> Flags.t Or_error.t
val parse_rname : string -> string option Or_error.t
val parse_pos : string -> int option Or_error.t
val parse_mapq : string -> int option Or_error.t
val parse_cigar : string -> cigar_op list Or_error.t
val parse_rnext : string -> rnext option Or_error.t
val parse_pnext : string -> int option Or_error.t
val parse_tlen : string -> int option Or_error.t
val parse_seq : string -> string option Or_error.t
val parse_qual :  string -> Phred_score.t list Or_error.t
val parse_alignment : ?ref_seqs:String.Set.t -> Line.t -> alignment Or_error.t


(******************************************************************************)
(** {2 Low-level Printers} *)
(******************************************************************************)

(******************************************************************************)
(** {3 Low-level Header Printers} *)
(******************************************************************************)
val print_header_item_tag : header_item_tag -> string
val print_tag_value : tag_value -> string
val print_header_version : string -> string
val print_sort_order : sort_order -> string
val print_header_line : header_line -> string
val print_ref_seq : ref_seq -> string
val print_platform : platform -> string
val print_read_group : read_group -> string
val print_program : program -> string
val print_other : string * tag_value list -> string

(******************************************************************************)
(** {3 Low-level Alignment Printers} *)
(******************************************************************************)
val print_qname : string option -> string
val print_flags : Flags.t -> string
val print_rname : string option -> string
val print_pos : int option -> string
val print_mapq : int option -> string
val print_cigar_op : cigar_op -> string
val print_cigar : cigar_op list -> string
val print_rnext : rnext option -> string
val print_pnext : int option -> string
val print_tlen : int option -> string
val print_seq : string option -> string
val print_qual : Phred_score.t list -> string
val print_optional_field : optional_field -> string
val print_alignment : alignment -> string
