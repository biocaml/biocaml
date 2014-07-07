(** SAM files. Documentation here assumes familiarity with the {{:
    http://samtools.github.io/hts-specs/SAMv1.pdf } SAM
    specification}. *)
open Core.Std
open Biocaml_internal_utils

(******************************************************************************)
(** {2 Types} *)
(******************************************************************************)

(******************************************************************************)
(** {3 Header Types} *)
(******************************************************************************)
(** Header item tags define the different types of header lines. The
    term "tag" in this context should not be confused with its use in
    "tag-value" pairs, which comprise the content of header items. *)
type header_item_tag = private [>
| `HD | `SQ | `RG | `PG | `CO
| `Other of string
] with sexp

(** A tag-value pair comprising the content of header items. *)
type tag_value = private string * string
with sexp

type sort_order = [ `Unknown | `Unsorted | `Query_name | `Coordinate ]
with sexp

(** @HD. A header consists of different types of lines. Confusingly, one of
    these types is called {i the} "header line", which is what this
    type refers to. It does not refer generically to any line within a
    header. *)
type header_line = private {
  version : string; (** VN *)
  sort_order : sort_order option; (** SO *)
} with sexp

(** @SQ. Reference sequence. *)
type ref_seq = private {
  name : string; (** SN *)
  length : int; (** LN *)
  assembly : string option; (** AS *)
  md5 : string option; (** M5 *)
  species : string option; (** SP *)
  uri : string option; (** UR *)
} with sexp

type platform = [
| `Capillary | `LS454 | `Illumina | `Solid
| `Helicos | `Ion_Torrent | `Pac_Bio
] with sexp

(** @RG. *)
type read_group = private {
  id : string; (** ID *)
  seq_center : string option; (** CN *)
  description : string option; (** DS *)
  run_date : [`Date of Date.t | `Time of Time.t] option; (** DT *)
  flow_order : string option; (** FO *)
  key_seq : string option; (** KS *)
  library : string option; (** LB *)
  program : string option; (** PG *)
  predicted_median_insert_size : int option; (** PI *)
  platform : platform option; (** PL *)
  platform_unit : string option; (** PU *)
  sample : string option; (** SM *)
} with sexp

(** @PG. *)
type program = private {
  id : string; (** ID *)
  name : string option; (** PN *)
  command_line : string option; (** CL *)
  previous_id : string option; (** PP *)
  description : string option; (** DS *)
  version : string option; (** VN *)
} with sexp

type header_item = private [>
| `HD of header_line
| `SQ of ref_seq
| `RG of read_group
| `PG of program
| `CO of string
| `Other of string * tag_value list
] with sexp



(******************************************************************************)
(** {3 Alignment Types} *)
(******************************************************************************)
module Flags : sig

  (** Flags are represented as a "bit map". *)
  type t = private int
  with sexp

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

end

(** CIGAR operations. *)
type cigar_op = private [>
| `Alignment_match of int
| `Insertion of int
| `Deletion of int
| `Skipped of int
| `Soft_clipping of int
| `Hard_clipping of int
| `Padding of int
| `Seq_match of int
| `Seq_mismatch of int
] with sexp

(** The constructor encodes the TYPE and each carries its
    corresponding VALUE. *)
type optional_field_value = private [>
| `A of string
| `i of Int32.t
| `f of float
| `Z of string
| `H of string
| `B of char * string list
] with sexp

type optional_field = private {
  tag : string;
  value : optional_field_value
} with sexp

type rnext = [`Value of string | `Equal_to_RNAME]
with sexp

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
  qual: Biocaml_phred_score.t list; (** QUAL *)
  optional_fields : optional_field list;
} with sexp



(******************************************************************************)
(** {3 Main Item Type} *)
(******************************************************************************)
(** Every item in a SAM file is either a header item or an alignment
    record. *)
type item = [
| `Header_item of header_item
| `Alignment of alignment
] with sexp


(******************************************************************************)
(** {2 Input/Output } *)
(******************************************************************************)
module MakeIO (Future : Future.S) : sig
  open Future

  val read : ?start:Pos.t -> Reader.t -> item Or_error.t Pipe.Reader.t

  val read_file
    : ?buf_len:int
    -> string
    -> item Or_error.t Pipe.Reader.t Deferred.t

end
include module type of MakeIO(Future_std)



(******************************************************************************)
(** {2 Low-level Parsers and Constructors} *)
(** {3 Low-level Header Parsers and Constructors} *)
(******************************************************************************)
val header_line
  :  version:string
  -> ?sort_order:sort_order
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

val parse_header_item_tag : string -> header_item_tag Or_error.t
val parse_tag_value : string -> tag_value Or_error.t
val parse_sort_order : string -> sort_order Or_error.t
val parse_header_line : tag_value list -> header_line Or_error.t
val parse_ref_seq : tag_value list -> ref_seq Or_error.t
val parse_platform : string -> platform Or_error.t
val parse_read_group : tag_value list -> read_group Or_error.t
val parse_program : tag_value list -> program Or_error.t
val parse_header_item : Line.t -> header_item Or_error.t



(******************************************************************************)
(** {3 Low-level Alignment Parsers and Constructors} *)
(******************************************************************************)
val alignment
  :  ?qname : string
  -> flags : Flags.t
  -> ?rname : string
  -> ?pos : int
  -> ?mapq : int
  -> ?cigar : cigar_op list
  -> ?rnext : rnext
  -> ?pnext : int
  -> ?tlen : int
  -> ?seq: string
  -> ?qual : Biocaml_phred_score.t list
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
val parse_qual :  string -> Biocaml_phred_score.t list Or_error.t
val parse_optional_field : string -> optional_field Or_error.t
val parse_optional_fields : string list -> optional_field list Or_error.t
val parse_alignment : Line.t -> alignment Or_error.t



(******************************************************************************)
(** {3 Main Item Parser} *)
(******************************************************************************)
val parse_item : Line.t -> item Or_error.t
