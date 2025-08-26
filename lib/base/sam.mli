(** SAM files. Documentation here assumes familiarity with the {{:
    http://samtools.github.io/hts-specs/SAMv1.pdf } SAM
    specification}. *)
open! Import

(** {2 Types} *)

(** {3 Header Types} *)

module Header_item_tag : sig
  (** Header item tags define the different types of header lines. The
    term "tag" in this context should not be confused with its use in
    "tag-value" pairs, which comprise the content of header items. *)
  type t =
    private
    [< `HD
    | `SQ
    | `RG
    | `PG
    | `CO
    | `Other of string
    ]
  [@@deriving sexp]

  val print_header_item_tag : t -> string
end

module Tag_value : sig
  (** A tag-value pair comprising the content of header items. Tag-value
      pairs occur in other places too, but this type is specifically for
      those in the header. *)
  type t = private string * string [@@deriving sexp]

  val print_tag_value : t -> string
end

module Sort_order : sig
  type t =
    [ `Unknown
    | `Unsorted
    | `Query_name
    | `Coordinate
    ]
  [@@deriving sexp]

  val parse_sort_order : string -> t Or_error.t
  val print_sort_order : t -> string
end

module Group_order : sig
  type t =
    [ `None
    | `Query
    | `Reference
    ]
  [@@deriving sexp]
end

module Header_line : sig
  (** @HD. A header consists of different types of lines. Confusingly, one of
      these types is called {i the} "header line", which is what this
      type refers to. It does not refer generically to any line within a
      header. *)
  type t =
    { version : string (** VN *)
    ; sort_order : Sort_order.t option (** SO *)
    ; group_order : Group_order.t option (** GO *)
    }
  [@@deriving sexp]
  (* FIXME: Make the type private. Removed temporarily to fix build. *)

  val header_line
    :  version:string
    -> ?sort_order:Sort_order.t
    -> ?group_order:Group_order.t
    -> unit
    -> t Or_error.t

  val parse_header_line : Tag_value.t list -> t Or_error.t
  val print_header_line : t -> string
end

module Ref_seq : sig
  (** @SQ. Reference sequence. *)
  type t = private
    { name : string (** SN *)
    ; length : int (** LN *)
    ; assembly : string option (** AS *)
    ; md5 : string option (** M5 *)
    ; species : string option (** SP *)
    ; uri : string option (** UR *)
    }
  [@@deriving sexp]

  val ref_seq
    :  name:string
    -> length:int
    -> ?assembly:string
    -> ?md5:string
    -> ?species:string
    -> ?uri:string
    -> unit
    -> t Or_error.t

  val parse_ref_seq : Tag_value.t list -> t Or_error.t
  val print_ref_seq : t -> string
end

module Platform : sig
  type t =
    [ `Capillary
    | `LS454
    | `Illumina
    | `Solid
    | `Helicos
    | `Ion_Torrent
    | `Pac_Bio
    ]
  [@@deriving sexp]

  val parse_platform : string -> t Or_error.t
  val print_platform : t -> string
end

module Read_group : sig
  (** @RG. *)
  type t = private
    { id : string (** ID *)
    ; seq_center : string option (** CN *)
    ; description : string option (** DS *)
    ; run_date : [ `Date of string | `Time of string ] option (** DT *)
    ; flow_order : string option (** FO *)
    ; key_seq : string option (** KS *)
    ; library : string option (** LB *)
    ; program : string option (** PG *)
    ; predicted_median_insert_size : int option (** PI *)
    ; platform : Platform.t option (** PL *)
    ; platform_unit : string option (** PU *)
    ; sample : string option (** SM *)
    }
  [@@deriving sexp]

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
    -> ?platform:Platform.t
    -> ?platform_unit:string
    -> ?sample:string
    -> unit
    -> t Or_error.t

  val parse_read_group : Tag_value.t list -> t Or_error.t
  val print_read_group : t -> string
end

module Program : sig
  (** @PG. *)
  type t = private
    { id : string (** ID *)
    ; name : string option (** PN *)
    ; command_line : string option (** CL *)
    ; previous_id : string option (** PP *)
    ; description : string option (** DS *)
    ; version : string option (** VN *)
    }
  [@@deriving sexp]

  val parse_program : Tag_value.t list -> t Or_error.t
  val print_program : t -> string
end

type header_item =
  private
  [< `HD of Header_line.t
  | `SQ of Ref_seq.t
  | `RG of Read_group.t
  | `PG of Program.t
  | `CO of string
  | `Other of string * Tag_value.t list
  ]
[@@deriving sexp]

(**
     - [sort_order]: Guaranteed to be [None] if [version = None].

     - [ref_seqs]: List of @SQ items. Order matters; it dictates
     alignment sorting order when [sort_order = `Coordinate].

     - [read_groups]: Unordered list of @RG items.

     - [programs]: List of @PG lines. Currently unordered, but we should
     topologically sort.

     - [comments]: Unordered list of @CO lines.
  *)
type header =
  { version : string option
  ; sort_order : Sort_order.t option
  ; group_order : Group_order.t option
  ; ref_seqs : Ref_seq.t list
  ; read_groups : Read_group.t list
  ; programs : Program.t list
  ; comments : string list
  ; others : (string * Tag_value.t list) list
  }
(* FIXME: Make the type private. Removed temporarily to fix build. *)

val empty_header : header

(** {3 Alignment Types} *)

module Flags : sig
  (** Flags are represented as a "bit map". *)
  type t = private int [@@deriving sexp]

  val of_int : int -> t Or_error.t
  val has_multiple_segments : t -> bool
  val each_segment_properly_aligned : t -> bool
  val segment_unmapped : t -> bool
  val next_segment_unmapped : t -> bool
  val seq_is_reverse_complemented : t -> bool
  val next_seq_is_reverse_complemented : t -> bool
  val first_segment : t -> bool
  val last_segment : t -> bool
  val secondary_alignment : t -> bool
  val not_passing_quality_controls : t -> bool
  val pcr_or_optical_duplicate : t -> bool
  val supplementary_alignment : t -> bool
end

(** CIGAR operations. *)
type cigar_op =
  private
  [< `Alignment_match of int
  | `Insertion of int
  | `Deletion of int
  | `Skipped of int
  | `Soft_clipping of int
  | `Hard_clipping of int
  | `Padding of int
  | `Seq_match of int
  | `Seq_mismatch of int
  ]
[@@deriving sexp]

(** The constructor encodes the TYPE and each carries its
    corresponding VALUE. *)
type optional_field_value =
  private
  [< `A of char
  | `i of Int64.t
  | `f of float
  | `Z of string
  | `H of string
  | `B of char * string list
  ]
[@@deriving sexp]

type optional_field = private
  { tag : string
  ; value : optional_field_value
  }
[@@deriving sexp]

type rnext =
  private
  [< `Value of string
  | `Equal_to_RNAME
  ]
[@@deriving sexp]

(** For [cigar] and [qual], empty list indicates no value, i.e. '*',
    was given. *)
type alignment = private
  { qname : string option (** QNAME *)
  ; flags : Flags.t (** FLAG *)
  ; rname : string option (** RNAME *)
  ; pos : int option (** POS *)
  ; mapq : int option (** MAPQ *)
  ; cigar : cigar_op list (** CIGAR *)
  ; rnext : rnext option (** RNEXT *)
  ; pnext : int option (** PNEXT *)
  ; tlen : int option (** TLEN *)
  ; seq : string option (** SEQ *)
  ; qual : Phred_score.t list (** QUAL *)
  ; optional_fields : optional_field list
  }
[@@deriving sexp]

(** {2 Low-level Parsers and Constructors} *)

(** {3 Low-level Header Parsers and Constructors} *)

val header
  :  ?version:string
  -> ?sort_order:Sort_order.t
  -> ?group_order:Group_order.t
  -> ?ref_seqs:Ref_seq.t list
  -> ?read_groups:Read_group.t list
  -> ?programs:Program.t list
  -> ?comments:string list
  -> ?others:(string * Tag_value.t list) list
  -> unit
  -> header Or_error.t

val parse_header_version : string -> string Or_error.t
val parse_header_item : Line.t -> header_item Or_error.t

(** {3 Low-level Optional field Parsers and Constructors} *)

val cigar_op_alignment_match : int -> cigar_op Or_error.t
val cigar_op_insertion : int -> cigar_op Or_error.t
val cigar_op_deletion : int -> cigar_op Or_error.t
val cigar_op_skipped : int -> cigar_op Or_error.t
val cigar_op_soft_clipping : int -> cigar_op Or_error.t
val cigar_op_hard_clipping : int -> cigar_op Or_error.t
val cigar_op_padding : int -> cigar_op Or_error.t
val cigar_op_seq_match : int -> cigar_op Or_error.t
val cigar_op_seq_mismatch : int -> cigar_op Or_error.t

(** {3 Low-level Optional field Parsers and Constructors} *)

val optional_field_value_A : char -> optional_field_value Or_error.t
val optional_field_value_i : Int64.t -> optional_field_value
val optional_field_value_f : float -> optional_field_value
val optional_field_value_Z : string -> optional_field_value Or_error.t
val optional_field_value_H : string -> optional_field_value Or_error.t
val optional_field_value_B : char -> string list -> optional_field_value Or_error.t
val optional_field : string -> optional_field_value -> optional_field Or_error.t
val parse_optional_field_value : string -> optional_field_value Or_error.t
val parse_optional_field : string -> optional_field Or_error.t

(** {3 Low-level Alignment Parsers and Constructors} *)

val alignment
  :  ?ref_seqs:(string, String.comparator_witness) Set.t
  -> ?qname:string
  -> flags:Flags.t
  -> ?rname:string
  -> ?pos:int
  -> ?mapq:int
  -> ?cigar:cigar_op list
  -> ?rnext:rnext
  -> ?pnext:int
  -> ?tlen:int
  -> ?seq:string
  -> ?qual:Phred_score.t list
  -> ?optional_fields:optional_field list
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
val parse_qual : string -> Phred_score.t list Or_error.t

val parse_alignment
  :  ?ref_seqs:(string, String.comparator_witness) Set.t
  -> Line.t
  -> alignment Or_error.t

(** {2 Low-level Printers} *)

(** {3 Low-level Header Printers} *)

val print_header_version : string -> string
val print_other : string * Tag_value.t list -> string

(** {3 Low-level Alignment Printers} *)

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
