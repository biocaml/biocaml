(** SAM files. Documentation here assumes familiarity with the {{:
    http://samtools.github.io/hts-specs/SAMv1.pdf } SAM
    specification}. *)
open! Import

module Header : sig
  module Type : sig
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

    val print : t -> string
  end

  module Tag_value : sig
    (** A tag-value pair comprising the content of header items. Tag-value
      pairs occur in other places too, but this type is specifically for
      those in the header. *)
    type t = private string * string [@@deriving sexp]

    val print : t -> string
  end

  module Sort_order : sig
    type t =
      [ `Unknown
      | `Unsorted
      | `Query_name
      | `Coordinate
      ]
    [@@deriving sexp]

    val parse : string -> t Or_error.t
    val print : t -> string
  end

  module Group_order : sig
    type t =
      [ `None
      | `Query
      | `Reference
      ]
    [@@deriving sexp]

    val parse : string -> t Or_error.t
    val print : t -> string
  end

  val parse_header_version : string -> string Or_error.t
  val print_header_version : string -> string

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

    val parse : Tag_value.t list -> t Or_error.t
    val print : t -> string
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

    val parse : Tag_value.t list -> t Or_error.t
    val print : t -> string
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

    val parse : string -> t Or_error.t
    val print : t -> string
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

    val parse : Tag_value.t list -> t Or_error.t
    val print : t -> string
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

    val parse : Tag_value.t list -> t Or_error.t
    val print : t -> string
  end

  module Header_item : sig
    type t =
      private
      [< `HD of Header_line.t
      | `SQ of Ref_seq.t
      | `RG of Read_group.t
      | `PG of Program.t
      | `CO of string
      | `Other of string * Tag_value.t list
      ]
    [@@deriving sexp]

    val parse : Line.t -> t Or_error.t
  end

  val print_other : string * Tag_value.t list -> string

  (**
     - [sort_order]: Guaranteed to be [None] if [version = None].

     - [ref_seqs]: List of @SQ items. Order matters; it dictates
     alignment sorting order when [sort_order = `Coordinate].

     - [read_groups]: Unordered list of @RG items.

     - [programs]: List of @PG lines. Currently unordered, but we should
     topologically sort.

     - [comments]: Unordered list of @CO lines.
  *)
  type t =
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

  val empty_header : t

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
    -> t Or_error.t
end

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

module Cigar_op : sig
  (** CIGAR operations. *)
  type t =
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

  val parse_cigar : string -> t list Or_error.t
  val print : t -> string
  val print_cigar : t list -> string

  (** {3 Low-level Optional field Parsers and Constructors} *)

  val cigar_op_alignment_match : int -> t Or_error.t
  val cigar_op_insertion : int -> t Or_error.t
  val cigar_op_deletion : int -> t Or_error.t
  val cigar_op_skipped : int -> t Or_error.t
  val cigar_op_soft_clipping : int -> t Or_error.t
  val cigar_op_hard_clipping : int -> t Or_error.t
  val cigar_op_padding : int -> t Or_error.t
  val cigar_op_seq_match : int -> t Or_error.t
  val cigar_op_seq_mismatch : int -> t Or_error.t
end

module Optional_field_value : sig
  (** The constructor encodes the TYPE and each carries its
    corresponding VALUE. *)
  type t =
    private
    [< `A of char
    | `i of Int64.t
    | `f of float
    | `Z of string
    | `H of string
    | `B of char * string list
    ]
  [@@deriving sexp]

  (** {3 Low-level Optional field Parsers and Constructors} *)

  val optional_field_value_A : char -> t Or_error.t
  val optional_field_value_i : Int64.t -> t
  val optional_field_value_f : float -> t
  val optional_field_value_Z : string -> t Or_error.t
  val optional_field_value_H : string -> t Or_error.t
  val optional_field_value_B : char -> string list -> t Or_error.t
  val parse : string -> t Or_error.t
end

module Optional_field : sig
  type t = private
    { tag : string
    ; value : Optional_field_value.t
    }
  [@@deriving sexp]

  val optional_field : string -> Optional_field_value.t -> t Or_error.t
  val parse : string -> t Or_error.t
  val print : t -> string
end

module Rnext : sig
  type t =
    private
    [< `Value of string
    | `Equal_to_RNAME
    ]
  [@@deriving sexp]

  val parse : string -> t option Or_error.t
  val print : t option -> string
end

module Alignment : sig
  (** For [cigar] and [qual], empty list indicates no value, i.e. '*',
    was given. *)
  type t = private
    { qname : string option (** QNAME *)
    ; flags : Flags.t (** FLAG *)
    ; rname : string option (** RNAME *)
    ; pos : int option (** POS *)
    ; mapq : int option (** MAPQ *)
    ; cigar : Cigar_op.t list (** CIGAR *)
    ; rnext : Rnext.t option (** RNEXT *)
    ; pnext : int option (** PNEXT *)
    ; tlen : int option (** TLEN *)
    ; seq : string option (** SEQ *)
    ; qual : Phred_score.t list (** QUAL *)
    ; optional_fields : Optional_field.t list
    }
  [@@deriving sexp]

  val alignment
    :  ?ref_seqs:(string, String.comparator_witness) Set.t
    -> ?qname:string
    -> flags:Flags.t
    -> ?rname:string
    -> ?pos:int
    -> ?mapq:int
    -> ?cigar:Cigar_op.t list
    -> ?rnext:Rnext.t
    -> ?pnext:int
    -> ?tlen:int
    -> ?seq:string
    -> ?qual:Phred_score.t list
    -> ?optional_fields:Optional_field.t list
    -> unit
    -> t Or_error.t

  val parse_qname : string -> string option Or_error.t
  val parse_flags : string -> Flags.t Or_error.t
  val parse_rname : string -> string option Or_error.t
  val parse_pos : string -> int option Or_error.t
  val parse_mapq : string -> int option Or_error.t
  val parse_pnext : string -> int option Or_error.t
  val parse_tlen : string -> int option Or_error.t
  val parse_seq : string -> string option Or_error.t
  val parse_qual : string -> Phred_score.t list Or_error.t

  val parse
    :  ?ref_seqs:(string, String.comparator_witness) Set.t
    -> Line.t
    -> t Or_error.t

  val print_qname : string option -> string
  val print_flags : Flags.t -> string
  val print_rname : string option -> string
  val print_pos : int option -> string
  val print_mapq : int option -> string
  val print_pnext : int option -> string
  val print_tlen : int option -> string
  val print_seq : string option -> string
  val print_qual : Phred_score.t list -> string
  val print : t -> string
end
