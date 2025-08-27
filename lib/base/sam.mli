(** SAM files. Documentation here assumes familiarity with the {{:
    http://samtools.github.io/hts-specs/SAMv1.pdf } SAM
    specification}. *)
open! Import

module Header : sig
  module Type : sig
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

    val string_of_t : t -> string
  end

  module Tag_value : sig
    (** A tag-value pair comprising the content of header items. Tag-value
      pairs occur in other places too, but this type is specifically for
      those in the header. *)
    type t = private string * string [@@deriving sexp]

    val string_of_t : t -> string
  end

  module HD : sig
    module SO : sig
      type t =
        [ `Unknown
        | `Unsorted
        | `Query_name
        | `Coordinate
        ]
      [@@deriving sexp]

      val t_of_string : string -> t Or_error.t
      val string_of_t : t -> string
    end

    module GO : sig
      type t =
        [ `None
        | `Query
        | `Reference
        ]
      [@@deriving sexp]

      val t_of_string : string -> t Or_error.t
      val string_of_t : t -> string
    end

    module VN : sig
      type t = string [@@deriving sexp]

      val t_of_string : string -> t Or_error.t
      val string_of_t : t -> string
    end

    type t =
      { version : VN.t
      ; sort_order : SO.t option
      ; group_order : GO.t option
      }
    [@@deriving sexp]
    (* FIXME: Make the type private. Removed temporarily to fix build. *)

    val make
      :  version:VN.t
      -> ?sort_order:SO.t
      -> ?group_order:GO.t
      -> unit
      -> t Or_error.t

    val t_of_tag_value_list : Tag_value.t list -> t Or_error.t
    val string_of_t : t -> string
  end

  module SQ : sig
    type t = private
      { name : string (** SN *)
      ; length : int (** LN *)
      ; assembly : string option (** AS *)
      ; md5 : string option (** M5 *)
      ; species : string option (** SP *)
      ; uri : string option (** UR *)
      }
    [@@deriving sexp]

    val make
      :  name:string
      -> length:int
      -> ?assembly:string
      -> ?md5:string
      -> ?species:string
      -> ?uri:string
      -> unit
      -> t Or_error.t

    val t_of_tag_value_list : Tag_value.t list -> t Or_error.t
    val string_of_t : t -> string
  end

  module RG : sig
    module PL : sig
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

      val t_of_string : string -> t Or_error.t
      val string_of_t : t -> string
    end

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
      ; platform : PL.t option (** PL *)
      ; platform_unit : string option (** PU *)
      ; sample : string option (** SM *)
      }
    [@@deriving sexp]

    (** The [run_date] string will be parsed as a Date.t or Time.t,
    whichever is possible. If it is a time without a timezone, local
    timezone will be assumed. *)
    val make
      :  id:string
      -> ?seq_center:string
      -> ?description:string
      -> ?run_date:string
      -> ?flow_order:string
      -> ?key_seq:string
      -> ?library:string
      -> ?program:string
      -> ?predicted_median_insert_size:int
      -> ?platform:PL.t
      -> ?platform_unit:string
      -> ?sample:string
      -> unit
      -> t Or_error.t

    val t_of_tag_value_list : Tag_value.t list -> t Or_error.t
    val string_of_t : t -> string
  end

  module PG : sig
    type t = private
      { id : string (** ID *)
      ; name : string option (** PN *)
      ; command_line : string option (** CL *)
      ; previous_id : string option (** PP *)
      ; description : string option (** DS *)
      ; version : string option (** VN *)
      }
    [@@deriving sexp]

    val t_of_tag_value_list : Tag_value.t list -> t Or_error.t
    val string_of_t : t -> string
  end

  module Other : sig
    type t = string * Tag_value.t list [@@deriving sexp]

    val string_of_t : t -> string
  end

  module Item : sig
    type t =
      private
      [< `HD of HD.t
      | `SQ of SQ.t
      | `RG of RG.t
      | `PG of PG.t
      | `CO of string
      | `Other of Other.t
      ]
    [@@deriving sexp]

    val t_of_line : Line.t -> t Or_error.t
  end

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
    ; sort_order : HD.SO.t option
    ; group_order : HD.GO.t option
    ; ref_seqs : SQ.t list
    ; read_groups : RG.t list
    ; programs : PG.t list
    ; comments : string list
    ; others : (string * Tag_value.t list) list
    }
  (* FIXME: Make the type private. Removed temporarily to fix build. *)

  val empty : t

  val make
    :  ?version:string
    -> ?sort_order:HD.SO.t
    -> ?group_order:HD.GO.t
    -> ?ref_seqs:SQ.t list
    -> ?read_groups:RG.t list
    -> ?programs:PG.t list
    -> ?comments:string list
    -> ?others:(string * Tag_value.t list) list
    -> unit
    -> t Or_error.t
end

module Qname : sig
  type t = string [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val string_of_t_option : t option -> string
end

module Flag : sig
  (** The FLAG field represents a "bit map" of flags (plural). We use the
      singular form for consistency with the SAM specification. *)
  type t = private int [@@deriving sexp]

  val of_int : int -> t Or_error.t
  val t_of_string : string -> t Or_error.t
  val string_of_t : t -> string
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

module Rname : sig
  type t = string [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val string_of_t_option : t option -> string
end

module Pos : sig
  type t = int [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val string_of_t_option : t option -> string
end

module Mapq : sig
  type t = int [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val string_of_t_option : t option -> string
end

module Cigar : sig
  module Op : sig
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

    val string_of_t : t -> string
    val alignment_match_of_int : int -> t Or_error.t
    val insertion_of_int : int -> t Or_error.t
    val deletion_of_int : int -> t Or_error.t
    val skipped_of_int : int -> t Or_error.t
    val soft_clipping_of_int : int -> t Or_error.t
    val hard_clipping_of_int : int -> t Or_error.t
    val padding_of_int : int -> t Or_error.t
    val seq_match_of_int : int -> t Or_error.t
    val seq_mismatch_of_int : int -> t Or_error.t
  end

  type t = Op.t list [@@deriving sexp]

  val t_of_string : string -> t Or_error.t
  val string_of_t : t -> string
end

module Rnext : sig
  type t =
    private
    [< `Value of string
    | `Equal_to_RNAME
    ]
  [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val string_of_t_option : t option -> string
end

module Pnext : sig
  type t = int [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val string_of_t_option : t option -> string
end

module Tlen : sig
  type t = int [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val string_of_t_option : t option -> string
end

module Seq : sig
  type t = string [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val string_of_t_option : t option -> string
end

module Qual : sig
  type t = Phred_score.t list [@@deriving sexp]

  val t_of_string : string -> t Or_error.t
  val string_of_t : t -> string
end

module Optional_field : sig
  module Value : sig
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

    val t_of_char_A : char -> t Or_error.t
    val t_of_int64_i : Int64.t -> t
    val t_of_float_f : float -> t
    val t_of_string_Z : string -> t Or_error.t
    val t_of_string_H : string -> t Or_error.t
    val t_of_char_string_list_B : char -> string list -> t Or_error.t
    val t_of_string : string -> t Or_error.t
  end

  type t = private
    { tag : string
    ; value : Value.t
    }
  [@@deriving sexp]

  val make : string -> Value.t -> t Or_error.t
  val t_of_string : string -> t Or_error.t
  val string_of_t : t -> string
end

module Alignment : sig
  (** For [cigar] and [qual], empty list indicates no value, i.e. '*',
    was given. *)
  type t = private
    { qname : Qname.t option (** QNAME *)
    ; flag : Flag.t (** FLAG *)
    ; rname : Rname.t option (** RNAME *)
    ; pos : Pos.t option (** POS *)
    ; mapq : int option (** MAPQ *)
    ; cigar : Cigar.t (** CIGAR *)
    ; rnext : Rnext.t option (** RNEXT *)
    ; pnext : Pnext.t option (** PNEXT *)
    ; tlen : Tlen.t option (** TLEN *)
    ; seq : Seq.t option (** SEQ *)
    ; qual : Qual.t (** QUAL *)
    ; optional_fields : Optional_field.t list
    }
  [@@deriving sexp]

  val make
    :  ?ref_seqs:(string, String.comparator_witness) Set.t
    -> ?qname:Qname.t
    -> flag:Flag.t
    -> ?rname:Rname.t
    -> ?pos:Pos.t
    -> ?mapq:int
    -> ?cigar:Cigar.t
    -> ?rnext:Rnext.t
    -> ?pnext:Pnext.t
    -> ?tlen:int
    -> ?seq:Seq.t
    -> ?qual:Qual.t
    -> ?optional_fields:Optional_field.t list
    -> unit
    -> t Or_error.t

  val t_of_line
    :  ?ref_seqs:(string, String.comparator_witness) Set.t
    -> Line.t
    -> t Or_error.t

  val string_of_t : t -> string
end
