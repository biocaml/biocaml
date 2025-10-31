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

    val to_string : t -> string
  end

  module Tag_value : sig
    (** A tag-value pair comprising the content of header items. Tag-value
      pairs occur in other places too, but this type is specifically for
      those in the header. *)
    type t = private string * string [@@deriving sexp]

    val to_string : t -> string
  end

  module HD : sig
    module VN : sig
      type t = private string [@@deriving sexp]

      val of_string : string -> t Or_error.t
      val to_string : t -> string
    end

    module SO : sig
      type t =
        [ `Unknown
        | `Unsorted
        | `Query_name
        | `Coordinate
        ]
      [@@deriving sexp]

      val of_string : string -> t Or_error.t
      val to_string : t -> string
    end

    module GO : sig
      type t =
        [ `None
        | `Query
        | `Reference
        ]
      [@@deriving sexp]

      val of_string : string -> t Or_error.t
      val to_string : t -> string
    end

    module SS : sig
      type t = private [ `coordinate | `queryname | `unsorted ] * string list
      [@@deriving sexp]

      val of_string : string -> t Or_error.t
      val to_string : t -> string
    end

    type t =
      { version : VN.t
      ; sort_order : SO.t option
      ; group_order : GO.t option
      ; sub_sort_order : SS.t option
      }
    [@@deriving sexp]

    val make
      :  version:VN.t
      -> ?sort_order:SO.t
      -> ?group_order:GO.t
      -> ?sub_sort_order:SS.t
      -> unit
      -> t

    val of_tag_value_list : Tag_value.t list -> t Or_error.t
    val to_string : t -> string
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

    val of_tag_value_list : Tag_value.t list -> t Or_error.t
    val to_string : t -> string
  end

  module RG : sig
    module PL : sig
      type t =
        [ `CAPILLARY
        | `DNBSEQ
        | `ELEMENT
        | `HELICOS
        | `ILLUMINA
        | `IONTORRENT
        | `LS454
        | `ONT
        | `PACBIO
        | `SINGULAR
        | `SOLID
        | `ULTIMA
        ]
      [@@deriving sexp]

      val of_string : string -> t Or_error.t
      val to_string : t -> string
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

    val of_tag_value_list : Tag_value.t list -> t Or_error.t
    val to_string : t -> string
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

    val of_tag_value_list : Tag_value.t list -> t Or_error.t
    val to_string : t -> string
  end

  module Other : sig
    type t = string * Tag_value.t list [@@deriving sexp]

    val to_string : t -> string
  end

  module Item : sig
    type t =
      [ `HD of HD.t
      | `SQ of SQ.t
      | `RG of RG.t
      | `PG of PG.t
      | `CO of string
      | `Other of Other.t
      ]
    [@@deriving sexp]

    (** [of_line line] parses the given [line] as a SAM header line.
        The [line] should not contain the final newline character. *)
    val of_line : string -> t Or_error.t

    (** [to_line x] converts the header item [x] to a SAM header line.
        The returned string will not contain the final newline character. *)
    val to_line : t -> string
  end

  (** List of header lines with guarantees:
     - The @HD line is the first item if it exists.
     - The SN fields of all @SQ lines (i.e. name field in SQ module
       above) are unique.
     - The order of @SQ lines is preserved as given in constructors.
       This is required by the SAM specification because it dictates
       alignment sorting order when [sort_order = `Coordinate].

     In addition to the @SQ lines, we preserve the order of all lines.
     Though not mandated by the SAM specification, this follows the
     principle of least surprise. *)
  type t = private Item.t list [@@deriving sexp]

  val of_items : Item.t list -> t Or_error.t
  val of_lines : string list -> t Or_error.t
  val hd : t -> HD.t option
  val version : t -> HD.VN.t option
  val sort_order : t -> HD.SO.t option
  val group_order : t -> HD.GO.t option
  val ref_seqs : t -> SQ.t list
  val read_groups : t -> RG.t list
  val programs : t -> PG.t list
  val comments : t -> string list
  val others : t -> Other.t list
end

module Qname : sig
  type t = private string [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val to_string_option : t option -> string
end

module Flag : sig
  (** The FLAG field represents a "bit map" of flags (plural). We use the
      singular form for consistency with the SAM specification. *)
  type t = private int [@@deriving sexp]

  val of_int : int -> t Or_error.t
  val of_string : string -> t Or_error.t
  val to_string : t -> string
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
  type t = private string [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val to_string_option : t option -> string
end

module Pos : sig
  type t = private int [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val to_string_option : t option -> string
end

module Mapq : sig
  type t = private int [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val to_string_option : t option -> string
end

module Cigar : sig
  module Op : sig
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

    val to_string : t -> string
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

  val of_string : string -> t Or_error.t
  val to_string : t -> string
end

module Rnext : sig
  type t =
    private
    [< `Value of string
    | `Equal_to_RNAME
    ]
  [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val to_string_option : t option -> string
end

module Pnext : sig
  type t = private int [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val to_string_option : t option -> string
end

module Tlen : sig
  type t = private int [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val to_string_option : t option -> string
end

module Seq : sig
  type t = private string [@@deriving sexp]

  val t_option_of_string : string -> t option Or_error.t
  val to_string_option : t option -> string
end

module Qual : sig
  type t = Phred_score.t list [@@deriving sexp]

  val of_string : string -> t Or_error.t
  val to_string : t -> string
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

    val of_char_A : char -> t Or_error.t
    val of_int64_i : Int64.t -> t
    val of_float_f : float -> t
    val of_string_Z : string -> t Or_error.t
    val of_string_H : string -> t Or_error.t
    val of_char_string_list_B : char -> string list -> t Or_error.t
    val of_string : string -> t Or_error.t
  end

  type t = private
    { tag : string
    ; value : Value.t
    }
  [@@deriving sexp]

  val make : string -> Value.t -> t Or_error.t
  val of_string : string -> t Or_error.t
  val to_string : t -> string
end

module Alignment : sig
  (** For [cigar] and [qual], empty list indicates no value, i.e. '*',
    was given. *)
  type t = private
    { qname : Qname.t option (** QNAME *)
    ; flag : Flag.t (** FLAG *)
    ; rname : Rname.t option (** RNAME *)
    ; pos : Pos.t option (** POS *)
    ; mapq : Mapq.t option (** MAPQ *)
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
    -> ?mapq:Mapq.t
    -> ?cigar:Cigar.t
    -> ?rnext:Rnext.t
    -> ?pnext:Pnext.t
    -> ?tlen:Tlen.t
    -> ?seq:Seq.t
    -> ?qual:Qual.t
    -> ?optional_fields:Optional_field.t list
    -> unit
    -> t Or_error.t

  (** [of_line line] parses the given [line] as a SAM alignment line.
      The [line] should not contain the final newline character. *)
  val of_line
    :  ?ref_seqs:(string, String.comparator_witness) Set.t
    -> string
    -> t Or_error.t

  (** [to_line x] converts the alignment [x] to a SAM alignment line.
      The returned string will not contain the final newline character. *)
  val to_line : t -> string
end

(** [must_be_header line] returns true if the first character of [line] is '@',
    quickly determining that it must be parsed as a header line. *)
val must_be_header : string -> bool

(** [of_lines lines] parses the given [lines] of a SAM file. *)
val of_lines : string list -> (Header.t * Alignment.t list) Or_error.t

val of_lines_exn : string list -> Header.t * Alignment.t list
