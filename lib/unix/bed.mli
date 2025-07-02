(** BED data files.

    A BED file is in the format shown below, where columns
    must be separated by a tab character.

    {v
    chrA   lo1   hi1
    chrA   lo2   hi2
    .      .     .
    .      .     .
    .      .     .
    chrB   lo1   hi1
    chrB   lo2   hi2
    .      .     .
    .      .     .
    .      .     .
    v}

    The definition is that intervals are zero based and half-open. So by
    default the line "chrA lo hi" is parsed to the interval [\[lo + 1,
    hi\]] on chromosome [chrA]. Similarly, when printing, the default
    is to print [\[lo - 1, hi\]]. The optional argument
    [increment_lo_hi] allows changing this behavior for non-conformant
    files. In addition, the optional argument [chr_map] is a [string
    -> string] function that allows changing of the chromosome name to
    a specified format, and defaults to [identity].

    Some tools require that the set of intervals do not overlap within
    each chromosome. This is not enforced, but you can use
    [any_overlap] to verify this property when needed.
*)

(** {2 Item Types} *)

(** The type of BED data stream items. *)
type item = string * int * int * Table.Row.t

(** {2 Tags: Describe The Format: TODO } *)

(** The specification of how to parse the remaining columns. *)
type parsing_spec =
  [ `enforce of Table.Row.t_type
  | `strings
  ]

(** {2 Error Types} *)

module Error : sig
  (** Definitions of error types ([with sexp]) *)

  (** The parsing errors. *)
  type parsing_base =
    [ `wrong_format of
      [ `column_number | `float_of_string of string | `int_of_string of string ]
      * Table.Row.t_type
      * string
    | `wrong_number_of_columns of Table.Row.t
    ]

  (** The parsing errors. *)
  type parsing = [ `bed of parsing_base ]

  (** The union of all the errors. *)
  type t = parsing

  val parsing_base_of_sexp : Sexplib.Sexp.t -> parsing_base
  val sexp_of_parsing_base : parsing_base -> Sexplib.Sexp.t
  val parsing_of_sexp : Sexplib.Sexp.t -> parsing
  val sexp_of_parsing : parsing -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end

(** {2 [In_channel] Functions } *)

(** The exception raised by the [*_exn] functions. *)
exception Error of Error.t

(** Parse an input-channel into [item] values. *)
val in_channel_to_item_stream
  :  ?buffer_size:int
  -> ?more_columns:parsing_spec
  -> In_channel.t
  -> (item, [> Error.parsing ]) result CFStream.Stream.t

(** Like [in_channel_to_item_stream] but use exceptions for errors
    (raised within [Stream.next]). *)
val in_channel_to_item_stream_exn
  :  ?buffer_size:int
  -> ?more_columns:parsing_spec
  -> In_channel.t
  -> item CFStream.Stream.t

(** {2 Conversions to/from [Biocaml.Line.t] }

    See also {!Biocaml.Line.t}.
*)

(** Basic parsing of a single line. *)
val item_of_line : how:parsing_spec -> Lines.item -> (item, [> Error.parsing ]) result

(** Basic “printing” of one single [item]. *)
val item_to_line : item -> Lines.item

(** {2 Transforms } *)

module Transform : sig
  (** Lower-level transforms of BED data-streams. *)

  (** Create a [Tfxm.t]-based parser, while providing the
     format of the additional columns (default [`strings]). *)
  val string_to_item
    :  ?more_columns:parsing_spec
    -> unit
    -> ( string
       , (string * int * int * Table.Row.item array, [> Error.parsing ]) result )
       Tfxm.t

  (** Create a [Tfxm.t] which “prints” BED data
     (reminder: includes ends-of-line). *)
  val item_to_string : unit -> (item, string) Tfxm.t
end

(** {2 S-Expressions} *)

val item_of_sexp : Sexplib.Sexp.t -> item
val sexp_of_item : item -> Sexplib.Sexp.t
val parsing_spec_of_sexp : Sexplib.Sexp.t -> parsing_spec
val sexp_of_parsing_spec : parsing_spec -> Sexplib.Sexp.t
