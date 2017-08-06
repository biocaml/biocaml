(** Generic “tables” (like CSV, TSV, Bed …). *)
open Core_kernel

(** {2 Table Rows/Lines } *)

module Row : sig
  (** Definition of rows *)

  type item = [`int of int | `float of float | `string of string ]
  (** Type row elements (or “cells”). *)

  type t = item array
  (** A single row. *)

  type item_type = [`type_int | `type_float | `type_string ]
  (** Definition of the type of a cell. *)

  type t_type = item_type array
  (** Definition of the type of a row. *)

  (** {3 Tags } *)

  module Tags: sig

    (** Tags describe the actual format of the row stream.
        See also {!Tags.t}.
    *)

    type t = [
      | `separator of char
      | `strict_about of [ `row_length | `cell_type ]
      | `format of t_type
    ] list
    (** The tags associated with table rows: {ul
        {li [`separator c] adds [c] in the list of separators used.}
        {li [`strict_about something] ensures that the format is fully
          compliant with [something] (example: otherwise the parser may
          ignore some errors to keep going). }
        {li [`tail_format array] describes the format of the
          columns of the row, if not provided everything is assumed to
          be [`type_string]. }
    } *)

    val separators: t -> char list
    (** Get the list of separators defined in [t]. *)

    val strict_row_length: t -> bool
    (** Tell whether one should be strict about the minimal number of
        cells per row (defined with [`format _]). *)

    val strict_cell_type: t -> bool
    (** Tell whether one should be strict about the types of the cells
        (defined with [`format _]). *)

    val format: t -> t_type option
    (** Get the defined format if any. *)

    val default: t
    (** The default tags define a loose TSV format. *)

    val default_extension: t -> string
    (** Give a file extension (["tsv"], ["csv"], or ["table"]). *)

    val to_string: t -> string
    (** Serialize tags. *)

    val of_string: string ->
      (t, [> `table_row of [> `tags_of_string of exn ] ]) Result.t
    (** Parse the description of the tags (for now S-Expressions). *)

    val t_of_sexp: Sexplib.Sexp.t -> t
    val sexp_of_t: t -> Sexplib.Sexp.t

  end

  module Error: sig

    type line_parsing =
      [ `wrong_format of
          [ `column_number
          | `float_of_string of string
          | `int_of_string of string ] * t_type * string ]

    type t = line_parsing

    val line_parsing_of_sexp: Sexplib.Sexp.t -> line_parsing
    val sexp_of_line_parsing: line_parsing -> Sexplib.Sexp.t
    val t_of_sexp: Sexplib.Sexp.t -> t
    val sexp_of_t: t -> Sexplib.Sexp.t
  end

  val of_line: ?separators:char list ->
    ?strict_row_length:bool -> ?strict_cell_type:bool -> ?format:t_type ->
    Line.t ->
    (t, [> Error.line_parsing ]) Result.t
  (** Parse a [Line.t] into a row while specifying a [format].
     - If [format] is [None] (the default), then all the elements are
       put in [`string _] rows.
     - The default cell separators are [[' '; '\t']].
     - If [strict_row_length] is [true] and [format] provided,
       then check that the number of columns is {b at least} the one
       of the [format].
     - If [strict_cell_type] is [true] and [format] provided,
       then check that each cell has the exactly right format.
  *)

  val to_line: sep:string -> t -> Line.t
  (** Write the row to a [Line.t]. *)

  (** {3 [Transform.t] Creations} *)

  module Transform: sig

    val line_to_item : ?tags:Tags.t -> unit ->
      (Lines.item,
       (t, [> `table_row of Error.line_parsing ]) Result.t)
        Tfxm.t
   (** Create a {!Tfxm.t} that converts lines to
       table-rows according to the [tags] (default: {!Tags.default}). *)

    val item_to_line:  ?tags:Tags.t -> unit ->
      (t, Lines.item) Tfxm.t
   (** Create a {!Tfxm.t} that converts rows to lines
       using the first separator in the tags or ['\t'] if none
       (default [tags]: {!Tags.default}). *)

  end

  (** {3 S-Expressions } *)

  val item_of_sexp : Sexplib.Sexp.t -> item
  val sexp_of_item : item -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val item_type_of_sexp : Sexplib.Sexp.t -> item_type
  val sexp_of_item_type : item_type -> Sexplib.Sexp.t
  val t_type_of_sexp : Sexplib.Sexp.t -> t_type
  val sexp_of_t_type : t_type -> Sexplib.Sexp.t


end

