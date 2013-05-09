(** Generic “tables” (like CSV, TSV, Bed …). *)

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
        See also {!Biocaml_tags.t}.
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
    (** Tell whether one should be strict about the length of the rows
        (defined with [`format _]). *)

    val strict_cell_type: t -> bool
    (** Tell whether one should be strict about the types of the cells
        (defined with [`format _]). *)

    val format: t -> t_type option
    (** Get the defined format if any. *)

    val to_string: t -> string
    (** Serialize tags. *)

    val of_string: string ->
      (t, [> `table_row of [> `tags_of_string of exn ] ]) Core.Result.t
    (** Parse the description of the tags (for now S-Expressions). *)

    val t_of_sexp: Sexplib.Sexp.t -> t
    val sexp_of_t: t -> Sexplib.Sexp.t

  end

  val of_line: ?separators:char list -> ?strict:bool -> ?format:t_type ->
    Biocaml_line.t ->
    (t, [> `wrong_format of
             [> `column_number
             | `float_of_string of string
             | `int_of_string of string ] * t_type * string ]) Core.Result.t
  (** Parse a [Line.t] into a row while specifying a [format].
     - If [format] is [None] (the default), then all the elements are
       put in [`string _] rows.
     - The default cell separators are [[' '; '\t']].
     - If [strict] is [true] and [format] provided, the {b only} the
       format will be accepted. If [strict] is [false], rows matching
       only a prefix of the [format] will be accepted, and rows longer
       that the [format] will be also accepted (using [`string _] for
       the cells).
  *)

  val to_line: sep:string -> t -> Biocaml_line.t
  (** Write the row to a [Line.t]. *)


  val item_of_sexp : Sexplib.Sexp.t -> item
  val sexp_of_item : item -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val item_type_of_sexp : Sexplib.Sexp.t -> item_type
  val sexp_of_item_type : item_type -> Sexplib.Sexp.t
  val t_type_of_sexp : Sexplib.Sexp.t -> t_type
  val sexp_of_t_type : t_type -> Sexplib.Sexp.t


end

