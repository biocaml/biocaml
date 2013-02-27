(** Generic “tables” (like CSV, TSV, Bed …). *)


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
  val item_of_sexp__ : Sexplib.Sexp.t -> item
  val sexp_of_item : item -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val item_type_of_sexp : Sexplib.Sexp.t -> item_type
  val item_type_of_sexp__ : Sexplib.Sexp.t -> item_type
  val sexp_of_item_type : item_type -> Sexplib.Sexp.t
  val t_type_of_sexp : Sexplib.Sexp.t -> t_type
  val sexp_of_t_type : t_type -> Sexplib.Sexp.t


end

