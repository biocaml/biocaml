(** Generic “tables” (like CSV, TSV, Bed …). *)


(** Definition of rows *)
module Row : sig

  type item = [`int of int | `float of float | `string of string ] with sexp
  (** Type row elements (or “cells”). *)

  type t = item array with sexp
  (** A single row. *)

  type item_type = [`type_int | `type_float | `type_string ] with sexp
  (** Definition of the type of a cell. *)

  type t_type = item_type array with sexp
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

end

