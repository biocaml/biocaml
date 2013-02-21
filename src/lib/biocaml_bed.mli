(** BED data. A BED file is in the format shown below, where columns
    must be separted by a tab character.

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

type item = string * int * int * Biocaml_table.Row.t
with sexp
(** The type of BED data stream items. *)

type parsing_spec = [
  | `enforce of Biocaml_table.Row.t_type
  | `strings
]
with sexp
(** The specification of how to parse the remaining columns. *)

(** Definitions of error types ([with sexp]) *)
module Error: sig

  type parsing_base = [
    | `wrong_format of
        [ `column_number
        | `float_of_string of string
        | `int_of_string of string ] *
          Biocaml_table.Row.t_type * string
    | `wrong_number_of_columns of Biocaml_table.Row.t ]
  with sexp

  type parsing = [ `bed of parsing_base ]
  with sexp

end

val item_of_line: how:parsing_spec -> Biocaml_lines.item ->
  (item, [> Error.parsing]) Core.Result.t
(** Basic parsing of a single line. *)

val item_to_line: item -> Biocaml_lines.item
(** Basic “printing” of one single [item]. *)

module Transform: sig

  val string_to_item :
    ?more_columns:parsing_spec ->
    unit ->
    (string,
     (string * int * int * Biocaml_table.Row.item array,
      [> Error.parsing]) Core.Result.t) Biocaml_transform.t
  (** Create a [Biocaml_transform.t]-based parser, while providing the
     format of the additional columns (default [`strings]). *)

  val item_to_string: unit ->
    (item, string) Biocaml_transform.t
  (** Create a [Biocaml_transform.t] which “prints” BED data
     (reminder: includes ends-of-line). *)

end
