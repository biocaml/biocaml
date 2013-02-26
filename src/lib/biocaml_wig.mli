(** WIG data.

    Internal representation of coordinates always assumes the first
    position on a chromosome is numbered 1. Also, integer ranges are
    always closed; the range [\[1, 10\]] is the set of integers from 1
    to 10 inclusive of 1 and 10. WIG data can be in three
    formats---bed, variable-step, or fixed-step---and unfortunately
    each has different conventions as follows:
    - Bed format requires half-open intervals [\[low, high\)] and
      numbers the first base as 0. Thus 1 is added to the low value
      when parsing. The line ["chrI 0 10 3.14"] is parsed to [("chrI",
      1, 10, 3.14)].
    - Variable-step format numbers the first position 1 and uses
      closed intervals. Thus no change is required. The line ["1
      3.14"] is parsed to [(1, 3.14)].
    - Fixed-step format numbers the first position 1 and uses closed
      intervals. Thus no change is required. The header line
      ["fixedStep chrom=chrI start=1 step=100 span=30"] is parsed to
      [("chrI", 1, 100, 30)].

    The inverse is done for printing routines. You are freed from
    these details if you always use this module to parse and print.

    All parsers allow columns (fields) on a line to be separated by
    any combination of space, tab, or carriage return
    characters. Printers always separate columns with a single
    tab. Tag-value pairs must be in the form "tag=value" with no space
    around the '='.
*)

(** {2 Basic Types} *)


type comment = [
| `comment of string
]

type variable_step = [
| `variable_step_state_change of string * int option (** name x span *)
| `variable_step_value of int * float
]

type fixed_step = [
| `fixed_step_state_change of string * int * int * int option
(** name, start, step, span *)
| `fixed_step_value of float
]

type bed_graph_value = string * int * int * float

(** {2 Parsing and Printing} *)

type t = [comment | variable_step | fixed_step | `bed_graph_value of bed_graph_value ]
(** The most general type that the default parser outputs.
    {[
    type t = [comment | variable_step | fixed_step | `bed_graph_value of bed_graph_value ]
    ]}
*)

type parse_error = [
| `cannot_parse_key_values of Biocaml_pos.t * string
| `empty_line of Biocaml_pos.t
| `incomplete_input of Biocaml_pos.t * string list * string option
| `missing_chrom_value of Biocaml_pos.t * string
| `missing_start_value of Biocaml_pos.t * string
| `missing_step_value of Biocaml_pos.t * string
| `wrong_start_value of Biocaml_pos.t * string
| `wrong_step_value of Biocaml_pos.t * string
| `unrecognizable_line of Biocaml_pos.t * string list
| `wrong_bed_graph_value of Biocaml_pos.t * string
| `wrong_fixed_step_value of Biocaml_pos.t * string
| `wrong_span_value of Biocaml_pos.t * string
| `wrong_variable_step_value of Biocaml_pos.t * string
]
(** The parsing errors. *)

val parse_error_to_string: parse_error -> string
(** Convert a [parse_error] to a string. *)

type tag = [ `sharp_comments | `pedantic ]
(** Additional tags (c.f. {!Biocaml_tags}). *)

val default_tags: tag list
(** Default tags ([[ `sharp_comments; `pedantic ]]). *)

module Transform: sig
  (** Low-level {!Biocaml_transform.t}. *)

  val string_to_t :
    ?filename:string ->
    ?tags: tag list ->
    unit ->
    (string, (t, [> parse_error]) Core.Result.t) Biocaml_transform.t
  (** Create the parsing [Biocaml_transform.t]. The parser is
      "best-effort" and stateless (i.e. a line containing ["1000 42."]
      will parsed succesfully as a [`variable_step_value (1000, 42.)]
      even if no ["variableStep"] was line present before). *)

  val t_to_string: ?tags: tag list -> unit -> (t, string) Biocaml_transform.t
  (** Create the transform that prints [t] values to strings. *)

  val t_to_bed_graph: unit ->
    (t,
     (bed_graph_value,
      [> `not_in_variable_step_state | `not_in_fixed_step_state]) Core.Result.t)
      Biocaml_transform.t
  (** Create a transform which converts [`variable_step_value _] and
      [`fixed_step_value _] values to [`bed_graph_value _] values, using the
      current state. The [`bed_graph_value _] and [`comment _] values stay
      untouched. *)
end

(** {2 S-Expressions} *)

val comment_of_sexp : Sexplib.Sexp.t -> comment
val comment_of_sexp__ : Sexplib.Sexp.t -> comment
val sexp_of_comment : comment -> Sexplib.Sexp.t
val variable_step_of_sexp : Sexplib.Sexp.t -> variable_step
val variable_step_of_sexp__ : Sexplib.Sexp.t -> variable_step
val sexp_of_variable_step : variable_step -> Sexplib.Sexp.t
val fixed_step_of_sexp : Sexplib.Sexp.t -> fixed_step
val fixed_step_of_sexp__ : Sexplib.Sexp.t -> fixed_step
val sexp_of_fixed_step : fixed_step -> Sexplib.Sexp.t
val bed_graph_value_of_sexp : Sexplib.Sexp.t -> bed_graph_value
val sexp_of_bed_graph_value : bed_graph_value -> Sexplib.Sexp.t
val t_of_sexp : Sexplib.Sexp.t -> t
val t_of_sexp__ : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t val parse_error_of_sexp : Sexplib.Sexp.t -> parse_error
val parse_error_of_sexp__ : Sexplib.Sexp.t -> parse_error
val sexp_of_parse_error : parse_error -> Sexplib.Sexp.t
val tag_of_sexp : Sexplib.Sexp.t -> tag
val tag_of_sexp__ : Sexplib.Sexp.t -> tag
val sexp_of_tag : tag -> Sexplib.Sexp.t

