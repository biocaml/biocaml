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
| `variable_step_state_change of string * int option
| `variable_step_value of int * float
]
(** variable_step_state_change of name x span *)

type fixed_step = [
| `fixed_step_state_change of string * int * int * int option
| `fixed_step_value of float
]
(** fixed_step_state_change of name, start, step, span *)

type bed_graph_value = string * int * int * float

type item = [comment | variable_step | fixed_step | `bed_graph_value of bed_graph_value ]
(** The most general type that the default parser outputs.
    {[
    type t = [comment | variable_step | fixed_step | `bed_graph_value of bed_graph_value ]
    ]}
*)

(** {2 Error Types} *)

module Error: sig
  (** The errors of the [Wig] module. *)


  type parsing = [
    | `cannot_parse_key_values of Pos.t * string
    | `empty_line of Pos.t
    | `incomplete_input of Pos.t * string list * string option
    | `missing_chrom_value of Pos.t * string
    | `missing_start_value of Pos.t * string
    | `missing_step_value of Pos.t * string
    | `wrong_start_value of Pos.t * string
    | `wrong_step_value of Pos.t * string
    | `unrecognizable_line of Pos.t * string list
    | `wrong_bed_graph_value of Pos.t * string
    | `wrong_fixed_step_value of Pos.t * string
    | `wrong_span_value of Pos.t * string
    | `wrong_variable_step_value of Pos.t * string
  ]
  (** The parsing errors. *)

  val parsing_error_to_string: parsing -> string
  (** Convert a [parsing] error to a string. *)

  type to_bed_graph = [`not_in_variable_step_state | `not_in_fixed_step_state]
  (** The errors encountered while transforming [item] values to
  bed-graph-only values. *)

  type t = [ parsing | to_bed_graph ]
  (** The union of all errors. *)

  val parsing_of_sexp : Sexplib.Sexp.t -> parsing
  val sexp_of_parsing : parsing -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val to_bed_graph_of_sexp : Sexplib.Sexp.t -> to_bed_graph
  val sexp_of_to_bed_graph : to_bed_graph -> Sexplib.Sexp.t
end

(** {2  Tags} *)

module Tags: sig

  type t = {
    allow_empty_lines: bool;
    sharp_comments: bool;
  }
  (** Additional tags (c.f. {!Tags}). *)

  val default: t
  (** Default tags ([{allow_empty_lines = false; sharp_comments = true}]). *)

  val of_string: string ->
    (t, [> `wig of [> `tags_of_string of exn ] ]) result
  (** Parse tags (for now S-Expressions). *)

  val to_string: t -> string
  (** Serialize tags (for now S-Expressions). *)

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end

(** {2 [In_channel] Functions} *)

exception Error of  Error.t
(** The exceptions raised by the [Wig] module's [*_exn] functions. *)

val in_channel_to_item_stream: ?buffer_size:int -> ?filename:string ->
  ?tags:Tags.t -> in_channel -> (item, Error.t) result Stream.t
(** Get a stream of [item] values out of an input-channel. *)

val in_channel_to_item_stream_exn: ?buffer_size:int -> ?filename:string ->
  ?tags:Tags.t -> in_channel -> item Stream.t
(** Do like [in_channel_to_item_stream] but each call to [Stream.next]
    may throw an exception. *)

val in_channel_to_bed_graph:  ?buffer_size:int -> ?filename:string ->
  ?tags:Tags.t -> in_channel ->
  (bed_graph_value, Error.t) result Stream.t
(** Get a stream of [bed_graph_value] values out of a WIG-file input-channel. *)

val in_channel_to_bed_graph_exn: ?buffer_size:int -> ?filename:string ->
  ?tags:Tags.t -> in_channel -> bed_graph_value Stream.t
(** Do like [in_channel_to_bed_graph] but each call to [Stream.next]
    may throw an exception. *)


(** {2 [To_string] Functions} *)


val item_to_string: ?tags: Tags.t -> item -> string
(** Convert an [item] to a string (including new line characters).

    Note: the parsing of the [Tags.t] is staged, so storing [let
    to_string = item_to_string ~tags] only once could be slightly more
    efficient than calling [item_to_string ~tags item] many times.
*)

(** {2 Transform Creations} *)

module Transform: sig
  (** Low-level {!Tfxm.t}. *)

  val string_to_item :
    ?filename:string ->
    ?tags: Tags.t ->
    unit ->
    (string, (item, [> Error.parsing]) result) Tfxm.t
  (** Create the parsing [Tfxm.t]. The parser is
      "best-effort" and stateless (i.e. a line containing ["1000 42."]
      will parsed succesfully as a [`variable_step_value (1000, 42.)]
      even if no ["variableStep"] was line present before). *)

  val item_to_string: ?tags: Tags.t -> unit -> (item, string) Tfxm.t
  (** Create the transform that prints [item] values to strings. *)

  val item_to_bed_graph: unit ->
    (item,
     (bed_graph_value, [> Error.to_bed_graph]) result)
      Tfxm.t
  (** Create a transform which converts [`variable_step_value _] and
      [`fixed_step_value _] values to [`bed_graph_value _] values, using the
      current state. The [`bed_graph_value _] items stay untouched
      and [`comment _] values are ignored. *)
end

(** {2 S-Expressions} *)

val comment_of_sexp : Sexplib.Sexp.t -> comment
val sexp_of_comment : comment -> Sexplib.Sexp.t
val variable_step_of_sexp : Sexplib.Sexp.t -> variable_step
val sexp_of_variable_step : variable_step -> Sexplib.Sexp.t
val fixed_step_of_sexp : Sexplib.Sexp.t -> fixed_step
val sexp_of_fixed_step : fixed_step -> Sexplib.Sexp.t
val bed_graph_value_of_sexp : Sexplib.Sexp.t -> bed_graph_value
val sexp_of_bed_graph_value : bed_graph_value -> Sexplib.Sexp.t
val item_of_sexp : Sexplib.Sexp.t -> item
val sexp_of_item : item -> Sexplib.Sexp.t
