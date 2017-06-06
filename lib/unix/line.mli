(** Single line of text. See also {!module: Lines}. *)
open Core_kernel

(** A line is simply a string, possibly empty, that does not contain a
    newline character. As the name suggests, usually such strings are
    obtained by reading a file line by line. *)
type t = private string

val to_string : t -> string

(** [string_to_lines s] splits [s] on newline characters, returning
    the resuling list of lines. The returned bool is true if the final
    line ended with a newline, or false otherwise. *)
val string_to_lines : string -> (t list * bool)

(** Return the given string without checking that it is a line. Useful
    for efficiency reasons if you're certain the given string is a
    line. *)
val of_string_unsafe : string -> t

(** {2 Standard String Operations} *)

val lstrip : ?drop:(char -> bool) -> t -> t
val rstrip : ?drop:(char -> bool) -> t -> t
val strip : ?drop:(char -> bool) -> t -> t
val split : t -> on:char -> string list
val for_all : string -> f:(char -> bool) -> bool

(** {2 S-Expressions } *)

val t_of_sexp: Sexplib.Sexp.t -> t
val sexp_of_t: t -> Sexplib.Sexp.t
