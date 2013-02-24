(** Single line of text. See also {!module: Lines}. *)

(** A line is simply a string, possibly empty, that does not contain a
    newline character. As the name suggests, usually such strings are
    obtained by reading a file line by line. *)
type t = private string

(** [string_to_lines s] splits [s] on newline characters, returning
    the resuling list of lines. The returned bool is true if the final
    line ended with a newline, or false otherwise. *)
val string_to_lines : string -> (t list * bool)

(** Return the given string without checking that it is a line. Useful
    for efficiency reasons if you're certain the given string is a
    line. *)
val of_string_unsafe : string -> t

(** Coerce a line to a plain string. *)
val line_to_string : t -> string

(** {6 Standard String Operations} *)

val lstrip : ?drop:(char -> bool) -> t -> t
val rstrip : ?drop:(char -> bool) -> t -> t
val strip : ?drop:(char -> bool) -> t -> t
