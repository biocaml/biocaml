(** Single line of text. See also {!module: Lines}. *)

(** A line is simply a string, possibly empty, that does not contain a
    newline character. As the name suggests, usually such strings are
    obtained by reading a file line by line. *)
type t = private string

val empty : t

(** [parse_string s] splits [s] on newline characters, returning
    the resulting list of lines. If the final line ended with a
    newline, the last string of the list is empty. *)
val parse_string : string -> t list

(** [rightmost s] returns a pair whose right member is the longest
    suffix [v] of [s] that represents a line, while the possible
    remainder of [s] (minus the newline character) is represented in
    the left member. More formally, if [v = s] then the left member of
    the pair is [None]; otherwise it is [Some u] where [s = u ^ "\n" ^
    v]. *)
val rightmost : string -> string option * t

val to_string : t -> string

(** Return the given string without checking that it is a line. Useful
    for efficiency reasons if you're certain the given string is a
    line. *)
val of_string_unsafe : string -> t

(** {2 Standard String Operations} *)

val is_empty : t -> bool
val lstrip : ?drop:(char -> bool) -> t -> t
val rstrip : ?drop:(char -> bool) -> t -> t
val strip : ?drop:(char -> bool) -> t -> t
val split : t -> on:char -> string list
val for_all : string -> f:(char -> bool) -> bool
val append : t -> t -> t

(** @raise [Invalid_arg _] if [sep = '\n'] *)
val concat : ?sep:char -> t list -> t

(** {2 S-Expressions } *)

val t_of_sexp: Sexp.t -> t
val sexp_of_t: t -> Sexp.t
