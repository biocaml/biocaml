(** Single line of text. See also {!module: Lines}. *)

(** A line is simply a string, possibly empty, that does not contain a
    newline character. As the name suggests, usually such strings are
    obtained by reading a file line by line. *)
type t = private string

val to_string : t -> string

val empty : t

(** [string_to_lines s] splits [s] on newline characters, returning
    the resuling list of lines. If the final line ended with a
    newline, the last string of the list is empty. *)
val string_to_lines : string -> t list

(** [rightmost s] returns a pair whose right member is the longest
    suffix [v] of [s] that represents a line, while the possible
    remainder of [s] (minus the newline character) is represented in
    the left member. More formally, if [v = s] then the left member of
    the pair is [None]; otherwise it is [Some u] where [s = u ^ "\n" ^
    v]. *)
val rightmost : string -> string option * t

(** {2 Standard String Operations} *)

val append : t -> t -> t
val split : t -> on:char -> string list

(** {2 S-Expressions } *)

val t_of_sexp: Sexplib.Sexp.t -> t
val sexp_of_t: t -> Sexplib.Sexp.t
