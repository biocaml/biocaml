(** Comment lines. Comment lines contain either just whitespace or
    have a user-specified start character. *)

open Batteries_uni

type t
    (** Sequence of comment lines. *)
    
exception Invalid of string

val enum : t -> string Enum.t
  (** Return enumeration of comment lines in given [t]. The start
      character is included in each string, but line terminating
      character(s) are not. *)

val concat : ?comment_char:char -> t list -> t
  (** [concat ts] flattens the comment lines in [ts].

      Default value for [comment_char] is '#' if [ts] is empty;
      otherwise it is determined from the given [ts]. In other words,
      you should omit [comment_char] if [ts] is non-empty. Raise
      [Invalid] if start character differs in any of the given
      comments or from [comment_char]. *)
  
val of_string : char -> string -> t
  (** [of_string c s] creates list of lines by splitting given string
      on newline characters. Each resulting line must begin with [c] or
      consist of only whitespace. Raise [Invalid] otherwise. *)
  
val to_string : t -> string
  (** Return string representation of comment lines. All but last line
      will include line terminating character(s). *)
  
val start_char : t -> char
  (** Return the start character for the given comments. *)
