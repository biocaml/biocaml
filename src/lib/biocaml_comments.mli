(** Comment lines. Comment lines contain either just whitespace or
    have a user-specified start character. *)

open Batteries

type t
    (** Sequence of comment lines. *)
    
exception Invalid of string

val enum : t -> char * string Enum.t
  (** Return start character and enumeration of comment lines in given
      [t]. The start character is included in each string, but line
      terminating character(s) are not. *)

val empty : char -> t
  (** [empty c] is the empty list of comment lines, but with the
      specification that [c] will be used as the start character. *)

val concat : t -> t -> t
  (** [concat a b] concatenates comment lines [a] and [b] such that
      [a] is followed by [b].

      @raise Invalid if [a] and [b] do not have the same start
      character. *)
  
val of_string : ?comment_char:char -> string -> t
  (** [of_string c s] creates list of lines by splitting given string
      on newline characters. Each resulting line must begin with
      [comment_char] (default is '#') or consist of only
      whitespace.

      @raise Invalid if any line's first character is not
      [comment_char] or is not all whitespace. *)

val to_string : t -> string
  (** Return string representation of comment lines. All but last line
      will include line terminating character(s). *)
  
val comment_char : t -> char
  (** Return the start character for the given comments. *)

val is_comments : ?comment_char:char -> string -> bool
  (** Like [of_string] but simply returns true if given string can be
      parsed as valid comment lines. *)

val filter_comments_prefix : char -> string Enum.t -> t * string Enum.t
  (** [filter_comments_prefix c e] removes the first lines of e that are 
      comments and builds a [Comment.t] with it, which is returned along
      with the rest of the enum [e] *)
