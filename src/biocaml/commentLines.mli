(** Comment lines. Comment lines contain either just whitespace or have a user-specified start character. *)

type t
    (** Type of a sequence of comment lines. *)
    
exception Bad of string
  
val concat : t -> t -> t
  (** [concat t1 t2] concatenates the given comment lines. Raise [Bad] if start characters differ. *)
  
val of_string : char -> string -> t
  (** [of_string c s] creates list of lines by splitting given string on newline characters. Each resulting line must begin with [c] or consist of only whitespace. Raise [Bad] otherwise. *)
  
val to_string : t -> string
  (** Return string representation of comment lines. All but last line will be followed by newline. *)

val start_char : t -> char
  (** Return the start character for the given comments. *)
