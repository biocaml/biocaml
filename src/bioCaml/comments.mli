(** Comment lines starting with a # symbol. *)

type t
    (** Type of a sequence of comment lines. *)
    
exception Bad of string
  
val concat : t -> t -> t
  
val of_string : string -> t
  (** Creates list of lines by splitting given string on newline characters. Each resulting line must begin with '#' or consist of only whitespace. Raise [Bad] otherwise. *)
  
val to_string : t -> string
  (** Return string representation of comment lines. All but last line will be followed by newline. *)
