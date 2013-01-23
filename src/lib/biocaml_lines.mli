(** Lines of a file. The end of a line is indicated by a single
    newline character. *)


(** A single line, possibly empty, without the ending newline
    character. *)
type item = private string

(** [string_to_items s] splits [s] on newline characters, returning
    the resuling list of lines. The returned bool is true if the final
    line ended with a newline or false otherwise. *)
val string_to_items : string -> (item list * bool)
