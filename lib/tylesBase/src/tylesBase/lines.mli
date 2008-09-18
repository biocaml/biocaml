(** Parsing and printing of line-oriented text formats. Input sources and output destinations can be files, channels, streams, or strings. Documentation below provided only for files, and operations on other types are analogous. Only difference is that [Error]'s raised on files will have the file name but other types will not since the file name is of course not known. *)

exception Error of (Pos.t * string)
  (** Raised when there is a parse error, giving the position and an explanatory message. *)


(** {8 File Operations} *)

val fold_file : ?strict:bool -> ('a -> string -> 'a) -> 'a -> string -> 'a
  (** [fold_file ~strict f init file] accumulates the result of applying [f] to each line of [file]. Function [f] should raise [Failure] to indicate an error for the given line. If [strict] is true, the default, this will be caught and re-raised as [Error (p,m)], where [p] gives the position of the error. If [strict] is false, the exception is ignored and parsing continues. *)

val of_file : ?strict:bool -> (string -> 'a) -> string -> 'a list
  (** [of_file ~strict f file] reads all lines from [file], parsing each with [f]. See [fold_file] for additional details. *)

val to_file : ('a -> string) -> string -> 'a list -> unit
  (** [to_file f file l] prints each item of [l] on a separate line to [file], using [f] to convert each to a string. Every line, including the last one, is terminated by '\n'. *)

val copy_file : ?first:int -> ?last:int -> string -> string -> unit
  (** [copy_file ~first ~last in_file out_file] copies line numbers [first] through [last] (inclusive) of [in_file] to [out_file]. Omitting [first] starts copying from beginning of file, omitting [last] copies to end of file, and thus omitting both copies entire file.
      
      Any combination of values for [first] and [last] are okay. [first] assumed to be 1 if given a value less than 1. Copying done through last line if [last] is greater than number of lines in file. No lines are copied if [last < first].*)
  
  
(** {8 Channel Operations} 
    Like file operations above but input and output are channels. *)
  
val fold_channel : ?strict:bool -> ('a -> string -> 'a) -> 'a -> in_channel -> 'a 
val of_channel : ?strict:bool -> (string -> 'a) -> in_channel -> 'a list
val to_channel : ('a -> string) -> out_channel -> 'a list -> unit
val copy_channel : ?first:int -> ?last:int -> in_channel -> out_channel -> unit


(** {8 Stream Operations}
    Like file operations above but input and output are character streams. *)

val fold_stream : ?strict:bool -> ('a -> string -> 'a) -> 'a -> char Stream.t -> 'a 
val of_stream : ?strict:bool -> (string -> 'a) -> char Stream.t -> 'a list
val to_stream : ('a -> string) -> 'a list -> char Stream.t
  (** Like [to_file] but print output to alternative destinations. *)


(** {8 String Operations}
    Like file operations above but input and output are strings. *)

val fold_string : ?strict:bool -> ('a -> string -> 'a) -> 'a -> string -> 'a 
val of_string : ?strict:bool -> (string -> 'a) -> string -> 'a list
val to_string : ('a -> string) -> 'a list -> string
