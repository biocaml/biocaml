(** Generic stream-transformations for parsing and pretty-printing. *)

(** A buffering parser for line-oriented formats. *)
module Line_oriented: sig
    
  type parser
  
  val parser: ?filename:string -> unit -> parser
  (** Create a "parser"; the optional [filename] is used only to
      create error locations. *)
    
  val feed_line: parser -> string -> unit
  (** Feed the parser with a line. *)

  val feed_string: parser -> string -> unit
  (** Feed the parser with an arbitrary string buffer. *)

    
  val queued_lines: parser -> int
  (** Get the number of lines in the buffer/queue. *)
    
  val next_line: parser -> string option
  (** Get the next line. *)

  exception No_next_line
  (** The exception thrown by [next_line_exn]. *)

  val next_line_exn: parser -> string
  (** Get the next line, but throw [No_next_line] if there is no line to return. *)
    
  val current_position: parser -> Biocaml_pos.t
  (** Get the current position in the stream. *)

end

(** A generic buffering printer.  *)
module Printer_queue: sig

  type 'a t

  val make: ?buffer:[`clear of int | `reset of int] ->
    to_string:('a -> string) -> unit -> 'a t
  (** Create a printer-queue with a [to_string] function. The [buffer]
      argument tells whether to use [Buffer.clear] or [Buffer.reset] after
      flushing the buffer. *)

  val feed: 'a t -> 'a -> unit
  (** Enqueue something in the printer. *)

  val flush: 'a t -> string
  (** Get the current transformed content. *)
    
end

(** {3 Classy Transformers} *)
  
(** Basic type a of a Cryptokit-styled transformation:
    An [\['input, 'output, 'error\] transform] is an object that can
    be fed with ['input] values and pulled form giving ['output] values
    or ['error] values.
*)
class type ['input, 'output, 'error] transform =
object
  method feed: 'input -> unit
  method next: [ `output of 'output | `not_ready | `error of 'error ]
end

val compose:
  ( 'input_left, 'middle, 'error_left) transform ->
  ( 'middle, 'output_right, 'error_right) transform ->
  ( 'input_left, 'output_right, [ `left of 'error_left | `right of 'error_right ] )
    transform
(** Compose (or {i Sequence}) two transformations. *)
    
val mix :
  ( 'input_left, 'output_left, 'error_left) transform ->
  ( 'input_right, 'output_right, 'error_right) transform ->
  f:('output_left -> 'output_right -> 'output_f) ->
  ( 'input_left * 'input_right, 'output_f,
    [ `left of 'error_left | `right of 'error_right ] ) transform
(** Create a transformation that merges the output of two transformations.  *) 


val enum_transformation :
  error_to_exn:('error -> exn) ->
  ('input, 'output, 'error) transform ->
  'input BatEnum.t -> 'output BatEnum.t
(** Make an enum-transformation that may raise exceptions. *)
    
