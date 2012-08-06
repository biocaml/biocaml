(** Generic stream-transformations for parsing and pretty-printing. *)



type ('input, 'output, 'error) t 
(** Basic type a of a Cryptokit-styled transformation:
    an [('input, 'output, 'error) t] is a buffered transformation
    that can be fed with ['input]
    values, stopped, and "pulled form" providing
    ['output] values or ['error] values.
    A {i "stoppable"} transformation is a transformation that fulfills
    the following requirements: {ul
     {li after [stop] has been called, the [next] should return
     [`end_of_stream] or [`error _] at some point.}
     {li calling [feed t] after having called [stop t] is erroneous and
     should throw [Feeding_stopped_transformation name].}
    }

*)

type no_error
(** An empty type to express transforms that cannot fail. *)

val make:
  ?name:string -> 
  next: (unit -> [ `output of 'output | `end_of_stream
                | `error of 'error | `not_ready ]) ->
  feed: ('input -> unit) ->
  stop: (unit -> unit) ->
  unit ->
  ('input, 'output, 'error) t
(** Build a basic transformation. *)

exception Feeding_stopped_transformation of string
(** Exception thrown by "stoppable" transformations.  *) 
    
val feed: 
  ('input, 'output, 'error) t -> 'input -> unit
val next: 
  ('input, 'output, 'error) t ->
  [ `output of 'output | `end_of_stream | `error of 'error | `not_ready ]
val stop: 
  ('input, 'output, 'error) t -> unit
val name:
  ('input, 'output, 'error) t -> string option

val make_stoppable: ?name:string -> 
  feed: ('input -> unit) ->
  next: (bool -> [ `output of 'output | `end_of_stream
                 | `error of 'error | `not_ready ]) ->
  unit ->
  ('input, 'output, 'error) t
(** Make a "stoppable" transformation easily, [make_stoppable] takes care of
    raising [Feeding_stopped_transformation] in case of wrong use, and calls the
    [~next] argument with a boolean value indicating if the transformation
    has been stopped. *)

val identity: ?name:string -> unit -> ('a, 'a, no_error) t
(** Create a stoppable, buffering transform that does nothing else. *)

val on_input: 
  ('input_a, 'output, 'error) t ->
  f:('input_b -> 'input_a) ->
  ('input_b, 'output, 'error) t
(** Map the input of a t (pre-processor). *)

val on_output: 
  ('input, 'output_a, 'error) t ->
  f:('output_a -> 'output_b) ->
  ('input, 'output_b, 'error) t
(** Map the output of a t (post-processor). *)

val on_error: 
  ('input, 'output, 'error_a) t ->
  f:('error_a -> 'error_b) ->
  ('input, 'output, 'error_b) t
(** Map on the errors of a transform (post-processor). *)

val compose:
  ( 'input_left, 'middle, 'error_left) t ->
  ( 'middle, 'output_right, 'error_right) t ->
  ( 'input_left, 'output_right, [ `left of 'error_left | `right of 'error_right ] )
    t
(** Compose (or {i Sequence}) two transforms. *)
    
val mix :
  ( 'input_left, 'output_left, 'error_left) t ->
  ( 'input_right, 'output_right, 'error_right) t ->
  f:('output_left -> 'output_right -> 'output_f) ->
  ( 'input_left * 'input_right, 'output_f,
    [ `left of 'error_left | `right of 'error_right
    | `end_of_left_stream | `end_of_right_stream ] ) t
(** Create a transformation that merges the output of two transformations.  *) 

val partially_compose:
  ('il, 'ol, 'el) t -> ('ir, 'our, 'er) t ->
  destruct:('ol -> [`Yes of 'ir | `No of 'filtered]) ->
  reconstruct:([`Filtered of 'filtered | `Done of 'our] -> 'result) ->
  ('il, 'result, [`left of 'el | `right of 'er]) t
(** Partially compose two transformations by providing a filtering
    function ([~destruct]) and a joining function ([~reconstruct]). *)

val split_and_merge:
  ('il, 'ol, 'el) t -> ('ir, 'our, 'er) t ->
  split:('input -> [`left of 'il | `right of 'ir]) ->
  merge:([`left of 'ol | `right of 'our] -> 'output) ->
  ('input, 'output, [`left of 'el | `right of 'er]) t
(** Split the flow between two transformations thanks to a splitting
    and a merging functions on their different inputs/outputs. The
    resulting transformation may not respect the order of the inputs (it
    depends on the buffering done by the individual input transforms). *)
    
val stream_transformation:
  error_to_exn:('error -> exn) ->
  ('input, 'output, 'error) t ->
  'input Stream.t -> 'output Stream.t
(** Make a transformation between standard OCaml streams that may
    raise exceptions. *)

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
  (** Get the number of lines ready-to-use in the buffer/queue. *)

  val is_empty: parser -> bool
  (** Tell if the parser's buffers are empty or not. For instance, when there is no
      more content to feed and [next_line] returns [None], [is_empty p =
      true] means that the content did not end with a complete line. *)
    
  val next_line: parser -> string option
  (** Get the next line. *)

  exception No_next_line
  (** The exception thrown by [next_line_exn]. *)

  val next_line_exn: parser -> string
  (** Get the next line, but throw [No_next_line] if there is no line to return. *)
    
  val current_position: parser -> Biocaml_pos.t
  (** Get the current position in the stream. *)

  val finish : parser -> [`ok | `error of string list * string option ]
(** Terminate the parsing, if the buffers are not empty return them as an error. *)
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

  val is_empty: 'a t -> bool
(** Check if the printer-queue is empty. *) 

end

(** {3 Non-cooperative Streams } *)

(** Pull-based streams built out of transforms (inherently
    non-cooperative). *)
module Pull_based: sig

  type 'a stream
  (** A stream container. *)

  val next: 'a stream -> 'a 
  (** Call the basic operation of a stream. *)

  val of_feeder:
    (unit -> 'input option) ->
    ('input, 'a, 'b) t ->
    [ `end_of_stream | `error of 'b | `output of 'a ] stream
  (** Create a stream from a feeding function. The transform is
      fed with the function's output ([None] means end-of-stream). *)

  val of_in_channel:
    ?buffer_size:int ->
    in_channel ->
    (string, 'a, 'b) t ->
    [ `end_of_stream | `error of 'b | `output of 'a ] stream
  (** Create a stream from an [in_channel]. The transformation is fed
      with strings of size [buffer_size] ({i or less}). *)
    
  val of_file :
    ?buffer_size:int ->
    string ->
    (string, 'a, 'b) t ->
    [ `end_of_stream | `error of 'b | `output of 'a ] stream
  (** Like [of_in_channel] but internally open the file and close it on
      [`end_of_stream] ({b Warning:} the channel is not closed on [`error _]. *)

  val to_stream_exn:
    error_to_exn:('error -> exn) ->
    [ `end_of_stream | `error of 'error | `output of 'output ] stream ->
    'output Stream.t
  (** Convert a stream to an exception-full OCaml [Stream.t]. *) 

  val to_stream_result:
    [ `end_of_stream | `error of 'error | `output of 'output ] stream ->
    ('output, 'error) Core.Std.Result.t Stream.t
  (** Convert a stream to an OCaml [Stream.t] of [Result.t] values. *) 
      
end
