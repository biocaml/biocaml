(** Generic stream-transformations for parsing, pretty-printing, and
more … *)

type ('input, 'output) t
(** Basic type a of a Cryptokit-styled transformation:
    an [('input, 'output) t] is a buffered transformation
    that can be fed with ['input]
    values, stopped, and "pulled form" providing
    ['output] values.
    A {i "stoppable"} transformation is a transformation that fulfills
    the following requirements: {ul
     {li after [stop] has been called, the [next] should return
     [`end_of_stream] at some point.}
     {li calling [feed t] after having called [stop t] is erroneous and
     should throw [Feeding_stopped_transformation name].}
    }

*)

val make:
  ?name:string ->
  next: (unit -> [ `output of 'output | `end_of_stream | `not_ready ]) ->
  feed: ('input -> unit) ->
  stop: (unit -> unit) ->
  unit ->
  ('input, 'output) t
(** Build a basic transformation. *)

exception Feeding_stopped_transformation of string
(** Exception thrown by "stoppable" transformations.  *)

val feed: ('input, 'output) t -> 'input -> unit

val next:
  ('input, 'output) t -> [ `output of 'output | `end_of_stream | `not_ready ]

val stop: ('input, 'output) t -> unit

val name: ('input, 'output) t -> string option

val make_stoppable: ?name:string ->
  feed: ('input -> unit) ->
  next: (bool -> [ `output of 'output | `end_of_stream | `not_ready ]) ->
  unit ->
  ('input, 'output) t
(** Make a "stoppable" transformation easily, [make_stoppable] takes care of
    raising [Feeding_stopped_transformation] in case of wrong use, and calls the
    [~next] argument with a boolean value indicating if the transformation
    has been stopped. *)

val make_stoppable_with_error: ?name:string ->
  feed: ('input -> unit) ->
  next: (bool -> [ `output of ('a, 'b) Core.Result.t
                 | `end_of_stream | `not_ready ]) ->
  unit ->
  ('input, ('a, 'b) Core.Result.t) t
(** Make a stoppable transformation like [make_stoppable] but also
    stop the stream after the first error is detected. *)

val identity: ?name:string -> unit -> ('a, 'a) t
(** Create a stoppable, buffering transform that does nothing else. *)

val on_input:
  ('input_a, 'output) t ->
  f:('input_b -> 'input_a) ->
  ('input_b, 'output) t
(** Map the input of a t (pre-processor). *)

val on_output:
  ('input, 'output_a) t ->
  f:('output_a -> 'output_b) ->
  ('input, 'output_b) t
(** Map the output of a t (post-processor). *)

val compose:
  ( 'input_left, 'middle) t ->
  ( 'middle, 'output_right) t ->
  ( 'input_left, 'output_right) t
(** Compose (or {i Sequence}) two transforms. *)

val bind_result:
  on_error:([`left of 'error_left | `right of 'error_right ] -> 'error) ->
  ( 'input_left, ('middle, 'error_left) Core.Result.t) t ->
  ( 'middle, ('output_right, 'error_right) Core.Result.t) t ->
  ( 'input_left, ('output_right, 'error) Core.Result.t) t
(** Compose (or {i Sequence}) two transforms that return [Result.t] values. *)

val bind_result_merge_error:
  ('a, ('b, 'el) Core.Result.t) t ->
  ('b, ('d, 'er) Core.Result.t) t ->
  ('a, ('d, [ `left of 'el | `right of 'er ]) Core.Result.t) t
(** Compose like [bind_result] but consider the error types compatible
    (or “merge-able”). *)

val map_result:
  ( 'input_left, ('middle, 'error) Core.Result.t) t ->
  ( 'middle, 'output_right) t ->
  ( 'input_left, ('output_right, 'error) Core.Result.t) t

val mix :
  ( 'input_left, 'output_left) t ->
  ( 'input_right, 'output_right) t ->
  f:('output_left -> 'output_right -> 'output_f) ->
  ( 'input_left * 'input_right, 'output_f) t
(** Create a transformation that merges the output of two transformations.  *)

val partially_compose:
  ('il, 'ol) t -> ('ir, 'our) t ->
  destruct:('ol -> [`Yes of 'ir | `No of 'filtered]) ->
  reconstruct:([`Filtered of 'filtered | `Done of 'our] -> 'result) ->
  ('il, 'result) t
(** Partially compose two transformations by providing a filtering
    function ([~destruct]) and a joining function ([~reconstruct]). *)

val split_and_merge:
  ('il, 'ol) t -> ('ir, 'our) t ->
  split:('input -> [`left of 'il | `right of 'ir]) ->
  merge:([`left of 'ol | `right of 'our] -> 'output) ->
  ('input, 'output) t
(** Split the flow between two transformations thanks to a splitting
    and a merging functions on their different inputs/outputs. The
    resulting transformation may not respect the order of the inputs (it
    depends on the buffering done by the individual input transforms). *)

val stream_transformation:
  ('input, 'output) t -> 'input Stream.t -> 'output Stream.t
(** Make a transformation between standard OCaml streams that may
    raise exceptions. *)

(** A buffering parsing_buffer for line-oriented formats. *)
module Line_oriented: sig

  type parsing_buffer

  val parsing_buffer: ?filename:string -> unit -> parsing_buffer
  (** Create a "parser"; the optional [filename] is used only to
      create error locations. *)

  val feed_line: parsing_buffer -> string -> unit
  (** Feed the parser with a line. *)

  val feed_string: parsing_buffer -> string -> unit
  (** Feed the parser with an arbitrary string buffer. *)

  val queued_lines: parsing_buffer -> int
  (** Get the number of lines ready-to-use in the buffer/queue. *)

  val is_empty: parsing_buffer -> bool
  (** Tell if the parser's buffers are empty or not. For instance, when there is no
      more content to feed and [next_line] returns [None], [is_empty p =
      true] means that the content did not end with a complete line. *)

  val next_line: parsing_buffer -> string option
  (** Get the next line. *)

  exception No_next_line
  (** The exception thrown by [next_line_exn]. *)

  val next_line_exn: parsing_buffer -> string
  (** Get the next line, but throw [No_next_line] if there is no line to return. *)

  val current_position: parsing_buffer -> Biocaml_pos.t
  (** Get the current position in the stream. *)

  val contents : parsing_buffer -> string list * string option
    (** Return any remaining lines and the unfinished string, without
        removing them from the buffer. *)

  val empty : parsing_buffer -> unit
    (** Empty the buffer. Subsequent call to [contents] will return
        [(\[\], None)]. *)

  val make_stoppable : ?name:string -> ?filename:string ->
    next:(parsing_buffer ->
          [ `not_ready | `output of ('b, 'errnext) Core.Result.t ]) ->
    on_error:(
      [`next of 'errnext
      | `incomplete_input of Biocaml_pos.t * string list * string option] ->
      'err) ->
    unit ->
    (string, ('b, 'err) Core.Result.t) t
  (** Build a stoppable line-oriented parsing_buffer. *)

  val make_stoppable_merge_error :
    ?name:string ->
    ?filename:string ->
    next:(parsing_buffer ->
          [ `not_ready
          | `output of ('a,
                        [> `incomplete_input of
                            Biocaml_pos.t * string list * string option ]
                          as 'b) Core.Result.t ]) ->
    unit ->
    (string, ('a, 'b) Core.Result.t) t
(** Do like [make_stoppable] but merge [`incomplete_input _] with the
    errors of [~next] (which must be polymorphic variants). *)

  val lines : unit -> (string, string) t
    (** Return a transform that converts a stream of arbitrary strings
        to a stream of lines. If the input terminates without a
        newline, the trailing string is still considered a line. *)

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
    ('input, 'a) t ->
    [ `end_of_stream | `output of 'a ] stream
  (** Create a stream from a feeding function. The transform is
      fed with the function's output ([None] means end-of-stream). *)

  val of_in_channel:
    ?buffer_size:int ->
    in_channel ->
    (string, 'a) t ->
    [ `end_of_stream | `output of 'a ] stream
  (** Create a stream from an [in_channel]. The transformation is fed
      with strings of size [buffer_size] ({i or less}). *)

  val of_file :
    ?buffer_size:int ->
    string ->
    (string, 'a) t ->
    [ `end_of_stream | `output of 'a ] stream
  (** Like [of_in_channel] but internally open the file and close it on
      [`end_of_stream] ({b Warning:} the channel is not closed on [`error _]. *)

  val to_stream_exn:
    error_to_exn:('error -> exn) ->
    [ `end_of_stream | `output of ('output, 'error) Core.Result.t ] stream ->
    'output Stream.t
  (** Convert a stream to an exception-full OCaml [Stream.t]. *)

  val to_stream_result:
    [ `end_of_stream | `output of ('output, 'error) Core.Result.t ] stream ->
    ('output, 'error) Core.Result.t Stream.t
  (** Convert a stream to an OCaml [Stream.t] of [Result.t] values. *)

end
