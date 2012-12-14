(** Buffered transforms. A buffered transform represents a method for
    converting a stream of [input]s to a stream of [output]s. However,
    [input]s can also be buffered, i.e. you can feed [input]s to the
    transform and pull out [output]s later. There is no requirement
    that 1 input produces exactly 1 output. It is common that multiple
    (contiguous) input values are needed to construct a single output,
    and vice versa.

    Buffered transforms serve as a general method for working with
    streams of data and flexibly composing mappings from [input]s to
    [output]s. The buffering aspect supports asynchronous programming
    interfaces. Parsers and printers throughout Biocaml are
    implemented with this module whenever possible.

    Often mappings need to account for errors, e.g. an input string
    cannot be converted to an integer. Several methods below
    explicitly support buffered transforms where the output type is a
    [Result.t].
*)

(** Type of a buffered transform converting ['input]s to
    ['output]s. *)
type ('input, 'output) t

(** Exception thrown when {!feed} is called on a transform after it
    has been {!stop}ped. *)
exception Feeding_stopped_transformation of string

(** [make_stoppable ~feed ~next ()] creates a transform that can be
    fed with [feed] and read from with [next].

    - [feed input] should store [input] in a buffer, which is
    presumably a shared state also available to [next].

    - [next stopped] should remove values from the buffer, convert it
    to an [`output] and return this output, or return [`not_ready] if
    there are not enough buffered inputs to create an output value, or
    return [`end_of_stream] if the buffer has been stopped, as
    determined by the supplied argument, and there is no more
    input. Depending on the specifics of the transform, it may be the
    case that the buffer has been stopped but there is not enough
    input to create an output value. It is the caller's choice how to
    handle this or any other kind of error, e.g. make the return type
    a [Result.t].

    - [name] an optional name for the transform that will be used in
    error messages.
*)
val make_stoppable:
  ?name:string ->
  feed: ('input -> unit) ->
  next: (bool -> [ `output of 'output | `end_of_stream | `not_ready ]) ->
  unit ->
  ('input, 'output) t

(** [feed t i] stores [i] into the buffered transform.

    @raise Feeding_stopped_transformation [name] if called on a [t]
    that has been {!stop}ped.
*)
val feed: ('input, 'output) t -> 'input -> unit

(** [next t] returns an output value if possible, [`not_ready] if [t]
    needs to be fed with more input before it can produce an output,
    or [`end_of_stream] if [t] has been stopped and has no more
    data. *)
val next: ('input, 'output) t -> [ `output of 'output | `end_of_stream | `not_ready ]

(** [stop t] declares [t] to be stopped, which means subsequent calls to:

    - [feed t _] will raise [Feeding_stopped_transformation]. Feeding
    a stopped transform is not allowed.

    - [next t] will eventually return [`end_of_stream], not
    necessarily the immediate next call as there may still be
    buffered values available for output.
*)
val stop: ('input, 'output) t -> unit

(** [name t] returns the name of [t]. *)
val name: ('input, 'output) t -> string option

(** [identity ()] returns a transform that simply returns its inputs
    as outputs without modification. *)
val identity: ?name:string -> unit -> ('a, 'a) t

(** [stream_transformation t] returns a function [f] that behaves like
    [t] but the inputs and outputs are on standard OCaml streams. *)
val stream_transformation:
  ('input, 'output) t -> ('input Stream.t -> 'output Stream.t)

(** [on_input f t] returns a transform that converts its inputs with
    [f] and feeds the results to [t]. *)
val on_input: ('b, 'c) t -> f:('a -> 'b) -> ('a, 'c) t

(** [on_output t f] returns a transform that behaves like [t] except
    the outputs are first converted by [f]. *)
val on_output: ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t

(** [compose t u] composes [t] and [u]. *)
val compose: ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

(** [mix t u f] returns a transform that takes as input a pair of the
    inputs expected by [t] and [u], and outputs a single value that is the
    result of applying [f] to the outputs of [t] and [u]. *)
val mix : ('a1, 'b1) t -> ('a2, 'b2) t -> f:('b1 -> 'b2 -> 'c) -> ('a1 * 'a2, 'c) t

(** [partially_compose t u ~destruct ~reconstruct] produces a
    transform that feeds a filtered subset of [t]s outputs to
    [u]. Only those outputs [ol] of [t] for which [destruct ol]
    returns [`Yes] are passsed on to [u]. The filterd out values are
    combined with [u]'s output using [reconstruct]. *)
val partially_compose:
  ('il, 'ol) t -> ('ir, 'our) t ->
  destruct:('ol -> [`Yes of 'ir | `No of 'filtered]) ->
  reconstruct:([`Filtered of 'filtered | `Done of 'our] -> 'result) ->
  ('il, 'result) t

(** [split_and_merge t u ~split ~merge] returns a transform whose
    input is split using [split], passing the result either to [t] or [u],
    and then the outputs of [t] and [u] are combined using [merge]. There
    is no guarantee about the order in which the inputs are fed to [t] and
    [u] (it depends on the buffering done by the individual input
    transforms). *)
val split_and_merge:
  ('il, 'ol) t -> ('ir, 'our) t ->
  split:('input -> [`left of 'il | `right of 'ir]) ->
  merge:([`left of 'ol | `right of 'our] -> 'output) ->
  ('input, 'output) t


(** {6 [Result.t] Outputs} *)

(** Like {!make_stoppable} but the output is a [Result.t]. Also,
    {!stop} is automatically called when an error occurs. *)
val make_stoppable_with_error:
  ?name:string ->
  feed: ('input -> unit) ->
  next: (bool -> [ `output of ('a, 'b) Core.Result.t | `end_of_stream | `not_ready ]) ->
  unit ->
  ('input, ('a, 'b) Core.Result.t) t

(** [bind_result t u] is like {!compose} but for transforms returning
    [Result.t]s. The [on_error] function specifies how errors in [t]
    or [u] should be converted into those in the resultant
    transform. *)
val bind_result:
  on_error:([`left of 'error_left | `right of 'error_right ] -> 'error) ->
  ( 'input_left, ('middle, 'error_left) Core.Result.t) t ->
  ( 'middle, ('output_right, 'error_right) Core.Result.t) t ->
  ( 'input_left, ('output_right, 'error) Core.Result.t) t

(** Like {!bind_result} but with a pre-specified [on_error]
    function. *)
val bind_result_merge_error:
  ('a, ('b, 'el) Core.Result.t) t ->
  ('b, ('d, 'er) Core.Result.t) t ->
  ('a, ('d, [ `left of 'el | `right of 'er ]) Core.Result.t) t

(** Like {!bind_result} but only the first transform returns
    [Result.t]s. *)
val map_result:
  ( 'input_left, ('middle, 'error) Core.Result.t) t ->
  ( 'middle, 'output_right) t ->
  ( 'input_left, ('output_right, 'error) Core.Result.t) t


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


(** {6 Low-level API} *)

val make:
  ?name:string ->
  next: (unit -> [ `output of 'output | `end_of_stream | `not_ready ]) ->
  feed: ('input -> unit) ->
  stop: (unit -> unit) ->
  unit ->
  ('input, 'output) t
    (** Build a basic transformation. *)
