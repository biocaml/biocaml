(** Manipulate the lines of a file. *)

type item = Line.t
[@@deriving sexp]

module MakeIO (Future : Future.S) : sig
  open Future

  val read : Reader.t -> item Pipe.Reader.t

  val write : Writer.t -> item Pipe.Reader.t -> unit Deferred.t

  val write_file
    : ?perm:int
    -> ?append:bool
    -> string
    -> item Pipe.Reader.t
    -> unit Deferred.t

end
include module type of MakeIO(Future_unix)

val of_char_stream : char Stream.t -> item Stream.t
(** Parse a stream of characters into a stream of lines. *)

val of_channel : in_channel -> item Stream.t
(** Get a stream of lines out of an input-channel. *)

val of_string : string -> item Stream.t
(** Get a stream of lines out a string *)

val to_channel : item Stream.t -> out_channel -> unit
(** Write a stream of lines to an output-channel. *)

module Buffer : sig
  (** Buffer used to parse strings into lines. *)

  type t
  (** The buffer handle. *)

  exception No_next_line
  (** The exception thrown by [next_line_exn]. *)

  val make: ?filename:string -> unit -> t
  (** Make a new empty buffer. The optional [filename] is used only
      for error reporting; it should be set to the name of the file,
      if any, from which you will feed the buffer. *)

  val feed_line: t -> item -> unit
  (** Feed the parser with a line. *)

  val feed_string: t -> string -> unit
  (** Feed the parser with an arbitrary string buffer. *)

  val queued_lines: t -> int
  (** Get the number of lines ready-to-use in the buffer/queue. *)

  val is_empty: t -> bool
  (** Tell if the parser's buffers are empty or not. For instance,
      when there is no more content to feed and [next_line] returns
      [None], [is_empty p = true] means that the content did not end
      with a complete line. *)

  val peek_line: t -> item option
  (** Peek at the next line, without removing it from the buffer. *)

  val next_line: t -> item option
  (** Get the next line. *)

  val next_line_exn: t -> item
  (** Get the next line, but throw [No_next_line] if there is no line
      to return. *)

  val current_position: t -> Pos.t
  (** Get the current position in the stream. *)

  val contents : t -> item list * string option
  (** Return any remaining lines and the unfinished string, without
      removing them from the buffer. *)

  val empty : t -> unit
  (** Empty the buffer. Subsequent call to [contents] will return
      [(\[\], None)]. *)

end

module Transform : sig
  (** Transforms from/to [Lines.item]. *)

  (** Return a transform that converts a stream of arbitrary strings
      to a stream of lines. If the input terminates without a newline,
      the trailing string is still considered a line. *)
  val string_to_item : unit -> (string, item) Tfxm.t

  (** Return a transform that converts a stream of lines to a stream
      of pairs of lines. It is considered an error if input ends with an
      odd number of lines. *)
  val group2 :
    unit ->
    (item,
    (item * item, [> `premature_end_of_input ]) result) Tfxm.t

  val item_to_string: ?buffer:[ `clear of int | `reset of int ] ->
    unit -> (item, string) Tfxm.t
  (** Return a transform that output [Line.item]s to strings (in other
      words a buffer with the lines {i plus} their end-of-line
      character). *)

  (** Build a stoppable line-oriented parsing_buffer. *)
  val make : ?name:string -> ?filename:string ->
    next:(Buffer.t ->
      [ `not_ready | `output of ('b, 'errnext) result ]) ->
    on_error:(
      [`next of 'errnext
      | `incomplete_input of Pos.t * string list * string option] ->
        'err) ->
    unit ->
    (string, ('b, 'err) result) Tfxm.t

  (** Do like [make] but merge [`incomplete_input _] with the
      errors of [~next] (which must be polymorphic variants). *)
  val make_merge_error :
    ?name:string ->
    ?filename:string ->
    next:(Buffer.t ->
      [ `not_ready
      | `output of ('a,
                   [> `incomplete_input of
                     Pos.t * string list * string option ]
                     as 'b) result ]) ->
    unit ->
    (string, ('a, 'b) result) Tfxm.t

end
