(** Streams.

    In general, functions that return a stream return a "fresh"
    stream, meaning that their count is set to 0.
*)
open Core

module Stream : sig
  (** Raised by operations working on more than one stream where all
    streams are expected to be of the same length. *)
  exception Expected_streams_of_equal_length

  (** Return first element in given stream if any and remove it from the
    stream. *)
  val next : 'a Stream.t -> 'a option

  (** Return first element in given stream and remove it from the
    stream.
    @raise Stream.Failure if the stream is empty. *)
  val next_exn : 'a Stream.t -> 'a

  (** True if the stream is empty, else false. *)
  val is_empty : 'a Stream.t -> bool

  (** {6 Constructors} *)

  (** Return a stream of strings from the input. Each string has length
    at most [buffer_size]. *)
  val strings_of_channel : ?buffer_size:int -> In_channel.t -> string Stream.t

  (** [range p until:q] creates a stream of integers [[p, p+1, ..., q]].
    If [until] is omitted, the enumeration is not bounded. Behaviour
    is not-specified once [max_int] has been reached.*)
  val range : ?until:int -> int -> int Stream.t

  (** The empty stream. *)
  val empty : unit -> 'a Stream.t

  (** [singleton x] returns a stream containing the single value [x]. *)
  val singleton : 'a -> 'a Stream.t

  (** [unfold a0 f] returns the stream [b0; b1; ...; bn], where

    - [Some (b0, a1) = f a0],
    - [Some (b1, a2) = f a1],
    - ...
    - [Some (bn, a(n+1)) = f an],
    - [None = f a(n+1)]

    The stream is infinite if [f] never returns None. *)
  val unfold : 'a -> f:('a -> ('b * 'a) option) -> 'b Stream.t

  val of_lazy : 'a Stream.t lazy_t -> 'a Stream.t

  (** {6 Iterators}
    Unless otherwise stated, functions in this section normally
    consume the entire stream. The exception is if a caller supplied
    function raises an exception, but that is not the normal intention
    of supplied functions.
*)

  (** [iter xs ~f] calls in turn [f x0], [f x1], ... *)
  val iter : 'a Stream.t -> f:('a -> unit) -> unit

  (** [fold xs ~init ~f] returns [f (...(f (f init x0) x1)...) xn], that
    is for the stream [a0; a1; ...; an] does the following calculations:

    - b1 = f init a0
    - b2 = f b1 a1
    - ...
    - bn = f b(n-1) a(n-1)

    and returns [bn]
*)
  val fold : 'a Stream.t -> init:'b -> f:('b -> 'a -> 'b) -> 'b

  (** {6 Converters}
    Extract a subset of a stream or map a stream into another type of
    stream.
*)

  (** [take xs ~n] builds a fresh stream from [xs] containing the [d]
    first elements of [xs] where [d = min n l] and [l] is the length
    of [xs]. As it is fresh, the count of the resulting stream starts
    from [0] whatever the count of [xs] is.

    Same as [take] but takes elements from the input enum as long as
    [f] evaluates to [true]. *)
  val take_while : 'a Stream.t -> f:('a -> bool) -> 'a Stream.t

  (** [drop xs ~n] is equivalent to calling [n] times [junk] on [xs].

  Similar to [drop]: [drop_while xs ~f] removes elements from [xs]
    and stops when [f] evals to false on the head element. *)
  val drop_while : 'a Stream.t -> f:('a -> bool) -> unit

  (** Similar to [drop] but returns the stream in input (useful in
    chained composition). *)
  val skip : 'a Stream.t -> n:int -> 'a Stream.t

  val map : 'a Stream.t -> f:('a -> 'b) -> 'b Stream.t
  val filter : 'a Stream.t -> f:('a -> bool) -> 'a Stream.t
  val append : 'a Stream.t -> 'a Stream.t -> 'a Stream.t
  val concat : 'a Stream.t Stream.t -> 'a Stream.t
  val concat_map : 'a Stream.t -> f:('a -> 'b Stream.t) -> 'b Stream.t

  (** {6 Data Interchange}
    Convert/create a stream to/from another data structure.
*)

  val of_list : 'a list -> 'a Stream.t
  val to_list : 'a Stream.t -> 'a list
  val of_hashtbl : ('a, 'b) Hashtbl.t -> ('a * 'b) Stream.t
  val to_set : 'a Stream.t -> 'a Set.Poly.t

  (** {6 Result.t's} *)

  (** Convert exception-less stream to exception-ful stream. Resulting
    stream raises exception at first error seen. *)
  val result_to_exn
    :  ('output, 'error) Result.t Stream.t
    -> error_to_exn:('error -> exn)
    -> 'output Stream.t

  (** Higher-order functions for streams of results

    The functions in this module can be used to iterate on a stream of
    results with a function that only considers the non-error
    case. The iterated function thus doesn't need to do the pattern
    match on each [Result] value.

    For each kind of iteration, two versions are proposed. One for
    total functions (that is, functions that cannot fail), the other
    for partial functions (they are assumed to return a [Result] in
    this case).
*)
  module Result : sig
    type ('a, 'e) t = ('a, 'e) Result.t Stream.t

    (** Generalization of [map] with two streams of results. If the two
      streams fail simultaneously, one of the two errors is propagated. *)
    val map2_exn
      :  ('a, 'e) t
      -> ('b, 'e) t
      -> f:('a -> 'b -> ('c, 'e) Result.t)
      -> ('c, 'e) t

    (** Analoguous of [map2_exn] for total functions *)
    val map2_exn' : ('a, 'e) t -> ('b, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'e) t

    (** [fold rs ~init ~f] computes a value by iterating [f] on each
      [Ok] element of [rs] starting from [init]. The computation stops
      with an [Error] case as soon as one is met on the stream, or
      when [f] returns one. *)
    val fold
      :  ('a, 'e) t
      -> init:'b
      -> f:('b -> 'a -> ('b, 'e) Result.t)
      -> ('b, 'e) Result.t

    (** Same as [fold], but for total functions. *)
    val fold' : ('a, 'e) t -> init:'b -> f:('b -> 'a -> 'b) -> ('b, 'e) Result.t
  end
end
