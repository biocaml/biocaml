(** Streams.

    In general, functions that return a stream return a "fresh"
    stream, meaning that their count is set to 0.
*)
open Core

module Stream : sig
  (** Type of streams holding values of type ['a]. *)
  type 'a t = 'a Stream.t

  (** Return a stream containing all elements of given data
      structure. Exact semantics depend on implementation. For
      example, elements in stream may or may not be ordered. *)
  val to_stream : 'a t -> 'a Stream.t

  (** Return a data structure containing all elements in given stream,
          fully consuming the stream. Exact semantics depend on
          implementation. For example, duplicate elements in input may be
          ignored if the data structure is a set. *)
  val of_stream : 'a Stream.t -> 'a t

  (** Raised when asking for an element of an empty stream, and by
    {!Genlex} parsers when none of the first components of the stream
    patterns is accepted.
*)
  exception Failure

  (** Raised by {!Genlex} parsers when the first component of a stream
    pattern is accepted, but one of the following components is
    rejected.
*)
  exception Error of string

  (** Raised by operations working on more than one stream where all
    streams are expected to be of the same length. *)
  exception Expected_streams_of_equal_length

  (** Raised when an operation needs more elements from a stream than
    available. *)
  exception Premature_end_of_input

  (** Return first element in given stream if any and remove it from the
    stream. *)
  val next : 'a t -> 'a option

  (** Return first element in given stream and remove it from the
    stream.
    @raise Stream.Failure if the stream is empty. *)
  val next_exn : 'a t -> 'a

  (** Return first element of given stream without removing it from the
    stream, or [None] if the stream is empty. *)
  val peek : 'a t -> 'a option

  (** [npeek s n] returns a list of the first [n] elements in stream
    [s], or all of its remaining elements if less than [n] elements
    are available. The elements are not removed from the stream. *)
  val npeek : 'a t -> int -> 'a list

  (** Discard first element of given stream or do nothing if the stream
    is empty. *)
  val junk : 'a t -> unit

  (** Return number of elements discarded from given stream. *)
  val count : 'a t -> int

  (** True if the stream is empty, else false. *)
  val is_empty : 'a t -> bool

  (** {6 Constructors} *)

  (** [from f] returns a stream whose [n]th element is determined by
    calling [f n], which should return [Some x] to indicate value [x]
    or [None] to indicate the end of the stream. The stream is
    infinite if [f] never returns None. *)
  val from : (int -> 'a option) -> 'a t

  (** Return a stream of characters by reading from the input
    channel. WARNING: Semantics unclear if the channel is closed
    before the stream reads all of its input. For example, the stream
    appears to return values although the channel has been closed. *)
  val of_channel : In_channel.t -> char t

  (** Return a stream of strings from the input. Each string has length
    at most [buffer_size]. *)
  val strings_of_channel : ?buffer_size:int -> In_channel.t -> string t

  (** [range p until:q] creates a stream of integers [[p, p+1, ..., q]].
    If [until] is omitted, the enumeration is not bounded. Behaviour
    is not-specified once [max_int] has been reached.*)
  val range : ?until:int -> int -> int t

  (** The empty stream. *)
  val empty : unit -> 'a t

  (** [init n f] returns the stream [f 0; f 1; ... f (n-1)]. *)
  val init : int -> f:(int -> 'a) -> 'a t

  (** [singleton x] returns a stream containing the single value [x]. *)
  val singleton : 'a -> 'a t

  (** [unfold a0 f] returns the stream [b0; b1; ...; bn], where

    - [Some (b0, a1) = f a0],
    - [Some (b1, a2) = f a1],
    - ...
    - [Some (bn, a(n+1)) = f an],
    - [None = f a(n+1)]

    The stream is infinite if [f] never returns None. *)
  val unfold : 'a -> f:('a -> ('b * 'a) option) -> 'b t

  (** Indexed variant of [unfold] *)
  val unfoldi : 'a -> f:(int -> 'a -> ('b * 'a) option) -> 'b t

  val of_lazy : 'a t lazy_t -> 'a t

  (** {6 Iterators}
    Unless otherwise stated, functions in this section normally
    consume the entire stream. The exception is if a caller supplied
    function raises an exception, but that is not the normal intention
    of supplied functions.
*)

  (** [iter xs ~f] calls in turn [f x0], [f x1], ... *)
  val iter : 'a t -> f:('a -> unit) -> unit

  (** Like [iter] but operates on two streams. Stops when either stream
    becomes empty. *)
  val iter2 : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit

  (** Like [iter2] except streams required to be of equal length.
    @raise Expected_streams_of_equal_length if the two streams have
    different lengths, in which case there is no guarantee about which
    elements were consumed. *)
  val iter2_exn : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit

  (** [fold xs ~init ~f] returns [f (...(f (f init x0) x1)...) xn], that
    is for the stream [a0; a1; ...; an] does the following calculations:

    - b1 = f init a0
    - b2 = f b1 a1
    - ...
    - bn = f b(n-1) a(n-1)

    and returns [bn]
*)
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b

  (** Like [fold] but operates on two streams. Processing continues
    until either stream becomes empty. *)
  val fold2 : 'a t -> 'b t -> init:'c -> f:('c -> 'a -> 'b -> 'c) -> 'c

  (** Like [fold2] except streams required to be of equal length.
    @raise Expected_streams_of_equal_length if the two streams have
    different lengths, in which case there is no guarantee about which
    elements were consumed. *)
  val fold2_exn : 'a t -> 'b t -> init:'c -> f:('c -> 'a -> 'b -> 'c) -> 'c

  (** Like [fold] but all intermediate values are returned, not just the
    final value. If given stream [s] is [a0; a1; ...], then [scanl f
    init s] is the stream containing

    - b0 = init
    - b1 = f b0 a0
    - b2 = f b1 a1
    - ...
. *)
  val scanl : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t

  (** [scan] is similar to [scanl] but without the [init] value: if [s]
    contains [x0], [x1], [x2] ..., [scan s ~f] contains

    - y0 = x0
    - y1 = f y0 x1
    - y2 = f y1 x2
    - ...

    For instance, [scan (1 -- 10) ~f:( * )] will produce an
    enumeration containing the successive values of the factorial
    function. Returns an empty stream if the input stream is empty as
    well. *)
  val scan : 'a t -> f:('a -> 'a -> 'a) -> 'a t

  (** Indexed variants of the previous higher-order functions. The index
    provided to the [~f] argument is the count of the stream, that is
    the number of discarded elements before the reaching the current
    one. For functions iterating on two streams, the [~f] is thus
    provided two indices, since the current count may differ from one
    stream to another. *)
  val iteri : 'a t -> f:(int -> 'a -> unit) -> unit

  val iter2i_exn : 'a t -> 'b t -> f:(int -> int -> 'a -> 'b -> unit) -> unit
  val iter2i : 'a t -> 'b t -> f:(int -> int -> 'a -> 'b -> unit) -> unit
  val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b) -> 'b
  val fold2i_exn : 'a t -> 'b t -> init:'c -> f:(int -> int -> 'c -> 'a -> 'b -> 'c) -> 'c
  val fold2i : 'a t -> 'b t -> init:'c -> f:(int -> int -> 'c -> 'a -> 'b -> 'c) -> 'c

  (** [reduce xs ~f] returns [f (...(f (f x1 x2) x3)...) xn] *)
  val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a

  (** [sum xs] returns the sum of the integers contained in [xs] *)
  val sum : int t -> int

  (** [fsum xs] returns the sum of the floats contained in [xs] *)
  val fsum : float t -> float

  (** {6 Scanners}
    Operations that scan a stream for various purposes. Unlike
    iterators, these operations are not inherently meant to consume
    streams, although they do partially or fully, due to the nature of
    streams.
*)

  (** [exists s ~f] returns [true] if there is some [x] in [s] such that
    [f x] is true. The stream is consumed through and including
    [x]. *)
  val exists : 'a t -> f:('a -> bool) -> bool

  (** [for_all s ~f] returns [true] if [f x] is true for every [x] in
    [s]. *)
  val for_all : 'a t -> f:('a -> bool) -> bool

  (** [find e ~f] returns either [Some x] where [x] is the first
    element of [e] such that [f x] returns [true], consuming the
    stream up to and including the found element, or [None] if no
    such element exists in the stream, consuming the whole stream in
    the search.

    Since [find] (eagerly) consumes a prefix of the stream, it
    can be used several times on the same stream to find the
    next element. *)
  val find : 'a t -> f:('a -> bool) -> 'a option

  (** Same as [find] except that it raises an exception [Not_found]
    instead of returning [None]. *)
  val find_exn : 'a t -> f:('a -> bool) -> 'a

  (** Similar to [find] *)
  val find_map : 'a t -> f:('a -> 'b option) -> 'b option

  (** {6 Converters}
    Extract a subset of a stream or map a stream into another type of
    stream.
*)

  (** [take xs ~n] builds a fresh stream from [xs] containing the [d]
    first elements of [xs] where [d = min n l] and [l] is the length
    of [xs]. As it is fresh, the count of the resulting stream starts
    from [0] whatever the count of [xs] is. *)
  val take : 'a t -> n:int -> 'a t

  (** Same as [take] but takes elements from the input enum as long as
    [f] evaluates to [true]. *)
  val take_while : 'a t -> f:('a -> bool) -> 'a t

  (** [drop xs ~n] is equivalent to calling [n] times [junk] on [xs]. *)
  val drop : 'a t -> n:int -> unit

  (** Similar to [drop]: [drop_while xs ~f] removes elements from [xs]
    and stops when [f] evals to false on the head element. *)
  val drop_while : 'a t -> f:('a -> bool) -> unit

  (** Similar to [drop] but returns the stream in input (useful in
    chained composition). *)
  val skip : 'a t -> n:int -> 'a t

  (** Similar to [skip]: [skip_while xs ~f] removes elements from [xs]
    and stops when [f] evals to false on the head element. *)
  val skip_while : 'a t -> f:('a -> bool) -> 'a t

  (** Indexed variants of the previous prefix/suffix constructors *)
  val take_whilei : 'a t -> f:(int -> 'a -> bool) -> 'a t

  val drop_whilei : 'a t -> f:(int -> 'a -> bool) -> unit
  val skip_whilei : 'a t -> f:(int -> 'a -> bool) -> 'a t

  (** [span test e] produces two streams [(hd, tl)], such that
    [hd] is the same as [take_while test e] and [tl] is the same
    as [skip_while test e]. *)
  val span : 'a t -> f:('a -> bool) -> 'a t * 'a t

  (** [group xs f] applies [f] to the elements of [xs] and distribute
    them according to the return value of [f]. Let [ys] = [group xs
    f], then [xs] = [concat ys] and in each stream [s] of [ys], all
    values give the same value with [f]. *)
  val group : 'a t -> f:('a -> 'b) -> 'a t t

  (** Same as [group] but with a comparison function instead of a
    mapping. *)
  val group_by : 'a t -> eq:('a -> 'a -> bool) -> 'a t t

  (** Given a stream with items [x0, x1, x2, x3,...], the returned stream
    will be pairs of items [(x0,x1), (x2,x3), ...].

    @raise Premature_end_of_input if input stream has an odd number of
    elements. *)
  val chunk2 : 'a t -> ('a * 'a) t

  (** Like [chunk2] but for 3-tuples. *)
  val chunk3 : 'a t -> ('a * 'a * 'a) t

  (** Like [chunk2] but for 4-tuples. *)
  val chunk4 : 'a t -> ('a * 'a * 'a * 'a) t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
  val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val mapi2_exn : 'a t -> 'b t -> f:(int -> 'a -> 'b -> 'c) -> 'c t
  val filter : 'a t -> f:('a -> bool) -> 'a t
  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
  val append : 'a t -> 'a t -> 'a t
  val concat : 'a t t -> 'a t
  val concat_map : 'a t -> f:('a -> 'b t) -> 'b t

  (** [combine] transforms a pair of streams into a stream of pairs of
    corresponding elements. If one stream is short, excess elements
    of the longer stream are ignored. *)
  val combine : 'a t * 'b t -> ('a * 'b) t

  (** [uncombine] is the opposite of [combine] *)
  val uncombine : ('a * 'b) t -> 'a t * 'b t

  (** [merge test (a, b)] merge the elements from [a] and [b] into a
      single stream. At each step, [test] is applied to the first
      element of [a] and the first element of [b] to determine which
      should get first into the resulting stream. If [a] or [b]
      runs out of elements, the process will append all elements of
      the other stream to the result.  *)
  val merge : 'a t -> 'a t -> cmp:('a -> 'a -> int) -> 'a t

  (** [partition e ~f] splits [e] into two streams, where the first
      stream have all the elements satisfying [f], the second stream
      is opposite. The order of elements in the source stream is
      preserved. *)
  val partition : 'a t -> f:('a -> bool) -> 'a t * 'a t

  (** [uniq e] returns a duplicate of [e] with repeated values
      omitted. (similar to unix's [uniq] command) *)
  val uniq : 'a t -> 'a t

  (** {6 Data Interchange}
    Convert/create a stream to/from another data structure.
*)

  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val of_array : 'a array -> 'a t
  val to_array : 'a t -> 'a array
  val of_hashtbl : ('a, 'b) Hashtbl.t -> ('a * 'b) t
  val to_hashtbl : ('a * 'b) t -> ('a, 'b) Hashtbl.t
  val of_map : ('a, 'b, 'c) Map.t -> ('a * 'b) t
  val to_map : ('a * 'b) t -> ('a, 'b) Map.Poly.t
  val of_set : ('a, 'b) Set.t -> 'a t
  val to_set : 'a t -> 'a Set.Poly.t
  val of_string : string -> char t

  (** {6 Result.t's} *)

  (** Convert exception-less stream to exception-ful stream. Resulting
    stream raises exception at first error seen. *)
  val result_to_exn
    :  ('output, 'error) Result.t t
    -> error_to_exn:('error -> exn)
    -> 'output t

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
