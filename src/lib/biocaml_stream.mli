(** This module is an enhancement of
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Stream.html}the
    stdlib's [Stream] module}. It is largely inspired from
    {{:http://ocaml-batteries-team.github.com/batteries-included/hdoc2/BatEnum.html}Batteries's
    [Enum] module}. However, it is written in a more core-styled
    way. *)

(** Type of streams holding values of type ['a]. *)
type 'a t = 'a Stream.t

include Biocaml_streamable.S with type 'a streamable = 'a t

(** Raised when asking for an element of an empty stream, and by
    {!Genlex} parsers when none of the first components of the stream
    patterns is accepted.

exception Failure
*)

(** Raised by {!Genlex} parsers when the first component of a stream
    pattern is accepted, but one of the following components is
    rejected.

exception Error of string
*)

(** Raised by operations working on more than one stream where all
    streams are expected to be of the same length. *)
exception Expected_streams_of_equal_length

(** Return first element in given stream if any and remove it from the
    stream. *)
val next: 'a t -> 'a option

(** Return first element in given stream and remove it from the
    stream.
    @raise {!Stream.Failure} if the stream is empty. *)
val next_exn: 'a t -> 'a

(** Return first element of given stream without removing it from the
    stream, or [None] if the stream is empty. *)
val peek : 'a t -> 'a option

(** [npeek n s] returns a list of the first [n] elements in stream
    [s], or all of its remaining elements if less than [n] elements
    are available. The elements are not removed from the stream. *)
val npeek : int -> 'a t -> 'a list

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

(** Return a stream of elements as occurring in the given list. *)
val of_list : 'a list -> 'a t

(** Return a stream characters as occurring in the given string. *)
val of_string : string -> char t

(** Return a stream of characters by reading from the input
    channel. WARNING: Semantics unclear if the channel is closed
    before the stream reads all of its input. For example, the stream
    appears to return values although the channel has been closed. *)
val of_channel : in_channel -> char t

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

(** [loop a0 ~f] returns the stream [b0; b1; ...; bn], where

    - [b0 = a0]
    - [Some b1 = f 1 b0]
    - [Some b2 = f 2 b1]
    - ...
    - [Some bn = f n b(n-1)]
    - [None = f (n+1) bn]

    The stream is infinite if [f] never returns None.
*)
val loop : 'a -> f:(int -> 'a -> 'a option) -> 'a t

(** [unfold a0 f] returns the stream [b0; b1; ...; bn], where

    - [Some (b0, a1) = f a0],
    - [Some (b1, a2) = f a1],
    - ...
    - [Some (bn, a(n+1)) = f an],
    - [None = f a(n+1)]

    The stream is infinite if [f] never returns None. *)
val unfold : 'a -> ('a -> ('b * 'a) option) -> 'b t


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

(** [fold xs ~init ~f] returns [f (...(f (f init x0) x1)...) xn]. *)
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
    final value. If given stream [s] is [x0; x1; ...], then [scanl f
    init s] is the stream containing [init; f init x0; f (f init x0)
    x1; f (f (f init x0) x1) x2, ...]. *)
val scanl : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t

(** [scan] is similar to [scanl] but without the [init] value: if [s]
    contains [x0], [x1], [x2] ..., [scan s ~f] is the enumeration
    containing [x0], [f x0 x1], [f (f x0 x1) x2]...

    For instance, [scan (1 -- 10) ~f:( * )] will produce an enumeration
    containing the successive values of the factorial function.*)
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

val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a
(** [reduce xs ~f] returns [f (...(f (f x1 x2) x3)...) xn] *)

val sum : int t -> int
(** [sum xs] returns the sum of the integers contained in [xs] *)

val fsum : float t -> float 
(** [fsum xs] returns the sum of the floats contained in [xs] *)


(** {6 Scanners}
    Operations that scan a stream for various purposes. Unlike
    iterators, these operations are not inherently meant to consume
    streams, although they do partially or fully, due to the nature of
    streams.
*)

(** [exists s ~f] returns [true] if there is some [x] in [s] such that
    [f x] is true. The stream is consumed through and including
    [x]. *)
val exists: 'a t -> f:('a -> bool) -> bool

(** [for_all s ~f] returns [true] if [f x] is true for every [x] in
    [s]. *)
val for_all: 'a t -> f:('a -> bool) -> bool

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
    Extract a subset of a stream, map a stream into another type of
    stream, or convert a stream into another data structure.
*)

(** [take n xs] builds a fresh stream from [xs] containing the [d]
    first elements of [xs] where [d = min n l] and [l] is the length
    of [xs]. As it is fresh, the count of the resulting stream starts
    from [0] whatever the count of [xs] is. *)
val take : int -> 'a t -> 'a t

(** Same as [take] but takes elements from the input enum as long as
    [f] evaluates to [true]. *)
val take_while : 'a t -> f:('a -> bool) -> 'a t

(** [drop n xs] is equivalent to calling [n] times [junk] on [xs]. *)
val drop : int -> 'a t -> unit

(** Similar to [drop]: [drop_while xs ~f] removes elements from [xs]
    and stops when [f] evals to false on the head element. *)
val drop_while : 'a t -> f:('a -> bool) -> unit

(** Similar to [drop] but returns a fresh stream obtained after
    discarding the [n] first elements. Being a fresh stream, the count
    of the returned stream starts from 0. Beware though, that the
    input and output streams are consuminmg the same resource, so
    consuming one modify the other. *)
val skip : int -> 'a t -> 'a t

(** Similar to [skip]: [skip_while xs ~f] removes elements from [xs]
    and stops when [f] evals to false on the head element. *)
val skip_while : 'a t -> f:('a -> bool) -> 'a t

(** Indexed variants of the previous prefix/suffix constructors *)
val take_whilei : 'a t -> f:(int -> 'a -> bool) -> 'a t
val drop_whilei : 'a t -> f:(int -> 'a -> bool) -> unit
val skip_whilei : 'a t -> f:(int -> 'a -> bool) -> 'a t

val span : 'a t -> f:('a -> bool) -> 'a t * 'a t
(** [span test e] produces two streams [(hd, tl)], such that
    [hd] is the same as [take_while test e] and [tl] is the same
    as [skip_while test e]. *)

val group : 'a t -> f:('a -> 'b) -> 'a t t
(** [group xs f] applies [f] to the elements of [xs] and distribute
    them according to the return value of [f]. Let [ys] = [group xs
    f], then [xs] = [concat ys] and in each stream [s] of [ys], all
    values give the same value with [f]. *)

val group_by : 'a t -> eq:('a -> 'a -> bool) -> 'a t t
(** Same as [group] but with a comparison function instead of a
    mapping. *)

val map : 'a t -> f:('a -> 'b) -> 'b t
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
val filter : 'a t -> f:('a -> bool) -> 'a t
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
val append : 'a t -> 'a t -> 'a t
val concat : 'a t t -> 'a t

val combine : 'a t * 'b t -> ('a * 'b) t
(** [combine] transforms a pair of streams into a stream of pairs of
    corresponding elements. If one stream is short, excess elements
    of the longer stream are ignored. *)

val uncombine : ('a * 'b) t -> 'a t * 'b t
  (** [uncombine] is the opposite of [combine] *)

val merge : 'a t -> 'a t -> cmp:('a -> 'a -> int) -> 'a t
  (** [merge test (a, b)] merge the elements from [a] and [b] into a
      single stream. At each step, [test] is applied to the first
      element of [a] and the first element of [b] to determine which
      should get first into the resulting stream. If [a] or [b]
      runs out of elements, the process will append all elements of
      the other stream to the result.  *)

val partition : 'a t -> f:('a -> bool) -> 'a t * 'a t
  (** [partition e ~f] splits [e] into two streams, where the first
      stream have all the elements satisfying [f], the second stream
      is opposite. The order of elements in the source stream is
      preserved. *)

val uniq : 'a t -> 'a t
  (** [uniq e] returns a duplicate of [e] with repeated values
      omitted. (similar to unix's [uniq] command) *)

val to_list : 'a t -> 'a list

(** {6 Streams of Lines} *)
val lines_of_chars : char t -> string t
val lines_of_channel : in_channel -> string t
val lines_to_channel : string t -> out_channel -> unit

(** {6 Streams of Result.t Values} *)

(** Convert exception-less stream to exception-ful stream. Resulting
    stream raises exception at first error seen. *)
val result_to_exn :
  ('output, 'error) Core.Std.Result.t t ->
  error_to_exn:('error -> exn) ->
  'output t

module Infix : sig
  val ( -- ) : int -> int -> int t
    (** As [range], without the label.

        [5 -- 10] is the enumeration 5,6,7,8,9,10.
        [10 -- 5] is the empty enumeration*)

  val ( --^ ) : int -> int -> int t
    (** As [(--)] but without the right endpoint

        [5 --^ 10] is the enumeration 5,6,7,8,9.
    *)

  val ( --. ) : (float * float) -> float -> float t
    (** [(a, step) --. b)] creates a float enumeration from [a] to [b] with an
        increment of [step] between elements.

        [(5.0, 1.0) --. 10.0] is the enumeration 5.0,6.0,7.0,8.0,9.0,10.0.
        [(10.0, -1.0) --. 5.0] is the enumeration 10.0,9.0,8.0,7.0,6.0,5.0.
        [(10.0, 1.0) --. 1.0] is the empty enumeration. *)

  val ( --- ) : int -> int -> int t
    (** As [--], but accepts enumerations in reverse order.

        [5 --- 10] is the enumeration 5,6,7,8,9,10.
        [10 --- 5] is the enumeration 10,9,8,7,6,5.*)

  val ( /@ ) : 'a t -> ('a -> 'b) -> 'b t
    (** [s /@ f] is equivalent to [map f s] *)

  val ( // ) : 'a t -> ('a -> bool) -> 'a t
    (** [s // f] is equivalent to [filter f s] *)

  val ( //@ ) : 'a t -> ('a -> 'b option) -> 'b t
  (** [s //@ f] is equivalent to [filter_map f s] *)
end
