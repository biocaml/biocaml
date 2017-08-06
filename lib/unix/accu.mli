(**
    A datastructure (based on Hashtbl) to accumulate values.

    An [Accu.t] can be seen as a generalized histogram: samples are
    mapped to bins, and each bin has a corresponding value which may
    be its size or its contents depending on the need.
*)
open Core_kernel

(** {7 Generic API} *)

type ('sample,'bin,'increment,'accu) t
(** General type for accumulators: ['sample]s are mapped to ['bin]s,
    and the ['accu]mulated value for a ['bin] is updated with an
    ['increment] *)

val create :
  ?n:int ->
  bin:('a -> 'b) ->
  zero:'d ->
  add:('c -> 'd -> 'd) ->
  unit ->
  ('a,'b,'c,'d) t
(** [create ~n ~zero ~bin ~add] creates an accumulator, which maps
    instances to bins with [bin], uses [zero] as a neutral element
    (that is the value associated to a bin before any value has been
    added to it) and updates the value of a bin with [add]. [n] is an
    estimation of the maximum number of bins. *)

val add : ('a,'b,'c,'d) t -> 'a -> 'c -> unit
(** [add accu x y] updates the value in [accu] for
    the bin of [x] by an increment [y] *)

val stream : ('a,'b,'c,'d) t -> ('b * 'd) Stream.t

val get : ('a,'b,'c,'d) t -> 'b -> 'd option
(** [get accu x] returns the value associated to [b] in [accu]. *)


(** {7 Counters and histograms} *)

module Counter : sig
  type nonrec 'a t = ('a, 'a, int, int) t
  val create : ?n:int -> unit -> 'a t
  val add : 'a t -> 'a -> int -> unit
  val tick : 'a t -> 'a -> unit
  val stream : 'a t -> ('a * int) Stream.t
  val of_stream : 'a Stream.t -> 'a t
end

val counts  : 'a Stream.t -> ('a * int) Stream.t

val product :
  ?filter:('a -> 'b -> bool) ->
  ('a -> 'b -> 'c) ->
  'a list -> 'b list ->
  ('c * int) Stream.t
(** [product filter f l1 l2] computes an histogram of values returned by f
    when it is applied for all combinations of elements in [l1] and
    [l2] such that the predicate [filter] is true *)

(** {7 Relation} *)

module Relation : sig
  type nonrec ('a, 'b) t = ('a,'a,'b,'b list) t
  val create : ?n:int -> unit -> ('a,'b) t
  val add : ('a,'b) t -> 'a -> 'b -> unit
  val stream : ('a,'b) t -> ('a * 'b list) Stream.t
  val of_stream : ('a * 'b) Stream.t -> ('a, 'b) t
end

val relation : ('a * 'b) Stream.t -> ('a * 'b list) Stream.t
