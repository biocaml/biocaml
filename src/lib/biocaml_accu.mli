(** 
    A datastructure (based on Hashtbl) to accumulate values.

    An [Accu.t] can be seen as a generalized histogram: values 
    are mapped to bins, and each bin has a corresponding value
    which may be its size or its contents depending on the need.
*)
open Biocaml_internal_pervasives

(** {7 Generic API} *)

type ('instance,'bin,'increment,'accu) t
(** General type for accumulators: ['instance]s are mapped to ['bin]s, 
    and the ['accu]mulated value for a ['bin] is updated with an 
    ['increment] *)

val create : ?n:int -> 'd -> ('a -> 'b) -> ('c -> 'd -> 'd) -> ('a,'b,'c,'d) t
(** [create ~n e f op] creates an accumulator, which maps instances to bins
    with [f], uses [e] as a neutral element (that is the value associated to a 
    bin before any value has been added to it) and updates the value of a bin 
    with [op]. [n] is an estimation of the maximum number of bins. *)

val add : ('a,'b,'c,'d) t -> 'a -> 'c -> unit
(** [add accu x y] updates the value in [accu] for
    the bin of [x] by an increment [y] *)

val stream : ('a,'b,'c,'d) t -> ('b * 'd) Stream.t

val get : ('a,'b,'c,'d) t -> 'b -> 'd option
(** [get accu x] returns the value associated to [b] in [accu]. *)


(** {7 Counters and histograms} *)

type 'instance counter = ('instance, 'instance, int, int) t
(** The type of accumulators that count values *)

module Counter : sig
  type 'a t = 'a counter
  val create : ?n:int -> unit -> 'a t
  val add : 'a counter -> 'a -> int -> unit
  val tick : 'a counter -> 'a -> unit
  val stream : 'a counter -> ('a * int) Stream.t
  val of_stream : 'a Stream.t -> 'a counter
end

val counts  : ('a -> 'b) -> 'a Stream.t -> ('b * int) Stream.t
val product : 
  ?filter:('a -> 'b -> bool) -> 
  ('a -> 'b -> 'c) -> 
  'a list -> 'b list -> 
  ('c * int) Stream.t 
(** [product filter f l1 l2] computes an histogram of values returned by f
    when it is applied for all combinations of elements in [l1] and
    [l2] such that the predicate [filter] is true *)

(** {7 Relation} *)

type ('a, 'b) relation = ('a,'a,'b,'b list) t

module Relation : sig
  type ('a, 'b) t = ('a,'b) relation
  val create : ?n:int -> unit -> ('a,'b) t
  val add : ('a,'b) t -> 'a -> 'b -> unit
  val stream : ('a,'b) relation -> ('a * 'b list) Stream.t
  val of_stream : ('a * 'b) Stream.t -> ('a, 'b) relation
end

val relation : ('a * 'b) Stream.t -> ('a * 'b list) Stream.t
