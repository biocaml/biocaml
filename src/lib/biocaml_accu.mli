(** 
    A datastructure (based on Hashtbl) to accumulate values.

    An [Accu.t] can be seen as a generalized histogram: values 
    are mapped to bins, and each bin has a corresponding value
    which may be its size or its contents depending on the need.
*)
open Batteries

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

val enum : ('a,'b,'c,'d) t -> ('b * 'd) Enum.t

val get : ('a,'b,'c,'d) t -> 'b -> 'd
(** [get accu x] returns the value associated to [b] in [accu]. *)


(** {7 Counters and histograms} *)

type 'instance counter = ('instance, 'instance, int, int) t
(** The type of accumulators that count values *)

module Counter : sig
  type 'a t = 'a counter
  val create : ?n:int -> unit -> 'a t
  val add : 'a counter -> 'a -> int -> unit
  val tick : 'a counter -> 'a -> unit
end

val counts : ('a -> 'b) -> 'a Enum.t -> ('b * int) Enum.t


(** {7 Relation} *)

type ('a, 'b) relation = ('a,'a,'b,'b list) t

module Relation : sig
  type ('a, 'b) t = ('a,'b) relation
  val create : ?n:int -> unit -> ('a,'b) t
  val add : ('a,'b) t -> 'a -> 'b -> unit
end

val relation : ('a * 'b) Enum.t -> ('a * 'b list) Enum.t




















