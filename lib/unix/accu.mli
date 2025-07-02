(**
    A datastructure (based on Hashtbl) to accumulate values.

    An [Accu.t] can be seen as a generalized histogram: samples are
    mapped to bins, and each bin has a corresponding value which may
    be its size or its contents depending on the need.
*)

(** {7 Generic API} *)

(** General type for accumulators: ['sample]s are mapped to ['bin]s,
    and the ['accu]mulated value for a ['bin] is updated with an
    ['increment] *)
type ('sample, 'bin, 'increment, 'accu) t

(** [create ~n ~zero ~bin ~add] creates an accumulator, which maps
    instances to bins with [bin], uses [zero] as a neutral element
    (that is the value associated to a bin before any value has been
    added to it) and updates the value of a bin with [add]. [n] is an
    estimation of the maximum number of bins. *)
val create
  :  ?n:int
  -> bin:('a -> 'b)
  -> zero:'d
  -> add:('c -> 'd -> 'd)
  -> unit
  -> ('a, 'b, 'c, 'd) t

(** [add accu x y] updates the value in [accu] for
    the bin of [x] by an increment [y] *)
val add : ('a, 'b, 'c, 'd) t -> 'a -> 'c -> unit

val stream : ('a, 'b, 'c, 'd) t -> ('b * 'd) CFStream.Stream.t
val to_alist : ('a, 'b, 'c, 'd) t -> ('b * 'd) list

(** [get accu x] returns the value associated to [b] in [accu]. *)
val get : ('a, 'b, 'c, 'd) t -> 'b -> 'd option

(** {7 Counters and histograms} *)

module Counter : sig
  type nonrec 'a t = ('a, 'a, int, int) t

  val create : ?n:int -> unit -> 'a t
  val add : 'a t -> 'a -> int -> unit
  val tick : 'a t -> 'a -> unit
  val stream : 'a t -> ('a * int) CFStream.Stream.t
  val of_stream : 'a CFStream.Stream.t -> 'a t
  val to_alist : 'a t -> ('a * int) list
end

val counts : 'a CFStream.Stream.t -> ('a * int) CFStream.Stream.t

(** [product filter f l1 l2] computes an histogram of values returned by f
    when it is applied for all combinations of elements in [l1] and
    [l2] such that the predicate [filter] is true *)
val product
  :  ?filter:('a -> 'b -> bool)
  -> ('a -> 'b -> 'c)
  -> 'a list
  -> 'b list
  -> ('c * int) CFStream.Stream.t

(** {7 Relation} *)

module Relation : sig
  type nonrec ('a, 'b) t = ('a, 'a, 'b, 'b list) t

  val create : ?n:int -> unit -> ('a, 'b) t
  val add : ('a, 'b) t -> 'a -> 'b -> unit
  val stream : ('a, 'b) t -> ('a * 'b list) CFStream.Stream.t
  val of_stream : ('a * 'b) CFStream.Stream.t -> ('a, 'b) t
  val to_alist : ('a, 'b) t -> ('a * 'b list) list
end

val relation : ('a * 'b) CFStream.Stream.t -> ('a * 'b list) CFStream.Stream.t

module Bins : sig
  type nonrec ('a, 'b) t = ('a, 'b, 'a, 'a list) t

  val of_list : 'a list -> f:('a -> 'b) -> ('a, 'b) t
end
