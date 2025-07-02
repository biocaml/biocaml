(* $Id: iSet.mli,v 1.1 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* Modified by Philippe Veber *)
(** DIET : Discrete Interval Encoding Trees

    Sets of integers represented as ranges

    This data structure is efficient for large sets of integers where
    many adjacent integers are all part of the set.  This will have
    higher overhead for sets with lots of point elements, but will be
    much more efficient for sets containing mostly ranges.
*)

type t

(** The empty set *)
val empty : t

(** Test whether a set is empty, returns [true] if the set is empty. *)
val is_empty : t -> bool

(** test whether a given int is a member of the set *)
val mem : t -> int -> bool

(** Add the given int to the set, returning a new set *)
val add : t -> int -> t

(** [add_range t lo hi] adds the range of integers [lo, hi] (including both endpoints) to
    the given set, returning a new set
    @raise Invalid_argument if [lo] > [hi] *)
val add_range : t -> int -> int -> t

val intersects_range : t -> int -> int -> bool

(** Return the singleton set containing only the given element *)
val singleton : int -> t

(** Remove an element from the given set, returning a new set *)
val remove : t -> int -> t

(** [remove_range lo hi t] removes a range of elements from the given set, returning a new set
    @raise Invalid_argument if [lo] > [hi] *)
val remove_range : t -> int -> int -> t

(** Compute the union of two sets.  This is the set whose elements are
    those elements in either input set. *)
val union : t -> t -> t

(** Compute the intersection of two sets.  This is the set whose
    elements are those in *both* of the input sets. *)
val inter : t -> t -> t

(** Compute the difference between two sets.  This is the set of
    elements that are in the first but not in the second. Unlike
    [union] and [inter], order matters here.*)
val diff : t -> t -> t

(** Create the complement of the given set - i.e. the set of all
    values not in the input set. *)
val compl : t -> t

(** Compare two sets.  It is not safe to use the polymorphic (<) and
    related functions to compare these sets, as the tree representation
    used can balance in multiple ways. *)
val compare : t -> t -> int

(** Test whether two sets are equal.  It is not safe to use the
    polymorphic (=) on these sets, as the same set can have multiple
    representations depending on how it was built. *)
val equal : t -> t -> bool

(** [subset t u] returns [true] if [t] is a subset of [u] *)
val subset : t -> t -> bool

(** [from ~n t] returns the portion of [t] in the range [n, max_int] *)
val from : t -> n:int -> t

(** [after ~n t] returns the portion of [t] in the range [n+1, max_int] *)
val after : t -> n:int -> t

(** [until ~n t] returns the portion of [t] in the range [min_int, n] *)
val until : t -> n:int -> t

(** [before x t] returns the portion of [t] in the range [min_int, n-1] *)
val before : t -> n:int -> t

(** [iter f t] calls [f] once for each element of [t] *)
val iter : t -> f:(int -> unit) -> unit

(** [iter_range ~f t] calls [f] once for each contiguous range of [t].
    The contiguous ranges of a set are sequences of adjacent integers
    all part of the set. *)
val iter_range : t -> f:(int -> int -> unit) -> unit

(** [fold f t x0] returns the final result of merging each element of
    [t] into [x0] using merge function [f] *)
val fold : t -> init:'a -> f:(int -> 'a -> 'a) -> 'a

(** As fold, but operates on contiguous ranges *)
val fold_range : t -> init:'a -> f:(int -> int -> 'a -> 'a) -> 'a

(** Tests whether a predicate applies to all elements of the set *)
val for_all : t -> f:(int -> bool) -> bool

(** Test whether some element of a set satisfies a predicate *)
val exists : t -> f:(int -> bool) -> bool

(** Builds the subset of those elements that satisfy the predicate *)
val filter : t -> f:(int -> bool) -> t

(** partitions the input set into two sets with elements that satisfy
    the predicate and those that don't *)
val partition : t -> f:(int -> bool) -> t * t

(** Returns the number of elements in the set *)
val cardinal : t -> int

(** Returns a list of all elements in the set *)
val elements : t -> int list

(** Returns a list of all contiguous ranges in the set *)
val ranges : t -> (int * int) list

(** Returns the minimum element in the set *)
val min_elt : t -> int

(** Returns the maximum element in the set *)
val max_elt : t -> int

(** Returns some element in the set *)
val choose : t -> int

(** Enumerates all contiguous ranges in the set *)
val to_stream : t -> (int * int) Stream.t

val of_stream : (int * int) Stream.t -> t

(** Build a ISet.t out of a list or enum of ranges *)
val of_list : (int * int) list -> t
