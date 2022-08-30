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

val empty : t
(** The empty set *)

val is_empty : t -> bool
(** Test whether a set is empty, returns [true] if the set is empty. *)

val mem : t -> int -> bool
(** test whether a given int is a member of the set *)

val add : t -> int -> t
(** Add the given int to the set, returning a new set *)

val add_range : t -> int -> int -> t
(** [add_range t lo hi] adds the range of integers [lo, hi] (including both endpoints) to
    the given set, returning a new set
    @raise Invalid_argument if [lo] > [hi] *)

val intersects_range : t -> int -> int -> bool

val singleton : int -> t
(** Return the singleton set containing only the given element *)

val remove : t -> int -> t
(** Remove an element from the given set, returning a new set *)

val remove_range : t -> int -> int -> t
(** [remove_range lo hi t] removes a range of elements from the given set, returning a new set
    @raise Invalid_argument if [lo] > [hi] *)

val union : t -> t -> t
(** Compute the union of two sets.  This is the set whose elements are
    those elements in either input set. *)

val inter : t -> t -> t
(** Compute the intersection of two sets.  This is the set whose
    elements are those in *both* of the input sets. *)

val diff : t -> t -> t
(** Compute the difference between two sets.  This is the set of
    elements that are in the first but not in the second. Unlike
    [union] and [inter], order matters here.*)

val compl : t -> t
(** Create the complement of the given set - i.e. the set of all
    values not in the input set. *)

val compare : t -> t -> int
(** Compare two sets.  It is not safe to use the polymorphic (<) and
    related functions to compare these sets, as the tree representation
    used can balance in multiple ways. *)

val equal : t -> t -> bool
(** Test whether two sets are equal.  It is not safe to use the
    polymorphic (=) on these sets, as the same set can have multiple
    representations depending on how it was built. *)

val subset : t -> t -> bool
(** [subset t u] returns [true] if [t] is a subset of [u] *)

val from : t -> n:int -> t
(** [from ~n t] returns the portion of [t] in the range [n, max_int] *)

val after : t -> n:int -> t
(** [after ~n t] returns the portion of [t] in the range [n+1, max_int] *)

val until : t -> n:int -> t
(** [until ~n t] returns the portion of [t] in the range [min_int, n] *)

val before : t -> n:int -> t
(** [before x t] returns the portion of [t] in the range [min_int, n-1] *)

val iter : t -> f:(int -> unit) -> unit
(** [iter f t] calls [f] once for each element of [t] *)

val iter_range : t -> f:(int -> int -> unit) -> unit
(** [iter_range ~f t] calls [f] once for each contiguous range of [t].
    The contiguous ranges of a set are sequences of adjacent integers
    all part of the set. *)

val fold : t -> init:'a -> f:(int -> 'a -> 'a) -> 'a
(** [fold f t x0] returns the final result of merging each element of
    [t] into [x0] using merge function [f] *)

val fold_range : t -> init:'a -> f:(int -> int -> 'a -> 'a) -> 'a
(** As fold, but operates on contiguous ranges *)

val for_all : t -> f:(int -> bool) -> bool
(** Tests whether a predicate applies to all elements of the set *)

val exists : t -> f:(int -> bool) -> bool
(** Test whether some element of a set satisfies a predicate *)

val filter : t -> f:(int -> bool) -> t
(** Builds the subset of those elements that satisfy the predicate *)

val partition : t -> f:(int -> bool) -> t * t
(** partitions the input set into two sets with elements that satisfy
    the predicate and those that don't *)

val cardinal : t -> int
(** Returns the number of elements in the set *)

val elements : t -> int list
(** Returns a list of all elements in the set *)

val ranges : t -> (int * int) list
(** Returns a list of all contiguous ranges in the set *)

val min_elt : t -> int
(** Returns the minimum element in the set *)

val max_elt : t -> int
(** Returns the maximum element in the set *)

val choose : t -> int
(** Returns some element in the set *)

val to_stream : t -> (int * int) Stream.t
(** Enumerates all contiguous ranges in the set *)

val of_stream : (int * int) Stream.t -> t

val of_list : (int * int) list -> t
(** Build a ISet.t out of a list or enum of ranges *)
