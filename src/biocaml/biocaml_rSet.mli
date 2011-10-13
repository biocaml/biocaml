(** Efficient integer sets when many elements expected to be large contiguous sequences of integers. *)

type t
    (** Type of a set of integers. *)

type range = Biocaml_range.t

exception Bad of string

val empty : t
  (** The empty set. *)

val of_range_list : (int * int) list -> t
  (** Construct the set of integers representing the union of integers in all given ranges. *)
      
val to_range_list : t -> (int * int) list
    (** Return set of integers as a minimal list of non-overlapping ranges in ascending order by their coordinates. *)

val to_list : t -> int list
  (** Return set of integers as a list. Elements will be in ascending order. *)

val size : t -> int
  (** Number of elements in set. *)

val is_empty : t -> bool
  (** Return true if given set is empty. *)

val inter : t -> t -> t
  (** Set intersection. *)
  
val union : t -> t -> t
  (** Set union. *)

val diff : t -> t -> t
  (** Set difference. [diff s t] is the set of elements that are in [s] but not in [t]. *)

val subset : t -> t -> bool
  (** [subset s t] returns true if [s] is a subset of [t]. *)
  

(** Debugging *)  
module Test : sig
  val test : (int * int) list -> (int * int) list -> unit
    (** [test ul vl] compares performance and correctness of set intersection and union. Sets of type {!IntSet.t} and {!t} are constructed from the given [ul] and [vl], and the corresponding intersection and union operations are used on the two versions. Messages are printed reporting times required to construct the sets, and take their intersection and union. Also, it is verified that the operations produce identical results. *)

  val default_test : unit -> unit
    (** This function generates random lists and uses them as arguments for [test]. The state of the [Random] module
	is not modified. *)
end
  
