(** Efficient integer sets when many elements expected to be large contiguous sequences of integers. *)

type t
    (** Type of a set of integers. *)

exception Bad of string

val of_range_list : (int * int) list -> t
  (** Construct the set of integers representing the union of integers in all given intervals. *)

val to_range_list : t -> (int * int) list
  (** Return set of integers as a minimal list of non-overlapping intervals, in ascending order by their coordinates. Number of intervals in answer is the minimum needed to represent given set. *)
  
val to_list : t -> int list
  (** Return set of integers as a list. Elements will be in ascending order. *)

val size : t -> int
  (** Number of elements in set. *)

val inter : t -> t -> t
  (** Set intersection. *)
  
val union : t -> t -> t
  (** Set union. *)

val diff : t -> t -> t
  (** Set difference. [diff s t] is the set of elements that are in [s] but not in [t]. *)
  

(** Debugging *)  
module Test : sig
  val test : (int * int) list -> (int * int) list -> unit
    (** [test ul vl] compares performance and correctness of set intersection and union. Sets of type {!IntSet.t} and {!t} are constructed from the given [ul] and [vl], and the corresponding intersection and union operations are used on the two versions. Messages are printed reporting times required to construct the sets, and take their intersection and union. Also, it is verified that the operations produce identical results. *)
    
end
  
