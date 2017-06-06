(** Efficient integer sets when many elements expected to be large contiguous sequences of integers. *)
open Core_kernel

type t
    (** Type of a set of integers. *)

type range = Range.t

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
  

  
  
