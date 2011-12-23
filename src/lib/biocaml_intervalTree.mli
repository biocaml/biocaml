(** Interval tree (data structure)
    
    An interval tree is a collection of integer-bounded intervals labeled by a
    value.

    For a brief description of the implementation, see {{:http://en.wikipedia.org/wiki/Interval_tree#Augmented_tree}

*)

type 'a t

val empty : 'a t
(** the empty tree *)

val is_empty : 'a t -> bool

exception Empty_tree

val add : int -> int -> 'a -> 'a t -> 'a t
(** [add lo hi v t] is an interval tree containing the interval ([lo], [hi]) 
    labeled with value [v] plus the contents of [t] *)

val find_closest : int -> int -> 'a t -> int * int * 'a
(** [find_closest lo hi t] returns the interval in [t] which is at minimal
    distance of the interval ([lo],[hi]). Overlapping intervals are at distance
    0 of each other.

    Raises [Empty_tree] if [t] is empty *)
