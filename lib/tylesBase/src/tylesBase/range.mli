(** Ranges of contiguous integers (integer intervals). A range is a contiguous sequence of integers from a lower bound to an upper bound. For example, [\[2, 10\]] is the set of integers from 2 through 10, inclusive of 2 and 10. *)

type t = private {lo:int; hi:int}
    (** Type of a range. *)
    
exception Bad of string
  (** Raised when unable to produce a well-formed range. *)

val make : int -> int -> t
  (** [make l u] returns the range from [l] to [u]. Raise [Bad] if [l > u]. *)

val make_opt : int -> int -> t option
  (** Like [make] but returns None instead of raising exception. *)

val size : t -> int
  (** [size v] returns the number of integers in [v], i.e. [v.hi - v.lo + 1]. *)

val equal : t -> t -> bool
  (** True if lower and upper bounds of both ranges are equal. *)

val member : t -> int -> bool
  (** [member t k] returns true if [t] contains [k]. *)
  
val to_string : t -> string
  (** String representation of an range, intended only for human legibility. *)
  
val to_list : t -> int list
  (** [to_list v] returns the set of integers contained in [v], in ascending order. *)

val overlap : t -> t -> int
  (** [overlap u v] returns amount of overlap between two ranges. A positive value indicates number of integers common to [u] and [v]. A negative value indicates the number of integers in between non-overlapping ranges. A zero value means the ranges are exactly adjacent to each other. The relation is symmetric. *)

val gap : t -> t -> int
  (** [gap u v] returns the size of the gap between [u] and [v]. It is equivalent to the negative of [overlap]. *)
  

(** {6 Set Operations} *)
  
val union : t -> t -> t list
  (** [union u v] returns the ranges representing the union of [u] and [v]. Returned list length will be 1 if ranges have non-negative overlap or 2 if they do not. In other words, the answer is represented with the fewest possible ranges. *)
  
val intersect : t -> t -> t option
  (** [intersect u v] returns the range representing the intersection of [u] and [v]. Return None if intersection is empty. *)
  

(** {6 Positional Range.}
    Positional means an range is viewed as coming either before or after another.
*)
  
val before : t -> t -> bool
  (** [before u v] is true if [strict_before u v || equal u v]. *)

val after : t -> t -> bool
  (** [after u v] is equivalent to [before v u]. *)
  
val strict_before : t -> t -> bool
  (** [strict_before u v] is true if [u.lo < v.lo && u.hi < v.hi]. *)
  
val strict_after : t -> t -> bool
  (** [strict_after u v] is equivalent to [strict_before v u]. *)
  
val compare_positional : t -> t -> int option
  (** [compare_positional u v] returns -1 if [u] is strictly before [v], 0 if [u] is equal to [v], +1 if [u] is strictly after [v], and returns None otherwise. *)
  
  
(** {6 Containment Range.}
    Containment means an range is viewed as being inside, or a subset of, another.
*)

val subset : t -> t -> bool
  (** [subset u v] is true if [u] is a subset of [v]. *)
  
val superset : t -> t -> bool
  (** [superset u v] is true if [u] is a superset of [v]. *)
  
val strict_subset : t -> t -> bool
  (** [strict_subset u v] is true if [u] is a strict subset of [v]. *)
  
val strict_superset : t -> t -> bool
  (** [strict_superset u v] is true if [u] is a strict superset of [v]. *)  
  
val compare_containment : t -> t -> int option
  (** [compare_containment u v] returns -1 if [u] is a strict subset of [v], 0 if [u] is equal to [v], +1 if [u] is a strict superset of [v], and returns None otherwise. *)


(** {6 Order Relations}
    A few total orders provided here, but {!Order.compose} allows generating others. It is suitable to pass [compare_positional] as the first or second argument, and [compare_containment] as the second or first, respectively, argument to this function. Also, either can be first reversed by {!Order.reversep}.
*)

val compare_lo : t -> t -> int
  (** Compare by ranges' [lo] bounds, ignoring [hi] bounds. *)
  
val compare_hi : t -> t -> int
  (** Compare by ranges' [hi] bounds, ignoring [lo] bounds. *)


(** {6 Range.Lists} *)
  
val any_overlap : t list -> bool
  (** Return true if any pair of given ranges overlap each other. *)
  
val all_positional : t list -> bool
  (** Return true if all pairs of given ranges are positionally comparable. *)

val max_gap_of_positional : t list -> int
  (** Return maximum gap between adjacent pairs of given ranges. Raise [Failure] if any pairs of given ranges not positionally comparable, or if given less than two ranges. *)
  

(** {6 More Specialized Operations} *)

val find_min_range : ?init_direction:string -> t -> (t -> bool) -> int -> t option
  (** [find_min_range v pred i] finds the minimum sized range within [v] centered around [i] that satisfies [pred]. Successively larger ranges are created starting from \[i, i\] and the first one to satisfy [pred] is returned. None is returned if the given range [v] itself is reached and [pred] still fails. Raise [Failure] if [i] not within [v].
      
      The first range tried is \[i, i\], by default the second is \[i, i+1\], the third \[i-1, i+1\], the fourth \[i-1, i+2\], and so on. The optional [init_direction] must be either "fwd" or "rev". If "fwd", which is the default, the range size is initially increased in the forward direction. If "rev", the second range tried will be \[i-1, i\]. If the range boundary is reached on either side, the size continues to be increased by incrementing on the opposing side. *)

val expand_assoc_list : (t * 'a) list -> 'a list list
  (** [expand_assoc_list dat] returns a list [l] whose [i]'th item is a list of all values associated in [dat] with ranges containing [i]. Raise [Failure] if any range ranges over negative values. *)

val exp_assoc_list : (t * 'a) list -> (int * 'a list) list
  (** [exp_assoc_list dat] returns a list associating each integer [i] with the list of values associated with all ranges overlapping [i] in [dat]. The set of integers considered is the union of all in given [dat]. More general and memory efficient than [expand_assoc_list], which should be deleted once all its clients are updated. *)
  (*
    val collapse_positional : (t * 'a) list -> (t * 'a list) list
  (** [unoverlap_positional f l] takes a list [l] associating ranges with values of type ['a]. The ranges might overlap so a particular integer [i] might be mapped to multiple values. The returned association list is obtained by applying [f] to the list of values that each [i] is associated with in [l], and thus contains only ranges that do not overlap. Raise [Failure] if ranges in [l] are not all positionally comparable. *)
  *)
val find_regions : ?max_gap:int -> ('a -> bool) -> (t * 'a) list -> t list
  (** TO DO: fill in this documentation. For now see {!Math.find_regions}. *)
  

(**/**)

val make_random : t -> t
  (** [make_random t] returns a random range that is a subset of [t]. May raise [Failure] if given range bounds exceed largest value handled by [Random] module. *)
