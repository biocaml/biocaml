open Biocaml_internal_pervasives

type range = Biocaml_range.t
type 'a location = 'a * range

(** Indicator function on a genome *)
module Selection : sig
  type 'a t

  val inter : 'a t -> 'a t -> 'a t
  val union : 'a t -> 'a t -> 'a t
  val diff : 'a t -> 'a t -> 'a t
  val size : 'a t -> int

  val intersects : 'a location -> 'a t -> bool
    (** [intersects loc sel] returns [true] if [loc] has a non-empty
        intersection with [sel], and [false] otherwise. *)

  val intersection_size : 'a location -> 'a t -> int

  val to_stream : 'a t -> 'a location Stream.t

  val of_stream : 'a location Stream.t -> 'a t
    (** [of_stream e] computes a selection as the union of the locations contained in [e] *)

end

(** Partial function over the genome: each base may be associated to a value. *)
module type Signal = sig
  type ('a,'b) t
  val make : ('b list -> 'c) -> ('a location * 'c) Stream.t -> ('a,'c) t

  val eval : 'a -> int -> ('a,'b) t -> 'b
  (** function evaluation at some point in the genome *)

  val fold : ('a -> range -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c
  (** folds on constant intervals of the function, in increasing order *)

  val to_stream : ('a,'b) t -> ('a location * 'b) Stream.t
  (** enumeration over all constant intervals of the function, in increasing order *)
end

(** A set of locations *)
module LSet : sig
  type 'a t

  val to_stream : 'a t -> 'a location Stream.t
  val of_stream : 'a location Stream.t -> 'a t


  (* val fold : ('a -> range -> 'b -> 'b) -> 'a t -> 'b -> 'b *)
  (*   (\** fold guaranteed on increasing order keywise, and for each key *\) *)

  val intersects : 'a location -> 'a t -> bool
  (** [intersects loc lmap] returns [true] if [loc] has a non-empty
      intersection with one of the locations in [lmap], and returns
      [false] otherwise *)

  val closest : 'a location -> 'a t -> ('a location * int) option
  (** [closest loc lset] returns the location in [lset] that is the
      closest to [loc], along with the actual (minimal)
      distance. Returns [None] if there is no location in [lset]
      that comes from the same sequence than [loc]. *)
    
  val intersecting_elems : 'a location -> 'a t -> 'a location Stream.t
  (** [intersecting_elems loc lset] returns an enumeration of all
      locations in [lset] that intersect [loc]. *)

end

(** A set of locations with an attached value on each of them *)
module LMap : sig
  type ('a,'b) t

  val to_stream : ('a, 'b) t -> ('a location * 'b) Stream.t
  val of_stream : ('a location * 'b) Stream.t -> ('a, 'b) t

  val intersects : 'a location -> ('a,'b) t -> bool
    (** [intersects loc lmap] returns [true] if [loc] has a non-empty
        intersection with one of the locations in [lmap], and returns
        [false] otherwise *)

  val closest : 'a location -> ('a,'b) t -> ('a location * 'b * int) option
    (** [closest loc lmap] returns the location in [lmap] that is the 
        closest to [loc], along with its annotation and the actual (minimal) 
        distance. Returns [None] if there is no location in [lmap] 
        that comes from the same sequence than [loc]. *)

  val intersecting_elems : 'a location -> ('a, 'b) t -> ('a location * 'b) Stream.t
  (** [intersecting_elems loc lmap] returns an enumeration of elements
      in [lmap] whose location intersects with [loc]. *)

end


(** A set of locations with an attached value on each of them *)
module type LMap_spec = sig
  type ('a,'b) t

  val make : ('a location * 'b) Stream.t -> ('a,'b) t


  val fold : ('a -> range -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c
    (** fold guaranteed on increasing order keywise, and for each key *)

  val pwfold : ('a -> range -> 'b list -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
    (** a constant interval of an LMap [lm] is a range intersecting [lm] and
that do not contain any start or end of a range from [lm]. A maximal
constant interval is a constant interval, maximum for inclusion.
[pwfold f lm init] folds on maximal constant intervals of [lm],
in increasing order *)

  val intersects : 'a location -> ('a,'b) t -> bool

  val to_stream : ('a,'b) t -> ('a location * 'b) Stream.t

  val union : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val add : 'a location -> 'b -> ('a,'b) t -> ('a,'b) t
end

