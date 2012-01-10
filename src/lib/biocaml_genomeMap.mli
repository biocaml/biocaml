open Batteries
type range = Biocaml_range.t
type 'a location = 'a * range

(** Indicator function on a genome *)
module Selection : sig
  type 'a t

  val inter : 'a t -> 'a t -> 'a t
  val diff : 'a t -> 'a t -> 'a t
  val size : 'a t -> int

  val intersects : 'a location -> 'a t -> bool
  val intersection_size : 'a location -> 'a t -> int

  val enum : 'a t -> 'a location Enum.t

  val of_enum : 'a location Enum.t -> 'a t
    (** [of_enum e] computes a selection as the union of the locations contained in [e] *)

end

(** Partial function over the genome: each base may be associated to a value. *)
module type Signal = sig
  type ('a,'b) t
  val make : ('b list -> 'c) -> ('a location * 'c) Enum.t -> ('a,'c) t

  val eval : 'a -> int -> ('a,'b) t -> 'b
  (** function evaluation at some point in the genome *)

  val fold : ('a -> range -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c
  (** folds on constant intervals of the function, in increasing order *)

  val enum : ('a,'b) t -> ('a location * 'b) Enum.t
  (** enumeration over all constant intervals of the function, in increasing order *)
end

(** A set of locations *)
module type LSet = sig
  type 'a t

  val make : 'a location Enum.t -> 'a t


  val fold : ('a -> range -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** fold guaranteed on increasing order keywise, and for each key *)

  val intersects : 'a location -> 'a t -> bool

  val enum : 'a t -> 'a location Enum.t


  val union : 'a t -> 'a t -> 'a t
  val add : 'a location -> 'a t -> 'a t
end

(** A set of locations with an attached value on each of them *)
module LMap : sig
  type ('a,'b) t

  val enum : ('a, 'b) t -> ('a location * 'b) Enum.t
  val of_enum : ('a location * 'b) Enum.t -> ('a, 'b) t
end


(** A set of locations with an attached value on each of them *)
module type LMap_spec = sig
  type ('a,'b) t

  val make : ('a location * 'b) Enum.t -> ('a,'b) t


  val fold : ('a -> range -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c
    (** fold guaranteed on increasing order keywise, and for each key *)

  val pwfold : ('a -> range -> 'b list -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
    (** a constant interval of an LMap [lm] is a range intersecting [lm] and
that do not contain any start or end of a range from [lm]. A maximal
constant interval is a constant interval, maximum for inclusion.
[pwfold f lm init] folds on maximal constant intervals of [lm],
in increasing order *)

  val intersects : 'a location -> ('a,'b) t -> bool

  val enum : ('a,'b) t -> ('a location * 'b) Enum.t

  val union : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val add : 'a location -> 'b -> ('a,'b) t -> ('a,'b) t
end

