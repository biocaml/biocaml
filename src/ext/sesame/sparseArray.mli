(** Sparse arrays. Semantically equivalent to normal arrays but implementation uses less memory when many elements expected to have a single identical value, called the default value. Only functions unique to this module are documented here; see Standard Library's [Array] module for others. *)

type 'a t
    
val make : ?eq:('a -> 'a -> bool) -> ?sparsity:float -> int -> 'a -> 'a t
  (** [make eq sparsity n x] returns a fresh array of length [n] with all elements initialized to [x]. The equality function [eq] (defaults to (=)) is needed internally to determine if an element equals the default value. The [sparsity] (default = 0.9) is a guess of the fraction of elements that will have the default value. Raise [Failure] if [sparsity] not between 0.0 and 1.0, or [n] less than 0. *)
  
val length : 'a t -> int
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val iter : ('a -> unit) -> 'a t -> unit
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_right : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
