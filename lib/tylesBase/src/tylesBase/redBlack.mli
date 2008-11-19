(** Red Black Trees *)

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end
  

module type S = sig
  type elt
    (** Type of elements stored in tree. *)
    
  type t
    (** Type of red black trees whose nodes carry values of type [elt]. *)
    
  val member : elt -> t -> bool
    (** [member x t] will return true if [x] is an element of [t]. *)
    
  (** [insert x t] will insert an element into the tree [t]. *)
  val insert : elt -> t -> t
    
  val empty : t

  val within : ('a -> elt -> int) -> 'a -> t -> elt
  exception NotInTree

end
  
module Make (Ord : OrderedType) : S with type elt = Ord.t
