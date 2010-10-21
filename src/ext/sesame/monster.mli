
module type OrderedType = sig
  type t 
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type appendage
  type body
  val empty : body
  val construct : elt list -> body
  val within : ('a -> elt -> bool) -> body -> 'a -> elt list
end

module Make (Ord : OrderedType) : S with type elt = Ord.t * Ord.t
  
