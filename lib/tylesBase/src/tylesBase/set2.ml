module List = List2

module type OrderedType = Set.OrderedType

module type S = sig
  include Set.S
  val of_list : elt list -> t
  val to_list : t -> elt list
  val unions : t list -> t
end

module Make (Ord:OrderedType) = struct
  include (Set.Make(Ord) : Set.S with type elt = Ord.t)

  let of_list el = List.fold_left (fun ans e -> add e ans) empty el
  let to_list t = List.rev (fold (fun e ans -> e::ans) t [])
  let unions xs = List.fold_left union empty xs
end
