(** Red Black trees. *)

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig

  (** Type of elements stored in tree. *)
  type elt
      
  (** Nodes can be either red or black. *)
  type color = Red | Black
      
  (** Type of red black trees. Nodes carry an [elt] as their value,
      are colored, and point to their left and right children. *)
  type t = private Leaf | Node of color * elt * t * t

  exception NotInTree
    (** Raised when an item is not found in the tree. *)
    
  val empty : t
    (** The empty tree. *)

  val member : elt -> t -> bool
    (** [member x t] will return true if [x] is an element of [t]. *)

  val insert : elt -> t -> t
    (** [insert x t] will insert [x] into [t]. *)

  val iter : (elt -> unit) -> t -> unit
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
    (** [iter] and [fold] traverse the tree in order. *)
    
  val iter_dfs : (elt -> unit) -> t -> unit
  val fold_dfs : ('a -> elt -> 'a) -> 'a -> t -> 'a
    (** [iter_dfs] and [fold_dfs] traverse the tree in depth-first-order. *)
    
  val map : (elt -> elt) -> t -> t
    (** [map f t] returns the new tree obtained by applying [f] to
        every item of [t]. *)
    
  val to_list : t -> elt list
    (** [to_list t] returns the elements of [t] in a list. Items are
        returned in order. *)
    
  val of_list : elt list -> t
    (** [of_list l] constructs a tree from all items in [l]. *)

  val to_string : (elt -> string) -> t -> string
    (** [to_string elt_to_string t] converts [t] to a human legible
        string. *)
    
  val print : ?cout:out_channel -> (elt -> string) -> t -> unit
    (** [print ~cout elt_to_string t] prints [t] to [cout], which is [stdout] by
        default. *)
end

module Make (Ord: OrderedType) : S with type elt = Ord.t

