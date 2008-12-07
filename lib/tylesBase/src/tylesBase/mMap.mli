(** Map of maps. *)

module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  module Fst : Map2.S (** First (outer) map. *)
  module Snd : Map2.S (** Second (inner) map. *)

  type 'a t = 'a Snd.t Fst.t (** Type of map of maps. *)

  val of_list : (Fst.key * Snd.key * 'a) list -> 'a t
    (** Construct a map from a list associating [Fst.key]'s and [Snd.key]'s with a value. If duplicate combinations of keys given only one will be inserted into map, but it is unspecified which one. *)
    
  val to_list : 'a t -> (Fst.key * Snd.key * 'a) list
    (** Return the map as a flat list. Items will be in ascending order first by [Fst.key]'s, then by [Snd.key]'s. *)
    
  val of_lists : (Fst.key * (Snd.key * 'a) list) list -> 'a t
    (** Construct a map from a list associating [Fst.key]'s to a list associating [Snd.key]'s to a value. If duplicate combinations of keys given only one will be inserted into map, but it is unspecified which one. *)

  val to_lists : 'a t -> (Fst.key * (Snd.key * 'a) list) list
    (** Return the map as a nested association list. Items in each list will be in ascending order by their respective keys. *)
    
  val keys : 'a t -> (Fst.key * Snd.key) list
  val mapi : (Fst.key -> Snd.key -> 'a -> 'b) -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val iter : (Fst.key -> Snd.key -> 'a -> unit) -> 'a t -> unit
  val fold : (Fst.key -> Snd.key -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val size : 'a t -> int
  val find : Fst.key -> Snd.key -> 'a t -> 'a
  val foldinner : Fst.key -> (Snd.key -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val mem : Fst.key -> Snd.key -> 'a t -> bool
  val add : Fst.key -> Snd.key -> 'a -> 'a t -> 'a t
  val add_with : (Fst.key -> Snd.key -> 'a option -> 'b -> 'a) -> Fst.key -> Snd.key -> 'b -> 'a t -> 'a t
  val empty : 'a t
  val map2i : (Fst.key -> Snd.key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val remove : Fst.key -> Snd.key -> 'a t -> 'a t
  val intersect : 'a t -> 'b t -> ('a * 'b) t
end
  
module Make (Ord1 : ORDERED) (Ord2 : ORDERED) : S with type Fst.key = Ord1.t and type Snd.key = Ord2.t
