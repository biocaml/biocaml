module List = List2
module Map = Map2

module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  module Fst : Map.S
  module Snd : Map.S

  type 'a t = 'a Snd.t Fst.t

  val of_list : (Fst.key * Snd.key * 'a) list -> 'a t
  val to_list : 'a t -> (Fst.key * Snd.key * 'a) list
    
  val of_lists : (Fst.key * (Snd.key * 'a) list) list -> 'a t
  val to_lists : 'a t -> (Fst.key * (Snd.key * 'a) list) list

  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (Fst.key -> Snd.key -> 'a -> 'b) -> 'a t -> 'b t
end

module Make (Ord1 : ORDERED) (Ord2 : ORDERED) = struct
  module Fst = Map.Make(struct type t = Ord1.t let compare = Ord1.compare end)
  module Snd = Map.Make(struct type t = Ord2.t let compare = Ord2.compare end)

  type 'a t = 'a Snd.t Fst.t

  let of_lists l =
    List.fold_left (fun ans (a,bcl) -> Fst.add a (Snd.of_list bcl) ans) Fst.empty l

  let to_lists t =
    List.map (fun (a,bmap) -> a, Snd.to_list bmap) (Fst.to_list t)

  let of_list l =
    let ll = List.npartition (fun (a1,_,_) (a2,_,_) -> Ord1.compare a1 a2 = 0) l in
    let f l =
      Tuple.Tr.prj1 (List.hd l),
      List.map (fun (_,b,c) -> b,c) l
    in
    let ll = List.map f ll in
      of_lists ll

  let to_list t =
    let f (a,bcl) = List.map (fun (b,c) -> a,b,c) bcl in 
      List.flatten (List.map f (to_lists t))
        
  let mapi f t = Fst.mapi (fun a bmap -> Snd.mapi (fun b c -> f a b c) bmap) t
  let map f t = mapi (fun _ _ c -> f c) t 
end
