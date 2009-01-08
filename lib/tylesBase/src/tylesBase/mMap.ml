
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
  val filter : (Fst.key -> Snd.key -> 'a -> bool) -> 'a t -> 'a t
  val add_with : (Fst.key -> Snd.key -> 'a option -> 'b -> 'a) -> Fst.key -> Snd.key -> 'b -> 'a t -> 'a t
  val empty : 'a t
  val map2i : (Fst.key -> Snd.key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val remove : Fst.key -> Snd.key -> 'a t -> 'a t
  val intersect : 'a t -> 'b t -> ('a * 'b) t
  val subtract : 'a t -> 'a t -> 'a t
  val split : (Fst.key -> Snd.key -> 'a -> bool) -> 'a t -> 'a t * 'a t
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
  let iter f t = Fst.iter (fun k1 m2 -> Snd.iter (f k1) m2) t
 
  let fold f a t = 
    let g k1 k2 v a = f k1 k2 a v in
      Fst.fold (fun k1 m2 a -> Snd.fold (g k1) m2 a) t a
 
  let size t = fold (fun _ _ ans _ -> ans + 1) 0 t
  let keys t = List.rev (fold (fun k1 k2 ans _ -> (k1,k2)::ans) [] t)

  let find k1 k2 t = Snd.find k2 (Fst.find k1 t)

  let foldinner k1 f a t = 
    let m2 = Fst.find k1 t in
    let f k2 elem acc = f k2 acc elem in
    Snd.fold f m2 a

  let mem k1 k2 t = 
    try 
      let m1 = Fst.find k1 t in
      Snd.mem k2 m1
    with Not_found -> false

  let empty = Fst.empty

  let add k1 k2 elem t = 
    let m2 = 
      try Fst.find k1 t 
      with Not_found -> Snd.empty 
    in
    let m2 = Snd.add k2 elem m2 in
    Fst.add k1 m2 t

  let filter pred t = 
    let f k1 k2 acc v = if pred k1 k2 v then add k1 k2 v acc else acc in
    fold f empty t

  let add_with f k1 k2 elem t =
    let y' = try Some (find k1 k2 t) with Not_found -> None in
    add k1 k2 (f k1 k2 y' elem) t

  let map2i f m n = 
    if size m <> size n then failwith "domains not equal in size";
    let mn = List.zip (to_list m) (to_list n) in
    let f ans ((k1,k2,a),(k3,k4,b)) = 
      if ((Ord1.compare k1 k3 = 0) && (Ord2.compare k2 k4 = 0))
      then add k1 k2 (f k1 k2 a b) ans
      else failwith "domains contain different keys"
    in
    List.fold_left f empty mn

  let map2 f m n = map2i (fun _ _ a b -> f a b) m n

  let remove k1 k2 t = 
    try 
      let m2 = Fst.find k1 t in
      let m2 = Snd.remove k2 m2 in 
      Fst.add k1 m2 t
    with Not_found -> t

  let intersect m1 m2 = 
    let f m2 k1 k2 acc elem = 
      if mem k1 k2 m2 then acc else remove k1 k2 acc
    in
    let m1 = fold (f m2) m1 m1 in
    let m2 = fold (f m1) m2 m2 in
    map2 (fun a b -> a,b) m1 m2

  let subtract m1 m2 = 
    let f k1 k2 acc elem = 
      try let _ = find k1 k2 m2 in acc
      with Not_found -> add k1 k2 elem acc
    in fold f empty m1

  let split pred t = 
    let trues = filter pred t in
    let falses = 
      filter (fun k1 k2 elem -> if pred k1 k2 elem then false else true) t in
    (trues,falses)
end
