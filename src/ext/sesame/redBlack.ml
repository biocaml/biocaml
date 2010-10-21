open Printf
module String = String2

(* Implementation of a balanced Red Black tree -- following Chris Okasaki *)

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type color = Red | Black
  type t = private Leaf | Node of color * elt * t * t
  exception NotInTree
  val empty : t
  val member : elt -> t -> bool
  val insert : elt -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val iter_dfs : (elt -> unit) -> t -> unit
  val fold_dfs : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val map : (elt -> elt) -> t -> t
  val to_list : t -> elt list
  val of_list : elt list -> t
  val to_string : (elt -> string) -> t -> string
  val print : ?cout:out_channel -> (elt -> string) -> t -> unit
end

module Make (Ord : OrderedType) = struct
  type elt = Ord.t
  type color = Red | Black
  type t = Leaf | Node of color * elt * t * t

  let rec member x tree = 
    match tree with
      | Leaf -> false
      | Node (_,y,a,b) ->
          match compare x y with
            | -1 -> member x a
            |  1 -> member x b
            |  0 -> true
            | _ -> assert false

  let insert x s = 
    let balance tree = 
      match tree with
        | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
        | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
        | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
        | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) -> 
            Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
        | a, b, c, d -> 
            Node (a, b, c, d)
    in
    let rec ins tree = 
      match tree with
        | Leaf -> Node (Red, x, Leaf, Leaf)
        | Node (color,y,a,b) ->
            match compare x y with
              | -1 -> balance (color, y, ins a, b)
              |  1 -> balance (color, y, a, ins b)
              |  0 -> tree
              |  _ -> assert false
    in
    match ins s with (* guaranteed to be non-empty *)
      | Node (_, y, a, b) -> Node (Black, y, a, b)
      | Leaf -> assert false
          
  let empty = Leaf
  exception NotInTree

  let rec iter f t = match t with
    | Leaf -> ()
    | Node (_,x,l,r) -> iter f l; f x; iter f r
        
  let fold f init t =
    let accum = ref init in
    let f x = accum := f !accum x in
    iter f t;
    !accum
      
  let rec iter_dfs f t = match t with
    | Leaf -> ()
    | Node (_,x,l,r) -> f x; iter_dfs f l; iter_dfs f r
        
  let fold_dfs f init t =
    let accum = ref init in
    let f x = accum := f !accum x in
    iter_dfs f t;
    !accum

  let map f = fold_dfs (fun tree x -> insert (f x) tree) Leaf 
    
  let to_list t = List.rev (fold (fun l x -> x::l) [] t)
    
  let of_list l =
    let rec loop t l = match l with
      | [] -> t
      | x::xs -> loop (insert x t) xs
    in
    loop Leaf l
      
  let print ?(cout=stdout) elt_to_string t =
    let rec loop depth t =
      let padding = String.make (2*depth) ' ' in
      match t with
        | Leaf -> fprintf cout "%s-\n" padding
        | Node (_, elt, l, r) ->
            fprintf cout "%s%s\n" padding (elt_to_string elt);
            let depth = depth + 1 in
            loop depth l;
            loop depth r
    in
    loop 0 t
      
  let to_string elt_to_string t =
    let rec loop accum depth t =
      let padding = String.make (2*depth) ' ' in
      match t with
        | Leaf -> sprintf "%s%s-\n" accum padding
        | Node (_, elt, l, r) ->
            let accum = sprintf "%s%s%s\n" accum padding (elt_to_string elt) in
            let depth = depth + 1 in
            let accum = loop accum depth l in
            let accum = loop accum depth r in
            accum
    in
    loop "" 0 t
end
