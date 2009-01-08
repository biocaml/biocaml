module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t
  val member : elt -> t -> bool
  val insert : elt -> t -> t
  val empty : t
  val within : ('a -> elt -> int) -> 'a -> t -> elt
  exception NotInTree
end

module Make (Ord : OrderedType) = struct
  type elt = Ord.t

  (* Implemented from "Functional Data Structures" by Chris Okasaki
   * and "Introduction to Objective Caml" by Jason Hickey *)

  type color = R | B

  (* Node of color * value * left tree * right tree *)
  type t = Leaf | Node of color * elt * t * t

  (* Invariant 1: No red node has a red child.
   * Invariant 2: Every path from the root to an empty node contains the same number 
   * of black nodes. *)

  let rec member x t = 
    match t with
      | Leaf -> false
      | Node (_,y,a,b) ->
          match compare x y with
            | -1 -> member x a
            |  1 -> member x b
            |  0 -> true
            | _ -> assert false

  let balance t = match t with
    | B, z, Node (R, y, Node (R, x, a, b), c), d
    | B, z, Node (R, x, a, Node (R, y, b, c)), d
    | B, x, a, Node (R, z, Node (R, y, b, c), d)
    | B, x, a, Node (R, y, b, Node (R, z, c, d)) -> 
        Node (R, y, Node (B, x, a, b), Node (B, z, c, d))
    | a, b, c, d -> Node (a, b, c, d)

  let empty = Leaf

  exception NotInTree
(*
  let rec remove x t = 
    match t with
      | Leaf -> Leaf (* not found *)
      | Node (color,x',l,r) ->
          let cmp = compare x x' in
          if cmp > 0 then Node(color,x',l,remove x r) else
            if cmp < 0 then Node(color,x',remove x l,r) else
              match (l,r) with
                | (_, Leaf) -> l
                | (Leaf, _) -> r
                | _ -> let (nx, nr) = remove_first r in
                  Node {value=nx; left=l; r=nr}
  and
      (* remove_first n  is (x, n') where x is the lowest value in
         n, and n' is n with x removed. Requires: n is not Leaf *)
      remove_first n =
    match n with
        Leaf -> raise (Failure "remove_first precondition")
      | Node {value=x; left=Leaf; right=r} -> (x,r)
      | Node {value=x; left=l; right=r} ->
          let (nx,nl) = remove_first l in
          (nx, Node{value=nx, left=nl, right=r}) 
*)

  let rec within (cmp:'a -> elt -> int) (elem:'a) (tree:t) = 
    match tree with 
      | Leaf -> raise NotInTree
      | Node (_,value,a,b) -> 
          match cmp elem value with
            | -1 -> within cmp elem a
            |  1 -> within cmp elem b
            |  0 -> value
            |  _ -> failwith "Wrong cmp function given to Redblack.within."

  let insert x s = 
    let rec ins t = 
      match t with
        | Leaf -> Node (R, x, Leaf, Leaf)
        | Node (color,y,a,b) as s ->
            match compare x y with
              | -1 -> balance (color, y, ins a, b)
              |  1 -> balance (color, y, a, ins b)
              |  0 -> s
              |  _ -> assert false
    in
    match ins s with (* guaranteed to be non-empty *)
        Node (_, y, a, b) -> Node (B, y, a, b)
      | Leaf -> raise (Invalid_argument "insert")
end
