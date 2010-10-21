module Array = Array2
module List = List2

module IntMonster = Monster.Make(Int)

type t = Leaf | Node of int * IntMonster.body * t * t

type 'a trisected = { 
      left:'a list;
      right:'a list;
      center:'a list;
      median:int
    }

let empty = Leaf

let cmp_to_mid (s,f) mid = 
    if f < mid then -1 
    else if s > mid then 1
    else 0

let cmp_intervals (i1,i2) (i3,i4) = 
  if i2 < i3 then -1
  else if i1 > i4 then 1
  else 0

let median a =
  let n = Array.length a in
  assert (n > 0);
  let a = Array.copy a in
  Array.sort Pervasives.compare a;
  if (n mod 2) <> 0
  then a.((n+1)/2 - 1)
  else let m = (n+1)/2 in (a.(m-1) + a.(m)) / 2

let array_of_endpoints lst = 
  let f acc (s,f) = s::f::acc in
  let lst = List.fold_left f [] lst in
  Array.of_list lst

let trisect_list lst : (int * int) trisected = 
  let mid = median (array_of_endpoints lst) in
  let init = {
      left=[];
      right=[];
      center=[];
      median=mid
    }
  in
  let f acc (s,f) = assert (f >= s);
    match cmp_to_mid (s,f) mid with
    | -1 -> { acc with left = (s,f)::(acc.left) }
    |  0 -> { acc with center = (s,f)::(acc.center) }
    |  1 -> { acc with right = (s,f)::(acc.right) }
    |  _ -> failwith "Error in trisecting list."
  in
  List.fold_left f init lst

let rec create ii_list : t = 
  match ii_list with 
    | [] -> Leaf
    | _ -> let tr = trisect_list ii_list in
      Node(tr.median, (IntMonster.construct tr.center),(create tr.left),(create tr.right))

let within (t:t) (elt:int * int) : (int * int) list =
  let cmp_for_monster (i1,i2) (i3,i4) = if (i1 >= i3) && (i2 <= i4) then true else false in
  let rec aux acc t = 
    match t with
      | Leaf -> acc
      | Node (mid,monster,left,right) -> 
          match cmp_to_mid elt mid with
              | -1 -> aux (List.append (IntMonster.within cmp_for_monster monster elt) acc) left 
              |  0 -> List.append (IntMonster.within cmp_for_monster monster elt) acc
              |  1 -> aux (List.append (IntMonster.within cmp_for_monster monster elt) acc) right
              |  _ -> failwith "Error in function within."
  in
  aux [] t

let within_pt (t:t) (elt:int) = within t (elt,elt)

