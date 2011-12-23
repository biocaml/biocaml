(*
 * This implementation is largely inspired from the Set implementation of the
 * standard library.
 *)

type 'a t = Empty | Node of 'a node 
and 'a node = {
  left : 'a t ;
  lo : int ;
  hi : int ;
  elt : 'a ;
  right : 'a t ;
  height : int ;
  left_end : int ;
  right_end : int 
}


let interval_compare lo hi lo' hi' =
  let c = compare lo lo' in 
  if c = 0 then compare hi hi'
  else c

let empty = Empty

let is_empty = function Empty -> true | _ -> false

exception Empty_tree

let height = function
  | Empty -> 0 
  | Node n -> n.height

let left_end = function
  | Empty -> max_int 
  | Node n -> n.right_end

let right_end = function
  | Empty -> min_int 
  | Node n -> n.right_end

let create l lo hi elt r =
  Node {
    left = l ; right = r ;
    hi ; lo ; elt;
    height = max (height l) (height r) + 1 ;
    left_end = min lo (min (left_end l) (left_end r)) ;
    right_end = max hi (max (right_end l) (right_end r))
  }

let bal l lo hi elt r = 
  let hl = height l and hr = height r in
  if hl > hr + 2 then (
    match l with 
	Empty -> assert false
      | Node ln -> 
	if height ln.left >= height ln.right then 
	  create ln.left ln.lo ln.hi ln.elt (create ln.right lo hi elt r)
	else 
	  match ln.right with 
	      Empty -> assert false
	    | Node lrn -> 
	      create 
		(create ln.left ln.lo ln.hi ln.elt lrn.left)
		lrn.lo lrn.hi lrn.elt
		(create lrn.right lo hi elt r)
  )
  else if hr > hl + 2 then (
    match r with 
	Empty -> assert false
      | Node rn -> 
	if height rn.right >= height rn.left then 
	  create (create l lo hi elt rn.left) rn.lo rn.hi rn.elt rn.right
	else 
	  match rn.left with
	      Empty -> assert false
	    | Node rln ->
	      create
		(create l lo hi elt rln.left)
		rln.lo rln.hi rln.elt
		(create rln.right rn.lo rn.hi rn.elt rn.right)
  )
  else create l lo hi elt r

let rec add lo hi elt = function
  | Empty -> create Empty lo hi elt Empty
  | Node n as t ->
    let c = interval_compare lo hi n.lo n.hi in 
    if c = 0 then t 
    else 
      if c < 0 then bal (add lo hi elt n.left) n.lo n.hi n.elt n.right
      else bal n.left n.lo n.hi n.elt (add lo hi elt n.right)

    
let test_add () = 
  let r = ref empty in 
  for i = 1 to 1000000 do 
    let j = Random.int 100 in
    r := add j (j + Random.int 100) () !r
  done

let interval_overlap lo hi lo' hi' =
  ( || )
    (hi >= lo' && hi <= hi')
    (lo >= lo' && lo <= hi')
  
let interval_distance lo hi lo' hi' =
  if interval_overlap lo hi lo' hi' then 0
  else min (abs (lo' - hi)) (abs (lo - hi'))
  
let tree_distance lo hi = function
  | Empty -> max_int
  | Node n -> interval_distance lo hi n.left_end n.right_end

let rec find_closest lo hi = function
  | Empty -> raise Empty_tree
  | Node n -> 
    let dc = interval_distance lo hi n.lo n.hi in 
    if dc = 0 then (n.lo,n.hi,n.elt)
    else
      let dl = tree_distance lo hi n.left
      and dr = tree_distance lo hi n.right in
      if dc <= min dl dr then (n.lo,n.hi,n.elt)
      else if dl <= dr then find_closest lo hi n.left
      else find_closest lo hi n.right

(* let rec find_kclosest k lo hi t =  *)
