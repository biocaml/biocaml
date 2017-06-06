(*
 * This implementation is largely inspired from the Set implementation of the
 * standard library and the BatSet module (from Batteries) for enum-related
 * functions
 *)
open Core_kernel
open CFStream

type 'a t = Empty | Node of 'a node
and 'a node = {
  left : 'a t ; (* left child *)
  lo : int ; (* left-end of the interval at this node *)
  hi : int ; (* right-end of the interval at this node *)
  elt : 'a ; (* element at this node *)
  right : 'a t ; (* right child *)
  height : int ; (* height of the tree (for balancing purpose) *)
  left_end : int ; (* left-end of the left-most interval in the tree *)
  right_end : int ; (* right-end of the right-most interval in the tree *)
}

exception Empty_tree

let interval_compare lo hi lo' hi' =
  let c = compare lo lo' in
  if c = 0 then compare hi hi'
  else c

let is_empty = function Empty -> true | _ -> false

let height = function
  | Empty -> 0
  | Node n -> n.height

let left_end = function
  | Empty -> Int.max_value
  | Node n -> n.left_end

let right_end = function
  | Empty -> Int.min_value
  | Node n -> n.right_end

let rec cardinal = function
  | Empty -> 0
  | Node n -> cardinal n.left + 1 + cardinal n.right

let node_contents n = n.lo, n.hi, n.elt

let interval_overlap lo hi lo' hi' =
  ( || )
    (lo  <= lo' && lo' <= hi)
    (lo' <= lo  && lo  <= hi')

let rec intersects t ~low ~high =
  match t with
  | Empty -> false
  | Node n ->
      if interval_overlap low high n.lo n.hi then true
      else if interval_overlap low high n.left_end n.right_end then
        intersects n.left ~low ~high || intersects n.right ~low ~high
      else false

let interval_distance lo hi lo' hi' =
  if interval_overlap lo hi lo' hi' then 0
  else min (abs (lo' - hi)) (abs (lo - hi'))

let tree_distance lo hi = function
  | Empty -> Int.max_value
  | Node n -> interval_distance lo hi n.left_end n.right_end


let empty = Empty


let create l lo hi elt r =
  let hl = match l with Empty -> 0 | Node n -> n.height
  and hr = match r with Empty -> 0 | Node n -> n.height in
  Node {
    left = l ; right = r ;
    hi ; lo ; elt;
    height = (if hl >= hr then hl + 1 else hr + 1) ;
    left_end = (let le = left_end l in if lo < le then lo else le) ;
    right_end = (
      let lre = right_end l
      and rre = right_end r in
      if hi > lre then
	if hi > rre then hi else rre
      else
	if lre > rre then lre else rre)
  }

let bal l lo hi elt r =
  let hl = match l with Empty -> 0 | Node n -> n.height
  and hr = match r with Empty -> 0 | Node n -> n.height in
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
  else Node { left = l ; lo ; hi ; elt ; right = r ;
	      left_end = (let le = left_end l in if lo < le then lo else le) ;
	      right_end = (
		let lre = right_end l
		and rre = right_end r in
		if hi > lre then
		  if hi > rre then hi else rre
		else if lre > rre then lre else rre) ;
	      height = (if hl >= hr then hl + 1 else hr + 1) }

let add t ~low ~high ~data =
  let rec aux lo hi elt = function
    | Empty -> create Empty lo hi elt Empty
    | Node n ->
      let c = interval_compare lo hi n.lo n.hi in
      if c <= 0 then bal (aux lo hi elt n.left) n.lo n.hi n.elt n.right
      else bal n.left n.lo n.hi n.elt (aux lo hi elt n.right)
  in
  aux low high data t



let rec elements_aux accu = function
  | Empty -> accu
  | Node n -> elements_aux ((node_contents n) :: elements_aux accu n.right) n.left

let elements s =
  elements_aux [] s


type 'a iter = E | C of 'a node * 'a t * 'a iter

let rec cons_iter s t = match s with
    Empty -> t
  | Node n -> cons_iter n.left (C (n, n.right, t))

let rec rev_cons_iter s t = match s with
    Empty -> t
  | Node n -> rev_cons_iter n.right (C (n, n.left, t))

let stream_next l _ = match !l with
    E -> None
  | C (n, s, t) -> l := cons_iter s t; Some (node_contents n)

let stream_backwards_next l _ = match !l with
    E -> None
  | C (n, s, t) -> l := rev_cons_iter s t; Some (node_contents n)

let to_stream t =
  let l = ref (cons_iter t E) in
  Stream.from (stream_next l)

let to_backwards_stream t =
  let l = ref (rev_cons_iter t E) in
  Stream.from (stream_backwards_next l)






let rec find_closest_aux lo hi = function
  | Empty -> None
  | Node n ->
    let dc = interval_distance lo hi n.lo n.hi in
    if dc = 0 then Some (n,0)
    else
      let dl_lb = tree_distance lo hi n.left
      and dr_lb = tree_distance lo hi n.right in
      let optval, optnode =
	if dl_lb < dc then
	  match find_closest_aux lo hi n.left with
	      Some (nl,dl) when dl < dc -> dl, nl
	    | _ -> dc, n
	else dc, n in
      let optval, optnode =
	if dr_lb < optval then
	  match find_closest_aux lo hi n.right with
	      Some (nr,dr) when dr < optval -> dr, nr
	    | _ -> optval, optnode
	else optval, optnode
      in
      Some (optnode, optval)

let find_closest lo hi t = match find_closest_aux lo hi t with
    Some (n,d) ->
      let lo', hi', v = node_contents n in lo', hi', v, d
  | None -> raise Empty_tree

















let rec check_height_integrity = function
  | Empty -> 0
  | Node n ->
    let h = max (check_height_integrity n.left) (check_height_integrity n.right) + 1 in
    assert (h = n.height) ;
    assert (abs (height n.left - height n.right) <= 2) ;
    h

let check_integrity t =
  ignore (check_height_integrity t)

let rec print_aux margin = function
  | Empty -> ()
  | Node n ->
    let space = String.make margin ' ' in
    printf "%s(%d,%d)_(%d,%d)\n" space n.lo n.hi n.left_end n.right_end;
    printf "%sleft\n" space ;
    print_aux (margin + 2) n.left ;
    printf "%sright\n" space ;
    print_aux (margin + 2) n.right

let print t = print_aux 0 t




let find_intersecting_elem lo hi t =
  let rec loop = function
  | [] -> None
  | h :: t -> match h with
    | Empty -> loop t
    | Node n ->
        if interval_overlap lo hi n.left_end n.right_end then (
          let t = n.left :: n.right :: t in
          if interval_overlap lo hi n.lo n.hi
          then Some (node_contents n, t)
          else loop t
        )
        else loop t
  in
  Stream.unfold [t] ~f:loop



let filter_overlapping t ~low ~high =
  let res = ref empty in
  let rec loop = function
    | [] -> ()
    | h :: t ->
      begin match h with
      | Empty -> loop t
      | Node n ->
        if interval_overlap low high n.left_end n.right_end then (
          let t = n.left :: n.right :: t in
          if interval_overlap low high n.lo n.hi
          then res := add !res ~low:n.lo ~high:n.hi ~data:n.elt;
          loop t
        )
        else loop t
      end
  in
  loop [t];
  !res
