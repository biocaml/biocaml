(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* Modified by Edgar Friendly <thelema314@gmail.com> *)
(* Modified by Philippe Veber <philippe.veber@gmail.com> *)

(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* Modified by Edgar Friendly <thelema314@gmail.com> *)

module Int = Core_kernel.Int
open CFStream

module BatAvlTree = struct
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree * int (* height *)

let empty = Empty

let is_empty = function
  | Empty -> true
  | Node _ -> false

let singleton_tree x =
  Node (Empty, x, Empty, 1)

let left_branch = function
  | Empty -> raise Not_found
  | Node (l, _, _, _) -> l

let right_branch = function
  | Empty -> raise Not_found
  | Node (_, _, r, _) -> r

let root = function
  | Empty -> raise Not_found
  | Node (_, v, _, _) -> v

let height = function
  | Empty -> 0
  | Node (_, _, _, h) -> h


let create l v r =
  let h' = 1 + Int.max (height l) (height r) in
  assert (abs (height l - height r ) < 2);
  Node (l, v, r, h')

(* Assume |hl - hr| < 3 *)
let bal l v r =
  let hl = height l in
  let hr = height r in
  if hl >= hr + 2 then
    match l with
    | Empty -> assert false
    | Node (ll, lv, lr, _) ->
      if height ll >= height lr then
        create ll lv (create lr v r)
      else
        match lr with
        | Empty -> assert false
        | Node (lrl, lrv, lrr, _) ->
          create (create ll lv lrl) lrv (create lrr v r)
  else if hr >= hl + 2 then
    match r with
    | Empty -> assert false
    | Node (rl, rv, rr, _) ->
      if height rr >= height rl then
        create (create l v rl) rv rr
      else
        match rl with
        | Empty -> assert false
        | Node (rll, rlv, rlr, _) ->
          create (create l v rll) rlv (create rlr rv rr)
  else
    create l v r

let rec add_left v = function
  | Empty -> Node (Empty, v, Empty, 1)
  | Node (l, v', r, _) -> bal (add_left v l) v' r

let rec add_right v = function
  | Empty -> Node (Empty, v, Empty, 1)
  | Node (l, v', r, _) -> bal l v' (add_right v r)

(* No assumption of height of l and r. *)
let rec make_tree l v r =
  match l , r with
  | Empty, _ -> add_left v r
  | _, Empty -> add_right v l
  | Node (ll, lv, lr, lh), Node (rl, rv, rr, rh) ->
    if lh > rh + 1 then bal ll lv (make_tree lr v r) else
    if rh > lh + 1 then bal (make_tree l v rl) rv rr else
    create l v r

(* Utilities *)
let rec split_leftmost = function
  | Empty -> raise Not_found
  | Node (Empty, v, r, _) -> (v, r)
  | Node (l, v, r, _) ->
    let v0, l' = split_leftmost l in
    (v0, make_tree l' v r)

let rec split_rightmost = function
  | Empty -> raise Not_found
  | Node (l, v, Empty, _) -> (v, l)
  | Node (l, v, r, _) ->
    let v0, r' = split_rightmost r in
    (v0, make_tree l v r')

let rec concat t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | Node (l1, v1, r1, h1), Node (l2, v2, r2, h2) ->
    if h1 < h2 then
      make_tree (concat t1 l2) v2 r2
    else
      make_tree l1 v1 (concat r1 t2)

let rec iter proc = function
  | Empty -> ()
  | Node (l, v, r, _) ->
    iter proc l;
    proc v;
    iter proc r

let rec fold f t init =
  match t with
  | Empty -> init
  | Node (l, v, r, _) ->
    let x = fold f l init in
    let x = f v x in
    fold f r x

(* FIXME: this is nlog n because of the left nesting of appends *)
let rec to_stream =
  function
  | Empty -> Stream.empty ()
  | Node (l, v, r, _) ->
    Stream.append
      (Stream.append
	 (Stream.of_lazy (lazy (to_stream l)))
	 (Stream.singleton v))
      (Stream.of_lazy (lazy (to_stream r)))

end

include BatAvlTree

type t = (int * int) tree

let rec mem s (n:int) =
  if is_empty s then false else
    let v1, v2 = root s in
    if n < v1 then mem (left_branch s) n else
      if v1 <= n && n <= v2 then true else
	mem (right_branch s) n

let rec intersects_range s i j =
  if i > j then raise (Invalid_argument "iset_intersects_range") ;
  if is_empty s then false
  else
    let v1, v2 = root s in
    if j < v1 then intersects_range (left_branch s) i j
    else if v2 < i then intersects_range (right_branch s) i j
    else true

let rec add s n =
  if is_empty s then make_tree empty (n, n) empty else
  let (v1, v2) as v = root s in
  let s0 = left_branch s in
  let s1 = right_branch s in
  if v1 <> min_int && n < v1 - 1 then make_tree (add s0 n) v s1 else
  if v2 <> max_int && n > v2 + 1 then make_tree s0 v (add s1 n) else
  if n + 1 = v1 then
    if not (is_empty s0) then
      let (u1, u2), s0' = split_rightmost s0 in
      if u2 <> max_int && u2 + 1 = n then
	make_tree s0' (u1, v2) s1
      else
	make_tree s0 (n, v2) s1
    else
      make_tree s0 (n, v2) s1
  else if v2 + 1 = n then
    if not (is_empty s1) then
      let (u1, u2), s1' = split_leftmost s1 in
      if n <> max_int && n + 1 = u1 then
	make_tree s0 (v1, u2) s1'
      else
	make_tree s0 (v1, n) s1
    else
      make_tree s0 (v1, n) s1
  else s

let rec from s ~n =
  if is_empty s then empty else
  let (v1, v2) as v = root s in
  let s0 = left_branch s in
  let s1 = right_branch s in
  if n < v1 then make_tree (from s0 ~n) v s1 else
  if n > v2 then from s1 ~n else
  make_tree empty (n, v2) s1

let after s ~n = if n = max_int then empty else from s ~n:(n + 1)

let rec until s ~n =
  if is_empty s then empty else
  let (v1, v2) as v = root s in
  let s0 = left_branch s in
  let s1 = right_branch s in
  if n > v2 then make_tree s0 v (until s1 ~n) else
  if n < v1 then until s0 ~n else
  make_tree s0 (v1, n) empty

let before s ~n = if n = min_int then empty else until s ~n:(n - 1)

let add_range s n1 n2 =
  if n1 > n2 then invalid_arg (Printf.sprintf "ISet.add_range - %d > %d" n1 n2) else
  let n1, l =
    if n1 = min_int then n1, empty else
    let l = until s ~n:(n1 - 1) in
    if is_empty l then n1, empty else
    let (v1, v2), l' = split_rightmost l in
    if v2 + 1 = n1 then v1, l' else n1, l in
  let n2, r =
    if n2 = max_int then n2, empty else
    let r = from s ~n:(n2 + 1) in
    if is_empty r then n2, empty else
    let (v1, v2), r' = split_leftmost r in
    if n2 + 1 = v1 then v2, r' else n2, r in
  make_tree l (n1, n2) r

let singleton n = singleton_tree (n, n)

let rec remove s n =
  if is_empty s then empty else
  let (v1, v2) as v = root s in
  let s1 = left_branch s in
  let s2 = right_branch s in
  if n < v1 then make_tree (remove s1 n) v s2
  else if n = v1 then
    if v1 = v2 then concat s1 s2 else
    make_tree s1 (v1 + 1, v2) s2
  else if n > v1 && n < v2 then
    let s = make_tree s1 (v1, n - 1) empty in
    make_tree s (n + 1, v2) s2
  else if n = v2 then make_tree s1 (v1, v2 - 1) s2 else
  make_tree s1 v (remove s2 n)

let remove_range s n1 n2 =
  if n1 > n2 then invalid_arg "ISet.remove_range" else
  concat (before s ~n:n1) (after s ~n:n2)

let rec union s1 s2 =
  if is_empty s1 then s2 else
  if is_empty s2 then s1 else
  let s1, s2 = if height s1 > height s2 then s1, s2 else s2, s1 in
  let n1, n2 = root s1 in
  let l1 = left_branch s1 in
  let r1 = right_branch s1 in
  let l2 = before s2 ~n:n1 in
  let r2 = after s2 ~n:n2 in
  let n1, l =
    if n1 = min_int then n1, empty else
    let l = union l1 l2 in
    if is_empty l then n1, l else
    let (v1, v2), l' = split_rightmost l in (* merge left *)
    if v2 + 1 = n1 then v1, l' else n1, l in
  let n2, r =
    if n1 = max_int then n2, empty else
    let r = union r1 r2 in
    if is_empty r then n2, r else
    let (v1, v2), r' = split_leftmost r in (* merge right *)
    if n2 + 1 = v1 then v2, r' else n2, r in
  make_tree l (n1, n2) r

(*$= union & ~cmp:equal ~printer:(IO.to_string print)
  (union (of_list [3,5]) (of_list [1,3])) (of_list [1,5])
  (union (of_list [3,5]) (of_list [1,2])) (of_list [1,5])
  (union (of_list [3,5]) (of_list [1,5])) (of_list [1,5])
  (union (of_list [1,5]) (of_list [3,5])) (of_list [1,5])
  (union (of_list [1,2]) (of_list [4,5])) (of_list [1,2;4,5])
 *)

let rec inter s1 s2 =
  if is_empty s1 then empty else
  if is_empty s2 then empty else
  let s1, s2 = if height s1 > height s2 then s1, s2 else s2, s1 in
  let n1, n2 = root s1 in
  let l1 = left_branch s1 in
  let r1 = right_branch s1 in
  let l2 = before s2 ~n:n1 in
  let r2 = after s2 ~n:n2 in
  let m = until (from s2 ~n:n1) ~n:n2 in
  concat (concat (inter l1 l2) m) (inter r1 r2)

(*$= inter & ~cmp:equal ~printer:(IO.to_string print)
  (inter (of_list [1,5]) (of_list [2,3])) (of_list [2,3])
  (inter (of_list [1,4]) (of_list [2,6])) (of_list [2,4])
 *)

let rec compl_aux n1 n2 s =
  if is_empty s then add_range empty n1 n2 else
  let v1, v2 = root s in
  let l = left_branch s in
  let r = right_branch s in
  let l = if v1 = min_int then empty else compl_aux n1 (v1 - 1) l in
  let r = if v2 = max_int then empty else compl_aux (v2 + 1) n2 r in
  concat l r

let compl s = compl_aux min_int max_int s

let diff s1 s2 = inter s1 (compl s2)

let rec compare_aux x1 x2 =
  match x1, x2 with
    [], [] -> 0
  | `Set s :: rest, x ->
      if is_empty s then compare_aux rest x2 else
      let l = left_branch s in
      let v = root s in
      let r = right_branch s in
      compare_aux (`Set l :: `Range v :: `Set r :: rest) x
  | _x, `Set s :: rest ->
      if is_empty s then compare_aux x1 rest else
      let l = left_branch s in
      let v = root s in
      let r = right_branch s in
      compare_aux x1 (`Set l :: `Range v :: `Set r :: rest)
  | `Range ((v1, v2)) :: rest1, `Range ((v3, v4)) :: rest2 ->
      let sgn = Int.compare v1 v3 in
      if sgn <> 0 then sgn else
      let sgn = Int.compare v2 v4 in
      if sgn <> 0 then sgn else
      compare_aux rest1 rest2
  | [], _ -> ~-1
  | _, [] -> 1

let compare s1 s2 = compare_aux [`Set s1] [`Set s2]

let equal s1 s2 = compare s1 s2 = 0

let rec subset s1 s2 =
  if is_empty s1 then true else
  if is_empty s2 then false else
  let v1, v2 = root s2 in
  let l2 = left_branch s2 in
  let r2 = right_branch s2 in
  let l1 = before s1 ~n:v1 in
  let r1 = after s1 ~n:v2 in
  (subset l1 l2) && (subset r1 r2)

let fold_range s ~init ~f = BatAvlTree.fold (fun (n1, n2) x -> f n1 n2 x) s init

let fold s ~init ~f =
  let rec g n1 n2 a =
    if n1 = n2 then f n1 a else
    g (n1 + 1) n2 (f n1 a) in
  fold_range ~f:g s ~init

let iter s ~f = fold s ~init:() ~f:(fun n () -> f n)

let iter_range s ~f = BatAvlTree.iter (fun (n1, n2) -> f n1 n2) s

let for_all s ~f =
  let rec test_range n1 n2 =
    if n1 = n2 then f n1 else
    f n1 && test_range (n1 + 1) n2 in
  let rec test_set s =
    if is_empty s then true else
    let n1, n2 = root s in
    test_range n1 n2 &&
    test_set (left_branch s) &&
    test_set (right_branch s) in
  test_set s

(*$T for_all
  for_all (fun x -> x < 10) (of_list [1,3;2,7])
  not (for_all (fun x -> x = 5) (of_list [4,5]))
 *)

let exists s ~f =
  let rec test_range n1 n2 =
    if n1 = n2 then f n1 else
    f n1 || test_range (n1 + 1) n2 in
  let rec test_set s =
    if is_empty s then false else
    let n1, n2 = root s in
    test_range n1 n2 ||
    test_set (left_branch s) ||
    test_set (right_branch s) in
  test_set s

(*$T exists
  exists (fun x -> x = 5) (of_list [1,10])
  not (exists (fun x -> x = 5) (of_list [1,3;7,10]))
 *)

let filter_range p n1 n2 a =
  let rec loop n1 n2 a = function
      None ->
	if n1 = n2 then
	  make_tree a (n1, n1) empty
	else
	  loop (n1 + 1) n2 a (if p n1 then Some n1 else None)
    | Some v1 as x ->
	if n1 = n2 then	make_tree a (v1, n1) empty else
	if p n1 then
	  loop (n1 + 1) n2 a x
	else
	  loop (n1 + 1) n2 (make_tree a (v1, n1 - 1) empty) None in
  loop n1 n2 a None

let filter s ~f = fold_range s ~f:(filter_range f) ~init:empty

let partition_range p n1 n2 (a, b) =
  let rec loop n1 n2 acc =
    let acc =
      let a, b, (v, n) = acc in
      if p n1 = v then acc else
      if v then
	(make_tree a (n, n1) empty, b, (not v, n1))
      else
	(a, make_tree b (n, n1) empty, (not v, n1)) in
    if n1 = n2 then
      let a, b, (v, n) = acc in
      if v then	(make_tree a (n, n1) empty, b) else
      (a, make_tree b (n, n1) empty)
    else
      loop (n1 + 1) n2 acc in
  loop n1 n2 (a, b, (p n1, n1))

let partition s ~f = fold_range ~f:(partition_range f) s ~init:(empty, empty)

let cardinal s =
  fold_range ~f:(fun n1 n2 c -> c + n2 - n1 + 1) s ~init:0

(*$T cardinal
  cardinal (of_list [1,3;5,9]) = 8
 *)

let rev_ranges s =
  fold_range ~f:(fun n1 n2 a -> (n1, n2) :: a) s ~init:[]

let rec burst_range n1 n2 a =
  if n1 = n2 then n1 :: a else
  burst_range n1 (n2 - 1) (n2 :: a)

let elements s =
  let f a (n1, n2) = burst_range n1 n2 a in
  List.fold_left f [] (rev_ranges s)

(*$Q ranges;of_list
  (Q.list (Q.pair Q.int Q.int)) (fun l -> \
    let norml = List.map (fun (x,y) -> if x < y then (x,y) else (y,x)) l in \
    let set = of_list norml in \
    equal set (ranges set |> of_list) \
  )
 *)

let ranges s = List.rev (rev_ranges s)

let min_elt s =
  let (n, _), _ = split_leftmost s in
  n

let max_elt s =
  let (_, n), _ = split_rightmost s in
  n

let choose s = fst (root s)

let of_list l = List.fold_left (fun s (lo,hi) -> add_range s lo hi) empty l
let of_stream e = Stream.fold ~f:(fun s (lo,hi) -> add_range s lo hi) ~init:empty e
