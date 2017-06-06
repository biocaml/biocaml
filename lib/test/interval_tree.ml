open Core_kernel.Std
open CFStream
open Stream.Infix
module Interval_tree = Biocaml_unix.Interval_tree
module Range = Biocaml_unix.Range
open OUnit

module ListImpl = struct
  type 'a t = (int * int * 'a) list

  exception Empty_tree

  let empty = []

  let is_empty x = ( = ) empty x

  let add t ~low ~high ~data = (low, high, data) :: t

  let cardinal = List.length

  let elements x = x

  let to_stream x =
    List.sort ~cmp:compare x |> Stream.of_list

  let to_backwards_stream x =
    List.sort ~cmp:(Fn.flip compare) x |> Stream.of_list

  let pos x =
    if x < 0 then 0 else x

  let interval_overlap lo hi lo' hi' =
    ( || )
      (lo  <= lo' && lo' <= hi)
      (lo' <= lo  && lo  <= hi')

  let intersects l ~low ~high =
    List.exists ~f:(fun (lo',hi',_) -> interval_overlap low high lo' hi') l

  let interval_distance lo hi lo' hi' =
    if interval_overlap lo hi lo' hi' then 0
    else min (abs (lo' - hi)) (abs (lo - hi'))

  let interval_dist = interval_distance

  let find_closest lo hi = function
  | [] -> raise Empty_tree
  | h :: t ->
      let lo', hi', x =
        List.fold_left t ~init:h ~f:(fun ((lo',hi',_) as interval') ((lo'', hi'', _) as interval'') ->
	  if interval_dist lo hi lo' hi' <= interval_dist lo hi lo'' hi''
	  then interval'
	  else interval''
	)
      in lo', hi', x, interval_dist lo hi lo' hi'

  let find_intersecting_elem lo hi t =
    Stream.filter
      ~f:(fun (x,y,_) -> interval_overlap lo hi x y)
      (to_stream t)

  let filter_overlapping t ~low ~high =
    List.filter
      ~f:(fun (x,y,_) -> interval_overlap low high x y)
      t

  let print _ = assert false
  let check_integrity _ = assert false
end

let random_interval ?(lb = 0) ?(ub = 100) ?(minw = 1) ?(maxw = 30) _ =
  assert (maxw < ub - lb) ;
  let w = Random.int (maxw - minw) + minw in
  let lo = Random.int (ub - lb - w) + lb in
  (lo, lo + w, ())

let random_intervals ?(lb = 0) ?(ub = 100) ?(minw = 1) ?(maxw = 30) n =
  (1 -- n) /@ (random_interval ~lb ~ub ~minw ~maxw)

module TestAdditions(I : module type of Biocaml_unix__Interval_tree) = struct
  include I
  let of_list l =
    List.fold_left l ~init:I.empty ~f:(fun accu (low,high,data) -> I.add accu ~low ~high ~data)
end

module T = TestAdditions(Interval_tree)
module L = TestAdditions(ListImpl)

let test_add () =
  for _ = 1 to 100 do
    let intervals = random_intervals 100 |> Stream.to_list in
    List.fold_left intervals ~init:T.empty ~f:(fun accu (lo,hi,_) ->
      let r = T.add accu ~low:lo ~high:hi ~data:() in
      Interval_tree.check_integrity r ; r
    )
    |> ignore
  done

let test_creation () =
  for _ = 1 to 100 do
    let intervals = random_intervals 100 |> Stream.to_list in
    let lres = L.(intervals |> of_list |> to_stream |> Stream.to_list)
    and tres = T.(intervals |> of_list |> to_stream |> Stream.to_list) in
    assert_equal ~printer:string_of_int 100 ~msg:"Verify list result length" (List.length lres) ;
    assert_equal ~printer:string_of_int 100 ~msg:"Verify tree result length" (List.length tres) ;
    assert_equal ~msg:"Compare list and tree results" lres tres
  done

let test_intersection () =
  for _ = 1 to 1000 do
    let intervals = random_intervals ~ub:1000 1000 |> Stream.to_list
    and low, high, _ = random_interval  ~ub:1000 () in
    let l = L.(intersects (of_list intervals) ~low ~high)
    and t = T.(intersects (of_list intervals) ~low ~high) in
    assert_equal l t
  done

let test_find_closest () =
  for _ = 1 to 1000 do
    let intervals = random_intervals ~ub:1000 1000 |> Stream.to_list
    and lo, hi, _ = random_interval  ~ub:1000 () in
    let llo,lhi,_, dl = L.(find_closest lo hi (of_list intervals))
    and tlo,thi,_, dt = T.(find_closest lo hi (of_list intervals))
    and dist (x,y) = ListImpl.interval_dist lo hi x y in
    assert_equal
      ~cmp:(fun x y -> dist x = dist y)
      ~printer:(fun (lo',hi') -> sprintf "[%d,%d](%d to [%d,%d])" lo' hi' (dist (lo', hi')) lo hi)
      (llo, lhi) (tlo, thi) ;
    assert_equal dl dt ;
    assert_equal dl (dist (llo, lhi))
  done



let test_find_intersecting_elem1 () =
  let u =
    T.(add ~low:69130 ~high:69630 ~data:()
         (add ~low:69911 ~high:70411 ~data:()
            (add ~low:70501 ~high:71001 ~data:()
               empty))) in
  assert_equal
    T.(find_intersecting_elem 70163 70163 u |> Stream.to_list |> List.length)
    1 ;
  assert_equal
    T.(find_intersecting_elem 65163 75163 u |> Stream.to_list |> List.length)
    3

let test_find_intersecting_elem2 () =
  for _ = 1 to 1000 do
    let intervals = random_intervals ~ub:1000 1000 |> Stream.to_list
    and lo, hi, _ = random_interval  ~ub:1000 () in
    let l = L.(find_intersecting_elem lo hi (of_list intervals)) |> Stream.to_set
    and t = T.(find_intersecting_elem lo hi (of_list intervals)) |> Stream.to_set in
    assert_equal Set.(length (union (diff l t) (diff t l))) 0 ;
  (* [assert_equal l t] is not a valid test because sets cannot be
     compared with ( = ). Indeed, a set is an AVL tree whose structure
     may depend on the order its elements were added: ( = ) compare
     the structure directly and may for this reason fail to see two
     sets have the same elements *)
  done

let tests = "IntervalTree" >::: [
  "Add" >:: test_add;
  "Creation" >:: test_creation;
  "Intersection" >:: test_intersection;
  "Find closest" >:: test_find_closest;
  "Find intersecting elements (1)" >:: test_find_intersecting_elem1;
  "Find intersecting elements (2)" >:: test_find_intersecting_elem2;
]
