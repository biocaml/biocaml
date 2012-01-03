open OUnit
open Batteries
open Printf

module Range = Biocaml_range
module IntervalTree = Biocaml_intervalTree

module ListImpl = struct
  type 'a t = (int * int * 'a) list

  exception Empty_tree

  let empty = []

  let is_empty x = ( = ) empty x

  let add lo hi v t = (lo, hi, v) :: t

  let cardinal = List.length

  let elements x = x

  let enum x = 
    Legacy.List.sort compare x |> List.enum

  let backwards x = 
    Legacy.List.sort (flip compare) x |> List.enum

  let pos x = 
    if x < 0 then 0 else x

  let interval_dist lo hi lo' hi' = Range.(
    pos (gap (make lo hi) (make lo' hi'))
  )

let interval_overlap lo hi lo' hi' =
  ( || )
    (hi >= lo' && hi <= hi')
    (lo >= lo' && lo <= hi')
  
let interval_distance lo hi lo' hi' =
  if interval_overlap lo hi lo' hi' then 0
  else min (abs (lo' - hi)) (abs (lo - hi'))

let interval_dist = interval_distance

  let find_closest lo hi = function
    | [] -> raise Empty_tree
    | h :: t -> 
      List.fold_left 
	(fun ((lo',hi',_) as interval') ((lo'', hi'', _) as interval'') ->
	  if interval_dist lo hi lo' hi' <= interval_dist lo hi lo'' hi''
	  then interval'
	  else interval'')
	h t

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

module TestAdditions(I : module type of Biocaml_intervalTree) = struct
  include I
  let of_list l = 
    List.fold_left
      (fun accu (lo,hi,v) -> I.add lo hi v accu)
      I.empty l
end

module T = TestAdditions(IntervalTree)
module L = TestAdditions(ListImpl)

let test_add () =
  for i = 1 to 100 do
    let intervals = random_intervals 100 |> List.of_enum in
    List.fold_left
      (fun accu (lo,hi,_) -> 
	let r = T.add lo hi () accu in
	Biocaml_intervalTree.check_integrity r ; r)
      T.empty intervals |> ignore
  done

let test_creation () =
  for i = 1 to 100 do
    let intervals = random_intervals 100 |> List.of_enum in
    let lres = L.(intervals |> of_list |> enum |> List.of_enum)
    and tres = T.(intervals |> of_list |> enum |> List.of_enum) in
    assert_equal ~printer:string_of_int 100 ~msg:"Verify list result length" (List.length lres) ;
    assert_equal ~printer:string_of_int 100 ~msg:"Verify tree result length" (List.length tres) ;
    assert_equal ~msg:"Compare list and tree results" lres tres
  done

let test_find_closest () = 
  for i = 1 to 1000 do
    let intervals = random_intervals ~ub:1000 1000 |> List.of_enum
    and lo, hi, _ = random_interval  ~ub:1000 () in
    let llo,lhi,_ = L.(find_closest lo hi (of_list intervals))
    and tlo,thi,_ = T.(find_closest lo hi (of_list intervals)) 
    and dist (x,y) = ListImpl.interval_dist lo hi x y in
    assert_equal
      ~cmp:(fun x y -> dist x = dist y)
      ~printer:(fun (lo',hi') -> sprintf "[%d,%d](%d to [%d,%d])" lo' hi' (dist (lo', hi')) lo hi)
      (llo, lhi) (tlo, thi)
  done

let tests = "IntervalTree" >::: [
  "Add" >:: test_add;
  "Creation" >:: test_creation;
  "Find closest" >:: test_find_closest;
]
