open OUnit
open Batteries

module Range = Biocaml_range
module IntervalTree = Biocaml_intervalTree

module ListImpl : module type of Biocaml_intervalTree = struct
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

  let find_closest lo hi = function
    | [] -> raise Empty_tree
    | h :: t -> 
      List.fold_left 
	(fun ((lo',hi',_) as interval') ((lo'', hi'', _) as interval'') ->
	  if interval_dist lo hi lo' hi' <= interval_dist lo hi lo'' hi''
	  then interval'
	  else interval'')
	h t
      
end

let random_intervals ?(lb = 0) ?(ub = 100) ?(minw = 1) ?(maxw = 30) n = 
  assert (maxw < ub - lb) ;
  let aux _ = 
    let w = Random.int (maxw - minw) + minw in
    let lo = Random.int (ub - lb - w) + lb in
    (lo, lo + w, ())
  in
  (1 -- n) /@ aux

module TestAdditions(I : module type of Biocaml_intervalTree) = struct
  include I
  let of_list l = 
    List.fold_left
      (fun accu (lo,hi,v) -> I.add lo hi v accu)
      I.empty l
end

module T = TestAdditions(IntervalTree)
module L = TestAdditions(ListImpl)

let test_creation () =
  for i = 1 to 100 do
    let intervals = random_intervals 100 |> List.of_enum in
    let lres = L.(intervals |> of_list |> enum |> List.of_enum)
    and tres = T.(intervals |> of_list |> enum |> List.of_enum) in
    assert_equal ~printer:string_of_int 100 ~msg:"Verify list result length" (List.length lres) ;
    assert_equal ~printer:string_of_int 100 ~msg:"Verify tree result length" (List.length tres) ;
    assert_equal ~msg:"Compare list and tree results" lres tres
  done

let tests = "IntervalTree" >::: [
  "Creation" >:: test_creation;
]
