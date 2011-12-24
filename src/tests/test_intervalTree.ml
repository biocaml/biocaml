open OUnit
open Batteries

module Range = Biocaml_range
module IntervalTree = Biocaml_intervalTree

module ListImplementation = struct
  type 'a t = (int * int * 'a) list

  let empty = []

  let is_empty = ( = ) empty

  let add lo hi v t = (lo, hi, v) :: t

  let cardinal = List.length

  let elements x = x

  let enum x = 
    List.sort compare x |> List.enum

  let backwards x = 
    List.sort (flip compare) x |> List.enum

  let pos x = 
    if x < 0 then 0 else x

  let interval_dist lo hi lo' hi' = 
    pos (gap (Range.make lo hi) (Range.make lo' hi'))

  let find_closest lo hi v = function
    | [] -> raise IntervalTree.Empty_tree
    | h :: t -> 
      List.fold_left 
	(fun ((lo',hi',_) as opt) ((lo'', hi'', _) as cand) ->
	  if interval_dist lo hi lo' hi' <= interval_dist lo hi lo'' hi''
	  then interval'
	  else interval'')
	h t
      
end

let test_creation () = ()

let tests = "IntervalTree" >::: [
  "Creation" >:: test_creation;
]
