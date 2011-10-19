open Batteries
open Printf

module Location = struct
  type t = {
    chr : string ;
    st : int ;
    ed : int
  }

  let to_string l = sprintf "%s:%d-%d" l.chr l.st l.ed

  let length l = l.ed - l.st + 1

  let make chr st ed = 
    if st > ed then (
      let msg = sprintf "Genome.Location.make: incorrect coordinates %d %d\n" st ed in
      raise (Invalid_argument msg) ;
    ) ;
    { chr = chr ; st = st ; ed = ed }

  let of_string s = 
    Scanf.sscanf 
      s "%s@:%d@-%d" 
      make

  let relmove l st ed = { l with st = l.st + st ; ed = l.ed + ed }

  let included_in s s' =
    if s.chr <> s'.chr then false 
    else (s.st >= s'.st && s.ed <= s'.ed)

  let intersection s s' = 
    if s.chr <> s'.chr then false else (
      let p s s' = (s.ed >= s'.st) && (s.ed <= s'.ed)
      in p s s' || p s' s
    )

  let inter s s' = 
    if not (intersection s s') then raise (Invalid_argument "Genome.Location.inter")
    else (make s.chr (max s.st s'.st) (min s.ed s'.ed))
      
  let dist s s' = 
    if s.chr <> s'.chr then raise (Invalid_argument "Genome.Location.dist")
    else (
      if intersection s s' then 0
      else min (abs (s'.st - s.ed)) (abs (s.st - s'.ed))
    )

  let position ~from loc = 
    if loc.chr <> from.chr then raise (Invalid_argument "Genome.Location.position")
    else (
      if intersection from loc then 0
      else (
	let a, b = from.st - loc.ed, loc.st - from.ed in
	if abs a < abs b then a else b
      )
    )
      
  let compare (x : t) (y : t) = compare x y    
end

module Annotation = struct
  open Printf
  open Batteries
  open Location

  type 'a t = (Location.t * 'a) array

  let compare x y = Location.compare (fst x) (fst y)

  let of_array a = 
    let res = Array.copy a
    in Array.fast_sort Pervasives.compare res ; res


  let rec regionsinwindow_start_aux regions loc a b =
    if a > b then raise Not_found
    else if a = b then (
      if Location.intersection loc (fst regions.(a)) then a
      else raise Not_found
    )
    else (
      let i = (a + b) / 2 in 
      if Location.intersection loc (fst regions.(i)) then
	regionsinwindow_start_aux regions loc a i
      else (
	if loc < (fst regions.(i)) then 
	  regionsinwindow_start_aux regions loc a (i - 1)
	else
	  regionsinwindow_start_aux regions loc (i + 1) b
      )
    )
  let regionsinwindow_start regions loc = regionsinwindow_start_aux regions loc 0 (Array.length regions - 1)
    
  let rec regionsinwindow_end_aux regions loc a b =
    if a > b then raise Not_found
    else if a = b then (
      if Location.intersection loc (fst regions.(a)) then a
      else raise Not_found
    )
    else (
      let i = (a + b) / 2 + 1 in 
      if Location.intersection loc (fst regions.(i)) then
	regionsinwindow_end_aux regions loc i b
      else (
	if loc < (fst regions.(i)) then 
	  regionsinwindow_end_aux regions loc a (i - 1)
	else
	  regionsinwindow_end_aux regions loc (i + 1) b
      )
    )
  let regionsinwindow_end regions loc = regionsinwindow_end_aux regions loc 0 (Array.length regions - 1)
    
  let regionsinwindow regions loc =
    try 
      let a = regionsinwindow_start regions loc in
      let b = regionsinwindow_end   regions loc 
      in 
      Array.sub regions a (b - a + 1)
    with Not_found -> [| |]

  let nbregions_in regions loc = 
    try 
      let a = regionsinwindow_start regions loc in
      let b = regionsinwindow_end   regions loc 
      in 
      b - a + 1
    with Not_found -> 0


  let cardinal = Array.length





  let search_width = 1000 :: 10000 :: 100000 :: 1000000 :: 1000000000 :: []

  let rec closest_region_aux loc map = function
      [] -> raise Not_found
    | w :: t ->
	let search_region = Location.relmove loc (- w) w in
	try 
	  let a = regionsinwindow_start map search_region 
	  and b = regionsinwindow_end   map search_region 
	  in
	  let istar, dstar = ref a, ref (Location.dist loc (fst map.(a)))
	  in
	  for i = a + 1 to b do
	    let d = Location.dist loc (fst map.(i)) in
	    if d < !dstar then (
	      istar := i ;
	      dstar := d
	    )
	  done ;
	  (fst map.(!istar), snd map.(!istar), !dstar)
	with Not_found -> (closest_region_aux loc map t)
  let closest_region loc map = closest_region_aux loc map search_width


  (* FIXME: kclosest and closest should be merged someday after better testing *)
  let rec k_closest_regions_aux k loc map = function
      [] -> raise Not_found
    | w :: t ->
	let search_region = Location.relmove loc (- w) w in
	try 
	  let a = regionsinwindow_start map search_region 
	  and b = regionsinwindow_end   map search_region in
	  let _ = 
	    if b - a + 1 < k 
	    then raise Not_found (* fails if not enough regions were found, just as if none were found *)
	  in
	  let selected = Array.init 
	    (b - a + 1)
	    (fun i -> let loc' = fst map.(a + i) in (Location.dist loc loc', a + i))
	  in
	  Array.fast_sort Pervasives.compare selected ;
	  Array.init k (fun i -> 
			  fst map.(snd selected.(i)),
			  snd map.(snd selected.(i)),
			  fst selected.(i))
	with Not_found -> (k_closest_regions_aux k loc map t)
  let k_closest_regions k loc map = k_closest_regions_aux k loc map search_width


  let intersects loc map = 
    try ignore (regionsinwindow_start map loc) ; true
    with Not_found -> false


  let fold f map init = Array.fold_left (fun accu (x,y) -> f x y accu) init map

  let fold_in f regions loc init = 
    try 
      let a = regionsinwindow_start regions loc 
      and b = regionsinwindow_end   regions loc 
      and accu = ref init in 
      for i = a to b do
	let loc, v = regions.(i) 
	in accu := f loc v !accu
      done ;
      !accu
    with Not_found -> init

  let map f = Array.map (fun (x,y) -> x, f x y)



  let regions_in map loc = Array.map snd (regionsinwindow map loc)




  type 'a partition = 'a t
  let of_partition x = x

  module Partition = struct

    let rec insert_in_stack (l,a) = function
	[] -> [ l , [ a ] ]

      | ((l', _) :: _) as stack when not (Location.intersection l l') && l < l' ->
	  (l,[ a ]) :: stack

      | ((l', _) :: t) when (Location.intersection l l') && l.st < l'.st ->
	  (make l.chr l.st (l'.st - 1), [ a ]) :: (insert_in_stack (make l.chr l'.st l.ed, a) t)

      | (l', ann) :: t when Location.intersection l l' && l'.st = l.st && l.ed < l'.ed ->
	  (l, a :: ann) :: (make l.chr (l.ed + 1) l'.ed, ann) :: t

      | (l', ann) :: t  when l = l' -> (l, a :: ann) :: t

      | (l', ann) :: t when Location.intersection l l' && l'.st = l.st && l.ed > l'.ed ->
	  (l', a :: ann) :: (insert_in_stack (make l.chr (l'.ed + 1) l.ed, a) t)

      | (l', ann) :: t when Location.intersection l l' && l'.st < l.st ->
	  (make l.chr l'.st (l.st - 1), ann) :: (insert_in_stack (l,a) ((make l.chr l.st l'.ed, ann) :: t))

      | ((l', _) :: t) when not (Location.intersection l l') && l > l' ->
	  insert_in_stack (l,a) t
      | _ -> assert false

    let rec fold_aux f map i accu = function
	[]          when i = Array.length map -> accu
      | (l, a) :: t when i = Array.length map ->
	  fold_aux f map i (f l a accu) t
      | [] -> 
	  fold_aux f map (i + 1) accu (insert_in_stack map.(i) [])
      | ((l, a) :: t) as stack -> 
	  let l', a' = map.(i) in
	  if Location.intersection l l' then
	    fold_aux f map (i + 1) accu (insert_in_stack map.(i) stack)
	  else fold_aux f map i (f l a accu) t
    ;;
    let fold f map init = fold_aux f map 0 init []


    let rec fold2_aux f map i map' j accu = function
	[]          when i = Array.length map && j = Array.length map' -> accu
      | (l, a) :: t when i = Array.length map && j = Array.length map' -> 
	  fold2_aux f map i map' j (f l a accu) t

      | []          when i = Array.length map -> 
	  let (l, a) = map'.(j) 
	  in fold2_aux f map i map' (j + 1) accu (insert_in_stack (l, `B a) [])

      | []          when j = Array.length map' -> 
	  let (l, a) = map.(i) 
	  in fold2_aux f map (i + 1) map' j accu (insert_in_stack (l, `A a) [])

      | ((l, a) :: t) as stack when i = Array.length map -> 
	  let l_j, a_j = map'.(j) in
	  if Location.intersection l l_j then
	    fold2_aux f map i map' (j + 1) accu (insert_in_stack (l_j, `B a_j) stack)
	  else 
	    fold2_aux f map i map' j (f l a accu) t

      | ((l, a) :: t) as stack when j = Array.length map' -> 
	  let l_i, a_i = map.(i) in
	  if Location.intersection l l_i then
	    fold2_aux f map (i + 1) map' j accu (insert_in_stack (l_i, `A a_i) stack)
	  else 
	    fold2_aux f map i map' j (f l a accu) t

      | [] ->
	  let l_i, a_i = map.(i) 
	  and l_j, a_j = map'.(j) in
	  if l_i <= l_j then 
	    fold2_aux f map (i + 1) map' j accu (insert_in_stack (l_i, `A a_i) [])
	  else
	    fold2_aux f map i map' (j + 1) accu (insert_in_stack (l_j, `B a_j) [])
	      
      | ((l, a) :: t) as stack -> 
	  let l_i, a_i = map.(i) 
	  and l_j, a_j = map'.(j) in
	  if l_i <= l_j then 
	    if Location.intersection l l_i then
	      fold2_aux f map (i + 1) map' j accu (insert_in_stack (l_i, `A a_i) stack)
	    else 
	      fold2_aux f map i map' j (f l a accu) t
	  else
	    if Location.intersection l l_j then
	      fold2_aux f map i map' (j + 1) accu (insert_in_stack (l_j, `B a_j) stack)
	    else 
	      fold2_aux f map i map' j (f l a accu) t
    ;;
    (* Traversal of two maps : [f] is called on each segment on which 
       [map] and [map'] are constant. [f] is given a list of annotations 
       as present in [map] and [map'], and their origin is kept by tagging 
       them with polymorphic variants `A and `B respectively. *)
    let fold2 f map map' init = fold2_aux f map 0 map' 0 init []

    let length map = Array.fold_right (fun (loc, _) -> ( + ) (Location.length loc)) map 0

    let make f map = 
      Array.of_list (List.rev (fold (fun l ann accu -> (l, f l ann) :: accu)  map []))

    let filter f map = Array.filter (fun (loc, ann) -> f loc ann) map


    let eval ~na ~chr ~pos ~map = 
      let loc = Location.make chr pos pos in
      match regionsinwindow map loc with
	  [| |] -> na
	| [| (l, v) |] -> v
	| _ -> assert false (* shouldn't happen with a flat map *)


  end



  let length map = Partition.fold (fun loc _ -> ( + ) (Location.length loc)) map 0

  let rec union_aux map i map' j accu =
    if i = Array.length map && j = Array.length map' then List.rev accu
    else if i = Array.length map then 
      union_aux map i map' (j + 1) (map'.(j) :: accu)
    else if j = Array.length map' then
      union_aux map (i + 1) map' j (map.(i) :: accu)
    else if fst map.(i) <= fst map'.(j) then
      union_aux map (i + 1) map' j (map.(i) :: accu)
    else
      union_aux map i map' (j + 1) (map'.(j) :: accu)

  let union map map' = Array.of_list (union_aux map 0 map' 0 [])


  exception Not_flat

  let oneandonlyone = function 
      [ e ] -> e
    | [] -> assert false
    | _ -> raise Not_flat


  let to_array f map = Array.map (fun (loc,x) -> f loc x) map

  let enum map = BatArray.enum map

  (* separate the annotation list produced by fold2 into two lists,
     one for each original map *)
  let partition l = 
    List.fold_left 
      (fun (la, lb) -> function `A x -> (x :: la, lb) | `B x -> (la, x :: lb))
      ([], [])
      l

  let union_partition m m' =
    let rev_list = 
      Partition.fold2 
	(fun l ann accu  -> 
	   let la, lb as pann = partition ann 
	   in (l, pann) :: accu)
	m
	m'
	[]
    in
    Array.of_list (List.rev rev_list)
      
  let intersection_partition m m' =
    let rev_list = 
      Partition.fold2 
	(fun l ann accu  -> 
	   let la, lb as pann = partition ann in
	   if la <> [] && lb <> [] then (l, pann) :: accu
	   else accu)
	m
	m'
	[]
    in
    Array.of_list (List.rev rev_list)

  let mask m m' =
    let rev_list = 
      Partition.fold2 
	(fun l ann accu  -> 
	   let la, lb = partition ann in
	   if la <> [] && lb <> [] then (l, la) :: accu
	   else accu)
	m
	m'
	[]
    in
    Array.of_list (List.rev rev_list)


  let mask_partition m m' =
    let rev_list = 
      Partition.fold2 
	(fun l ann accu  -> 
	   let la, lb = partition ann in
	   if la <> [] && lb <> [] then (
	     assert (List.length la = 1) ;
	     (l, List.hd la) :: accu
	   )
	   else accu)
	m
	m'
	[]
    in
    Array.of_list (List.rev rev_list)


  let make n e = 
    let r = Array.init n (fun _ -> Option.get (Enum.get e)) 
    in Array.fast_sort compare r ; r



  let leftjoin_aux up down bmap fa a = 
    let loc = Location.relmove (fa a) up down in
    let res = fold_in (fun _ b accu -> b ::  accu) bmap loc [] in
    a, Array.of_list (List.rev res)

  let leftjoin ?(up = 0) ?(down = 0) fa fb a b = 
    let bmap = of_array (Array.map (fun b -> fb b, b) b) in
    Array.map (leftjoin_aux up down bmap fa) a
end
