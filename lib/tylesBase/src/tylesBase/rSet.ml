module List = List2
open Printf

type t = Range.t list (* retained in canonical form *)
type range = Range.t
exception Bad of string
let raise_bad msg = raise (Bad msg)

let empty = []
let size t = List.fold_left (fun ans v -> ans + Range.size v) 0 t
let is_empty t = size t = 0

(* A canonical interval list is one in which adjacent intervals have gap between them and intervals retained in ascending order according to their coordinates. *)
let rec is_canonical (vl : Range.t list) : bool =
  match vl with
    | [] | _::[] -> true
    | u::v::vl -> u.Range.hi < v.Range.lo && is_canonical (v::vl)
        
let to_canonical (vl : Range.t list) : Range.t list =
  (* Order relation such that subset and after are larger. *)
  let compare_intervals =
    Order.compose
      (Order.reversep Range.compare_containment)
      Range.compare_positional
  in
  
  let vl = List.sort ~cmp:compare_intervals vl in
  
  let rec canonize ans vl =
    match vl with
      | [] -> ans
      | v::[] -> v::ans
      | u::v::vl ->
          if Range.equal u v then
            canonize ans (v::vl)
          else if Range.superset u v then
            canonize ans (u::vl)
          else if Range.before u v then
            match Range.union u v with
              | uv::[] -> canonize ans (uv::vl)
              | u::v::[] -> canonize (u::ans) (v::vl)
              | _ -> invalid_arg "impossible to get here"
          else
            invalid_arg "impossible to get here"
  in
  let ans = List.rev (canonize [] vl) in
  assert(is_canonical ans);
  ans
    
let of_range_list l = to_canonical l
let to_range_list t = t
let to_list t = List.flatten (List.map Range.to_list t)  
let union s t = of_range_list (s @ t) (* better implementation possible *)

let inter s t =
  let rec loop ans s t =
    match (s,t) with
      | (_, []) -> ans
      | ([], _) -> ans
      | (u::s, v::t) ->
          if u.Range.lo > v.Range.hi then
            loop ans (u::s) t
          else if u.Range.hi < v.Range.lo then
            loop ans s (v::t)
          else
            match Range.intersect u v with
              | None -> invalid_arg "impossible to get here"
              | Some w ->
                  match Pervasives.compare u.Range.hi v.Range.hi with
                    | -1 -> loop (w::ans) s (v::t)
                    |  0 -> loop (w::ans) s t
                    |  1 -> loop (w::ans) (u::s) t
                    |  _ -> invalid_arg "impossible to get here"
  in
  of_range_list (loop [] s t) (* calling of_range_list assures canoninicity, 
                               * but maybe could get that simply by List.rev *)
    
let diff s t =
  let rec loop ans s t =
    match (s,t) with
      | (_,[]) -> ans @ (List.rev s)
      | ([],_) -> ans
      | (u::s, v::t) ->
          if u.Range.lo > v.Range.hi then
            loop ans (u::s) t
          else if u.Range.hi < v.Range.lo then
            loop (u::ans) s (v::t)
          else
            match Range.intersect u v with
              | None -> invalid_arg "impossible to get here"
              | Some w ->
                  let u_pre = Range.make_opt u.Range.lo (w.Range.lo - 1) in
                  let u_post = Range.make_opt (w.Range.hi + 1) u.Range.hi in
                  (* v_pre = Range.make_opt v.Range.lo (w.Range.lo - 1)
                   ** v_pre not needed, note also that u_pre and v_pre cannot both be None *)
                  let v_post = Range.make_opt (w.Range.hi + 1) v.Range.hi in
                  let ans = match u_pre with None -> ans | Some x -> x::ans in
                  let s = match u_post with None -> s | Some x -> x::s in
                  let t = match v_post with None -> t | Some x -> x::t in
                  loop ans s t
  in
  of_range_list (loop [] s t)

let subset s t = is_empty (diff s t)

module Test = struct
    
  let make_int_set (l : Range.t list) : IntSet.t =
    List.fold_left (fun s v -> IntSet.union s (IntSet.of_list (Range.to_list v))) IntSet.empty l
      
  let test vl1 vl2 =
    let intset1 = Test.timesf "making first IntSet" make_int_set vl1 in
    let intset2 = Test.timesf "making second IntSet" make_int_set vl2 in
    let set1 = Test.timesf "making first efficient set" of_range_list vl1 in
    let set2 = Test.timesf "making second efficient set" of_range_list vl2 in
    
    let is_good intset_op set_op op_name =
      let ans1 = Test.timesf ("naive " ^ op_name) (intset_op intset1) intset2 in
      let ans2 = Test.timesf ("efficient " ^ op_name) (set_op set1) set2 in
      if IntSet.to_list ans1 = to_list ans2
      then print_endline (op_name ^ " test PASSED\n")
      else print_endline (op_name ^ " test FAILED\n")
    in
    is_good IntSet.inter inter "intersection";
    is_good IntSet.union union "union";
    is_good IntSet.diff diff "diff"
      
end



  
(*
  type interval = t
  module Set = struct
  module S = Reins.AVLSet.MonoSet
  (struct
  type t = interval

  let compare v1 v2 =
  let precedes v1 v2 = is_before v1 v2 || proper_subset v2 v1 in
  if precedes v1 v2 then -1
  else if v1 = v2 then 0
  else (assert (precedes v2 v1); 1)
  
  let to_string = to_string
  end)
  include S
  end
*)
