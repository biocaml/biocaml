open Biocaml_internal_pervasives
module Range = Biocaml_range

type t = Range.t list (* retained in canonical form *)
type range = Range.t
exception Bad of string
let raise_bad msg = raise (Bad msg)

let empty = []
let size t = List.fold_left ~f:(fun ans v -> ans + Range.size v) ~init:0 t
let is_empty t = size t = 0

(* A canonical interval list is one in which adjacent intervals have gap between them and intervals retained in ascending order according to their coordinates. *)
let rec is_canonical (vl : Range.t list) : bool =
  match vl with
    | [] | _::[] -> true
    | u::(v::vl as tail) -> u.Range.hi < v.Range.lo && is_canonical tail
        
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
      | u::(v::vl as tail) ->
          if u = v then
            canonize ans tail
          else if Range.superset u v then
            canonize ans (u::vl)
          else if Range.before u v then
            match Range.union u v with
              | uv::[] -> canonize ans (uv::vl)
              | u::v::[] -> canonize (u::ans) tail
              | _ -> invalid_arg "impossible to get here"
          else
            invalid_arg "impossible to get here"
  in
  let ans = List.rev (canonize [] vl) in
  assert(is_canonical ans);
  ans
    
let of_range_list l = 
  let f acc (x,y) =
    match Range.make_opt x y with
      | Some range -> range::acc 
      | None -> acc
  in
  to_canonical (List.fold ~f ~init:[] l)

let to_range_list t = List.map ~f:Range.to_pair t

let to_list t = List.concat (List.map ~f:Range.to_list t)  
let union s t = to_canonical (s @ t) (* better implementation possible *)

let inter s t =
  let rec loop ans s t =
    match (s,t) with
      | (_, []) -> ans
      | ([], _) -> ans
      | ((u::s as ul), (v::t as vl)) ->
          if u.Range.lo > v.Range.hi then
            loop ans ul t
          else if u.Range.hi < v.Range.lo then
            loop ans s vl
          else
            match Range.intersect u v with
              | None -> invalid_arg "impossible to get here"
              | Some w ->
                  match Pervasives.compare u.Range.hi v.Range.hi with
                    | -1 -> loop (w::ans) s vl
                    |  0 -> loop (w::ans) s t
                    |  1 -> loop (w::ans) ul t
                    |  _ -> invalid_arg "impossible to get here"
  in
  to_canonical (loop [] s t) (* canoninicity could maybe be obtained simply by List.rev *)
    
let diff s t =
  let rec loop ans s t =
    match (s,t) with
      | (_,[]) -> ans @ (List.rev s)
      | ([],_) -> ans
      | ((u::s as ul), (v::t as vl)) ->
          if u.Range.lo > v.Range.hi then
            loop ans ul t
          else if u.Range.hi < v.Range.lo then
            loop (u::ans) s vl
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
  to_canonical (loop [] s t)

let subset s t = is_empty (diff s t)

  (*
module Test = struct
    
  let make_int_set (l : (int * int) list) : Int.Set.t =
    let f accum (lo,hi) =
      if lo > hi then
        accum
      else
        let v = Range.make lo hi in
        Int.Set.union accum (Int.Set.of_list (Range.to_list v))
    in
    List.fold_left ~f ~init:Int.Set.empty l
      
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

  let default_test () =
    let f () = BatList.init 10000 (fun _ -> let x = Random.int 100 in x, Random.int 20 + x) in
    let st = Random.get_state () in
    Random.init 42 ;
    test (f ()) (f ()) ;
    Random.set_state st
end
  *)
