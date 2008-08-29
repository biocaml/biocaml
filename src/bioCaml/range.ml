open TylesBase
open Printf

type t = {lo:int; hi:int}
exception Bad of string
let raise_bad msg = raise (Bad msg)

let make l u =
  if l <= u then {lo=l; hi=u}
  else raise_bad (sprintf "lower bound %d must be less than or equal to upper bound %d" l u)

let make_opt l u = try Some (make l u) with Bad _ -> None

let size v = v.hi - v.lo + 1
let equal u v = u.lo = v.lo && u.hi = v.hi
let member t k = t.lo <= k && k <= t.hi
let to_string t = String.concat "" ["["; string_of_int t.lo; ", "; string_of_int t.hi; "]"]
let to_list v = List.init (size v) ((+) v.lo)
let overlap u v = (min u.hi v.hi) - (max u.lo v.lo) + 1
let gap u v = -(overlap u v)

let union u v =
  if overlap u v < 0 then [u;v]
  else [{lo = min u.lo v.lo; hi = max u.hi v.hi}]

let intersect u v =
  let l = max u.lo v.lo in
  let h = min u.hi v.hi in
    if l <= h then Some {lo=l; hi=h} else None

let strict_before u v = u.lo < v.lo && u.hi < v.hi
let strict_after u v = strict_before v u
let before u v = strict_before u v || equal u v
let after u v = before v u

let compare_positional u v =
  if equal u v then Some 0
  else if strict_before u v then Some (-1)
  else if strict_after u v then Some 1
  else None

let subset u v = u.lo >= v.lo && u.hi <= v.hi
let superset u v = subset v u
let strict_subset u v = subset u v && not (equal u v)
let strict_superset u v = strict_subset v u

let compare_containment u v =
  if equal u v then Some 0
  else if strict_subset u v then Some (-1)
  else if strict_superset u v then Some 1
  else None

let compare_lo u v = Pervasives.compare u.lo v.lo
let compare_hi u v = Pervasives.compare u.hi v.hi
                
let any_overlap tl =
  let tl = List.sort ~cmp:compare_lo tl in
  let rec loop tl =
    match tl with
      | [] | _::[] -> false
      | u::v::tl -> v.lo <= u.hi || loop (v::tl)
  in loop tl
    
let all_positional vl =
  let cmp = Order.compose compare_containment compare_positional in
  let vl = List.sort ~cmp vl in
  let rec loop vl =
    match vl with
      | [] | _::[] -> true
      | u::v::vl -> (before u v) && loop (v::vl)
  in loop vl

let max_gap_of_positional vl =
  let cmp = Order.compose compare_containment compare_positional in
  let vl = List.sort ~cmp vl in
  let rec loop ans vl =
    match vl with
      | [] | _::[] -> ans
      | u::v::vl ->
          if before u v
          then loop (max ans (gap u v)) (v::vl)
          else failwith (sprintf "ranges %s and %s not positionally comparable" (to_string u) (to_string v))
  in
  match vl with
    | [] | _::[] -> failwith "given fewer than two ranges"
    | u::v::vl -> loop (gap u v) (v::vl)

let find_min_range ?(init_direction="fwd") v pred i =
  if i < v.lo || i > v.hi then invalid_arg (sprintf "%d not in range %s" i (to_string v));
  let rec loop (dir:string) ans =
    if pred ans then Some ans
    else if equal ans v then None
    else
      match dir with
        | "fwd" -> let hi' = if ans.hi = v.hi then ans.hi else ans.hi+1 in loop "rev" {ans with hi = hi'}
        | "rev" -> let lo' = if ans.lo = v.lo then ans.lo else ans.lo-1 in loop "fwd" {ans with lo = lo'}
        | _ -> invalid_arg (sprintf "valid directions are \"fwd\" or \"rev\" but given \"%s\"" dir)
  in loop init_direction {lo=i; hi=i}

let expand_assoc_list dat =
  match dat with
    | [] -> []
    | (u,_)::_ ->
        let min = List.fold_left (fun ans (v,_) -> min v.lo ans) u.lo dat in
        let max = List.fold_left (fun ans (v,_) -> max v.hi ans) u.hi dat in
        let size = max + 1 in
        if min < 0 then
          failwith "expand_assoc_list given ranges ranging over negative values"
        else
          let ans = Array.make size [] in
          let f (v,a) = List.iter (fun i -> ans.(i) <- a::ans.(i)) (to_list v) in
          let _ = List.iter f dat in
          List.map List.rev (Array.to_list ans)

let exp_assoc_list tal =
  let ans = Hashtbl.create 100 in
  let insert (t,a) =
    for i = t.lo to t.hi do
      let prev = try Hashtbl.find ans i with Not_found -> [] in
      Hashtbl.replace ans i (a::prev)
    done
  in
  let _ = List.iter insert tal in
  let ans = Hashtbl.fold (fun key value ans -> (key,value)::ans) ans [] in
  List.rev (List.map (fun (i,al) -> i, List.rev al) ans)
  
(*  
let unoverlap_positional f tal =
  let cmp (u,_) (v,_) = Order.compose compare_containment compare_positional u v in
  let tal = List.sort ~cmp tal in
  let rec loop ans tal =
    | [] -> ans
    | (t,a)::[] -> 
*)

let find_regions ?(max_gap=0) pred tal =
  if any_overlap (List.map fst tal) then failwith "overlapping ranges not allowed";
  let tal = List.sort ~cmp:(fun (u,_) (v,_) -> Order.totalify compare_positional u v) tal in  

  (* see below for meaning of [curr] *)    
  let insert (curr : (t * int) option) ans =
    match curr with
      | None -> ans
      | Some (v,gap) ->
          try (make v.lo (v.hi - gap))::ans
          with Bad _ -> failwith (Msg.bug (sprintf "gap = %d, range = %s" gap (to_string v)))    
  in

  (* curr = Some (v,gap) means [v] is region built up thus far,
   *        with last [gap] elements failing pred, [0 <= gap <= max_gap].
   * curr = None means no region currently being built *)
  let rec loop (curr : (t * int) option) ans tal =
    match tal with
      | [] -> insert curr ans
      | (t,a)::tal ->
          match curr with
            | None ->
                let curr = if pred a then Some(t,0) else None in
                loop curr ans tal
            | Some (curr_v, curr_gap) ->
                let extra_gap = t.lo - curr_v.hi - 1 in
                let t,pred,tal =
                  if extra_gap > 0 then
                    make (curr_v.hi + 1) (t.lo - 1), false, ((t,a)::tal)
                  else 
                    t, pred a, tal
                in
                let curr_v = make curr_v.lo t.hi in
                let curr_gap = if pred then 0 else size t + curr_gap in
                let curr = Some (curr_v, curr_gap) in
                if curr_gap > max_gap
                then loop None (insert curr ans) tal
                else loop curr ans tal
  in
  List.rev (loop None [] tal)
    
let rec make_random t =
  let max = t.hi - t.lo + 1 in
  let l = t.lo + Random.int max in 
  let u = t.lo + Random.int max in
  try make l u with Bad _ -> make_random t
    
