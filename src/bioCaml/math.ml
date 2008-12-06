open TylesBase
open Printf
open Array

let row m i =
  if i < length m then copy m.(i)
  else failwith (sprintf "invalid row index %d" i)
    
let column m i =
  if for_all (fun row -> i < length row) m
  then init (length m) (fun j -> m.(j).(i))
  else failwith (sprintf "invalid column index %d" i)
    
let transpose a =
  assert (is_rectangular a);
  if length a = 0 then [||] 
  else if length a.(0) = 0 then failwith "cannot transpose matrix with empty rows"
  else
    let ans = make_matrix (length a.(0)) (length a) a.(0).(0) in
    for i = 0 to length a - 1 do
      for j = 0 to length a.(i) - 1 do
        ans.(j).(i) <- a.(i).(j)
      done
    done;
    ans

let log ?base x =
  match base with
    | None -> Pervasives.log x
    | Some b -> (Pervasives.log x) /. (Pervasives.log b)
        
let log10 = Pervasives.log10
let log2 = log ~base:2.0

let even x = (x mod 2) = 0
let odd x = (x mod 2) <> 0

let min a = assert (length a > 0); fold_left min a.(0) a
let max a = assert (length a > 0); fold_left max a.(0) a
  
let range step first last =
  assert (step > 0.0);
  let n = (int_of_float <<- ceil <<- abs_float) ((last -. first) /. step) + 1 in
  let a = make n 0.0 in
  let (op,comp) = if first <= last then ((+.),(<=)) else ((-.),(>=)) in
  iteri (fun i _ -> a.(i) <- op first (float_of_int i *. step)) a;
  if comp a.(n-1) last
  then a
  else sub a 0 (n-1)
    
let mean a =
  let n = length a in
  assert (n > 0);
  (fold_left (+.) 0. a) /. (float_of_int n)
    
let variance a =
  let n = length a in
  assert (n > 1);
  let avrg = mean a in
  let f v = let diff = v -. avrg in diff *. diff in
  let a = map f a in
  (fold_left (+.) 0. a) /. (float_of_int (n - 1))
    
let rms = sqrt <<- mean <<- (map (fun x -> x *. x))
  
let stdv = sqrt <<- variance
  
let median a =
  let n = length a in
  assert (n > 0);
  let a = copy a in
  sort Pervasives.compare a;
  if odd n
  then a.((n+1)/2 - 1)
  else let m = (n+1)/2 in (a.(m-1) +. a.(m)) /. 2.0
                       
let pseudomedian a =
  let n = length a in
  assert (n > 0);
  if n = 1 then
    a.(0)
  else
    let nn = n*(n-1)/2 in
    let averages = make nn 0.0 in
    let idx = ref 0 in
    for i = 0 to n-2 do
      for j = i+1 to n-1 do
        averages.(!idx) <- (a.(i) +. a.(j)) /. 2.0;
        incr idx
      done
    done;
    median averages
      
let mad a =
  assert (length a > 0);
  let med = median a in
  let a = map (fun v -> abs_float (v -. med)) a in
  median a
    
let quantile_normalization aa =
  assert (is_rectangular aa);
  if length aa = 0 || length aa.(0) = 0 || length aa.(0) = 1 then
    Array.copy aa
  else
    let num_expts = float_of_int (length aa.(0)) in
    let num_pts = length aa in
    
    let comp1 (a,_) (b,_) = Pervasives.compare a b in
    let comp2 (_,a) (_,b) = Pervasives.compare a b in
    
    let aa = transpose aa in
    let aa = map (mapi Tuple.Pr.make) aa in
    (iter (sort comp2)) aa;
    let avg i = (fold_left (fun sum expt -> snd expt.(i) +. sum) 0.0 aa) /. num_expts in
    let norms = init num_pts avg in
    let aa = map (mapi (fun i (idx,_) -> idx, norms.(i))) aa in
    (iter (sort comp1)) aa;
    transpose (map (map snd) aa)

let histogram ?(cmp=Pervasives.compare) arr =
  let f mp a =
    if PMap.mem a mp
    then PMap.add a (PMap.find a mp + 1) mp
    else PMap.add a 1 mp
  in
  let mp = fold_left f (PMap.create cmp) arr in
  let ans = PMap.foldi (fun a k ans -> (a,k)::ans) mp [] in
  of_list (List.rev ans)

let prediction_values tp tn fp fn =
  let tp = float_of_int tp in
  let tn = float_of_int tn in
  let fp = float_of_int fp in
  let fn = float_of_int fn in
  let sensitivity = tp /. (tp +. fn) in
  let specificity = tn /. (fp +. tn) in
  let pos_prediction_accuracy = tp /. (tp +. fp) in
  let neg_prediction_accuracy = tn /. (tn +. fn) in
  sensitivity, specificity, pos_prediction_accuracy, neg_prediction_accuracy

let pearson (a1:float array) (a2:float array) = 
  let a1avg,a2avg = (mean a1),(mean a2) in
  let a1sd,a2sd = (stdv a1),(stdv a2) in
  let a1,a2 = (Array.to_list a1), (Array.to_list a2) in
  let f acc e1 e2 = 
    (((e1 -. a1avg) /. a1sd) *. ((e2 -. a2avg) /. a2sd)) +. acc 
  in
  (List.fold_left2 f 0. a1 a2) /. (float_of_int (List.length a1))
    
let rank (arr:float array) = 
  let newarr = Array.copy arr in
  let newarr = Array.mapi (fun idx a -> (a, idx)) newarr in
  let _ = Array.sort (fun (a,_) (b,_) -> Pervasives.compare a b) newarr in
  if Array.unique (fun (a,_) (b,_) -> Pervasives.compare a b) newarr 
  then 
    let newarr = Array.mapi (fun i (a,idx) -> (i + 1,idx)) newarr in
    (Array.sort (fun (_,a) (_,b) -> Pervasives.compare a b) newarr; 
    Array.map (fun (i,idx) -> float_of_int i) newarr)
  else 
    let len = Array.length newarr in
    let ranker a = 
      let putter count idx = 
        let r = 
          if count = 1 then (float_of_int idx) else
            let hi = float_of_int idx in
            let lo = float_of_int (idx - count + 1) in
            (hi +. lo) /. 2.
        in
        let rec putter_aux r count idx = 
          if count = 0 then ()
          else 
            let (_,prev) = a.(idx - 1) in
            a.(idx - 1) <- (r, prev); putter_aux r (count - 1) (idx - 1)
        in
        putter_aux r count idx
      in
      let rec ranker_aux count idx =
        if idx >= len then putter count idx
        else 
          match count with
            | 0 -> ranker_aux 1 1
            | n -> 
                let (m,_),(n,_) = a.(idx),a.(idx - 1) in
                if m = n 
                then ranker_aux (count + 1) (idx + 1)
                else (putter count idx; ranker_aux 1 (idx + 1))
      in
      ranker_aux 0 0; newarr
    in
    let ranked_arr = ranker newarr in
    (Array.sort (fun (_,a) (_,b) -> Pervasives.compare a b) ranked_arr;
    Array.map (fun (i,idx) -> i) newarr)

let spearman (arr1:float array) (arr2: float array) = 
  let arr1,arr2 = rank arr1, rank arr2 in
  pearson arr1 arr2

let idxsort (cmp : 'a -> 'a -> int) (a : 'a array) : int array =
  let a = mapi Tuple.Pr.make a in
  sort (fun a b -> cmp (snd a) (snd b)) a;
  map fst a

let find_regions ?(max_gap=0) pred a =
  if max_gap < 0 then failwith ("max gap must be non-negative but is " ^ (string_of_int max_gap));
  let size = length a in
  let ans = ref [] in
  
  (* Add region built up thus far, if any, to ans.
   * curr_index is one beyond what will be considered for inclusion in region *)
  let add_region curr_index start_index currGap =
    if start_index >= 0 then
      let finish_index = curr_index - currGap - 1 in
      ans := (start_index,finish_index)::!ans
  in
  
  (* i is current array index.
   * start_index is index of a region that has started to be built, -1 if none started yet.
   * currGap is number of previous contiguous items failing pred *)
  let rec loop i start_index currGap = 
    if i = size
    then add_region i start_index currGap
    else
      (
        if pred a.(i) then
          if start_index >= 0 
          then loop (i+1) start_index 0
          else loop (i+1) i 0
        else
          (
            if currGap >= max_gap
            then (add_region i start_index currGap; loop (i+1) (-1) (currGap+1))
            else loop (i+1) start_index (currGap+1)
          )
      )
  in
  loop 0 (-1) 0;
  (of_list <<- List.rev) !ans
    
let find_min_window ?(init_direction="fwd") a pred i =
  let size = length a in
  if size < 1 then
    [||]
  else
    let v = Range.make 0 (size - 1) in
    let pred v = pred v.Range.lo v.Range.hi in
    let ans = Range.find_min_range ~init_direction v pred i in
    match ans with
      | None -> [||]
      | Some ans -> sub a ans.Range.lo (ans.Range.hi - ans.Range.lo + 1)

