open TylesBase
open Printf
open Array

exception ValueError of string

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
  (List.fold_left2 f 0. a1 a2) /. (float_of_int ((List.length a1) - 1))
    
let rank arr =
  let arr = Array.copy arr in
  let arr = Array.mapi (fun i a -> a,i) arr in
  Array.sort (fun (a,_) (b,_) -> Pervasives.compare a b) arr;
  let g prev il ans = 
    let count = List.length il in
    let n = count + (List.length ans) in
    let hi = float_of_int n in
    let lo = float_of_int (n - count + 1) in
    let rank = (hi +. lo) /. 2. in
    (List.map (fun i -> rank,i) il) @ ans
  in
  let f (prev, il, ans) (x,i) =   (* prev is the value that was equal *)
    let count = List.length il in (* il is list of original indices in reverse for items that were equal *)
    if count = 0 then             (* ans is list of ranks and original index pairs in reverse *)  
      x, [i], ans               
    else if x = prev then
      x, i::il, ans
    else
      x, [i], g prev il ans
  in
  let prev,il,ans = Array.fold_left f (0.,[],[]) arr in
  let ans = g prev il ans in
  let ans = List.sort ~cmp:(fun (_,a) (_,b) -> Pervasives.compare a b) ans in
  Array.of_list (List.map fst ans)

let spearman (arr1:float array) (arr2: float array) = 
  let arr1,arr2 = rank arr1, rank arr2 in
  pearson arr1 arr2

let cnd x = 
(* Modified from C++ code by David Koppstein. Found from
   www.sitmo.com/doc/Calculating_the_Cumulative_Normal_Distribution *)
  let b1,b2,b3,b4,b5,p,c = 
    0.319381530, -0.356563782, 1.781477937, -1.821255978, 
    1.330274429, 0.2316419, 0.39894228 in
  if x >= 0. then
    let t = 1. /. (1. +. (p *. x)) in
    (1. -. (c *. (exp (-.x *. x /. 2.)) *. t *. 
      (t *. (t *. (t *. ((t *. b5) +. b4) +. b3) +. b2) +. b1 )))
    else 
      let t = 1. /. (1. -. p *. x) in
      c *. (exp (-.x *. x /. 2.)) *. t *.
        (t *. (t *. (t *. ((t *. b5) +. b4) +. b3) +. b2) +. b1 )
    
let ltqnorm p =
(* 
   Modified from python code by David Koppstein. Original comments follow below. 
   First version was written in perl, by Peter J. Acklam, 2000-07-19. 
   Second version was ported to python, by Dan Field, 2004-05-03. 
*)
    if (p <= 0.) || (p >= 1.) then
      raise (ValueError ("Argument to ltqnorm " ^ (string_of_float p) ^ 
        " must be in open interval (0,1)"))
    else
      (* Coefficients in rational approximations. *)
      let a = 
        [|-3.969683028665376e+01; 2.209460984245205e+02; 
        -2.759285104469687e+02; 1.383577518672690e+02; 
        -3.066479806614716e+01; 2.506628277459239e+00|] in
      let b = 
        [|-5.447609879822406e+01; 1.615858368580409e+02; 
        -1.556989798598866e+02; 6.680131188771972e+01; 
        -1.328068155288572e+01|] in
      let c = 
        [|-7.784894002430293e-03; -3.223964580411365e-01; 
        -2.400758277161838e+00; -2.549732539343734e+00; 
        4.374664141464968e+00; 2.938163982698783e+00|] in
      let d = 
        [|7.784695709041462e-03; 3.224671290700398e-01; 
        2.445134137142996e+00; 3.754408661907416e+00|] in

      (* Define break-points. *)
      let plow  = 0.02425 in
      let phigh = 1. -. plow in
      let f q = 
        (((((c.(0)*.q+.c.(1))*.q+.c.(2))*.q+.c.(3))*.q+.c.(4))*.q+.c.(5)) /.
          ((((d.(0)*.q+.d.(1))*.q+.d.(2))*.q+.d.(3))*.q+.1.)
      in

    (* Rational approximation for lower region: *)
    if p < plow then
      let q  = sqrt ((-2.) *. (log p)) in
      f q

    (* Rational approximation for upper region: *)
    else if phigh < p then
      let q = sqrt ((-2.) *. (log (1. -. p))) in
      f q

    (* Rational approximation for central region: *)
    else 
      let q = p -. 0.5 in
      let r = q *. q in
    (((((a.(0)*.r+.a.(1))*.r+.a.(2))*.r+.a.(3))*.r+.a.(4))*.r+.a.(5))*.q /. 
      (((((b.(0)*.r+.b.(1))*.r+.b.(2))*.r+.b.(3))*.r+.b.(4))*.r+.1.)

let wilcoxon_rank_sum_to_z arr1 arr2 = 
  let l1,l2 = (Array.length arr1),(Array.length arr2) in
  let ranked = rank (Array.append arr1 arr2) in
  let arr1 = Array.sub ranked 0 l1 in
  let l1,l2 = (float_of_int l1), (float_of_int l2) in
  let sum1 = 
    let f acc elem = elem +. acc in
    Array.fold_left f 0. arr1
  in
  let expectation = (l1 *. (l1 +. l2 +. 1.)) /. 2. in
  let var = (l1 *. l2 *. ((l1 +. l2 +. 1.) /. 12.)) in
  (sum1 -. expectation) /. (sqrt var)
  
let wilcoxon_rank_sum ?(alpha=0.05) arr1 arr2 = 
  let l1,l2 = (Array.length arr1),(Array.length arr2) in
  let ranked = rank (Array.append arr1 arr2) in
  let arr1 = Array.sub ranked 0 l1 in
  let l1,l2 = (float_of_int l1), (float_of_int l2) in
  let sum1 = 
    let f acc elem = elem +. acc in
    Array.fold_left f 0. arr1
  in
  let expectation = (l1 *. (l1 +. l2 +. 1.)) /. 2. in
  let var = (l1 *. l2 *. ((l1 +. l2 +. 1.) /. 12.)) in
  let z = abs_float ((sum1 -. expectation) /. (sqrt var)) in
  (* here we assume this is a two-tailed distribution *)
  let threshold = abs_float (ltqnorm (1. -. (alpha /. 2.))) in
  if z < threshold then true else false

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

let factorial n =
  if n < 2 then 1 else
    let rec aux acc n = if n < 2 then acc else aux (n * acc) (n - 1) in
    aux 1 n

let epsilon f init fin = 
  let rec aux acc n = if n = fin then acc else aux (acc +. (f n fin)) (n + 1) in
  aux 0. init

let shuffle result = 
  let result = Array.copy result in
  for i = Array.length result - 1 downto 0 do
    let other = Random.int (i + 1) and tmp = result.(i) in
    result.(i) <- result.(other); result.(other) <- tmp
  done; result
