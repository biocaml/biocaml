open Core_kernel

exception ValueError of string

let row m i =
  if i < Array.length m then Array.copy m.(i)
  else failwith (sprintf "invalid row index %d" i)

let column m i =
  if Array.for_all ~f:(fun row -> i < Array.length row) m
  then Array.init (Array.length m) ~f:(fun j -> m.(j).(i))
  else failwith (sprintf "invalid column index %d" i)

let is_rectangular a =
  let dimension = Array.length a in
  Array.for_all a ~f:(fun suba -> Array.length suba = dimension)

let transpose a =
  if not (is_rectangular a) then invalid_arg "Math.transpose: not-rectangular";
  let n_rows = Array.length a in
  if Array.length a = 0 || Array.length a.(0) = 0 then [| |]
  else
    let n_cols = Array.length a.(0) in
    let ans = Array.make_matrix ~dimx:n_cols ~dimy:n_rows a.(0).(0) in
    for i = 0 to n_rows - 1 do
      for j = 0 to Array.length a.(i) - 1 do
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

let min a =
  Option.value_exn ~message:"Math.min: empty" (Array.reduce a ~f:min)
let max a =
  Option.value_exn ~message:"Math.max: empty" (Array.reduce a ~f:max)

let prange add step lo hi =
  let rec f acc x =
    if x > hi then
      List.rev acc
    else
      let next = add x step in
      f (x::acc) next
  in
  f [] lo

let range_ints = prange (+)
let range_floats = prange (+.)

let range step first last =
  assert (step > 0.0);
  let n =
    (((last -. first) /. step) |> Float.abs |> Float.round_up |> Int.of_float) + 1 in
  let a = Array.create ~len:n 0.0 in
  let (op,comp) = if first <= last then ((+.),(<=)) else ((-.),(>=)) in
  Array.iteri ~f:(fun i _ -> a.(i) <- op first (Float.of_int i *. step)) a;
  if comp a.(n-1) last
  then a
  else Array.sub a ~pos:0 ~len:(n-1)

let mean a =
  let n = Array.length a in
  assert (n > 0);
  (Array.fold ~f:(+.) ~init:0. a) /. (Float.of_int n)

let variance a =
  let n = Array.length a in
  assert (n > 1);
  let avrg = mean a in
  let f v = let diff = v -. avrg in diff *. diff in
  let a = Array.map ~f a in
  (Array.fold ~f:(+.) ~init:0. a) /. (Float.of_int (n - 1))

let rms a =
  Array.map ~f:(fun x -> x *. x) a |> mean |> sqrt

let stdv x = variance x |> sqrt

let median a =
  let n = Array.length a in
  assert (n > 0);
  let a = Array.copy a in
  Array.sort ~cmp:Pervasives.compare a;
  if odd n
  then a.((n+1)/2 - 1)
  else let m = (n+1)/2 in (a.(m-1) +. a.(m)) /. 2.0

let pseudomedian a =
  let n = Array.length a in
  assert (n > 0);
  if n = 1 then
    a.(0)
  else
    let nn = n*(n-1)/2 in
    let averages = Array.create ~len:nn 0.0 in
    let idx = ref 0 in
    for i = 0 to n-2 do
      for j = i+1 to n-1 do
        averages.(!idx) <- (a.(i) +. a.(j)) /. 2.0;
        incr idx
      done
    done;
    median averages

let mad a =
  assert (Array.length a > 0);
  let med = median a in
  let a = Array.map ~f:(fun v -> Float.abs (v -. med)) a in
  median a

let quantile_normalization aa =
  assert (is_rectangular aa);
  if Array.length aa = 0 || Array.length aa.(0) = 0
     || Array.length aa.(0) = 1 then
    Array.copy aa
  else
    let num_expts = Float.of_int (Array.length aa.(0)) in
    let num_pts = Array.length aa in

    let comp1 (a,_) (b,_) = Pervasives.compare a b in
    let comp2 (_,a) (_,b) = Pervasives.compare a b in

    let aa = transpose aa in
    let aa = Array.map ~f:(Array.mapi ~f:(fun a b -> (a, b))) aa in
    (Array.iter ~f:(Array.sort ~cmp:comp2)) aa;
    let avg i =
      (Array.fold ~f:(fun sum expt -> snd expt.(i) +. sum) ~init:0.0 aa)
      /. num_expts in
    let norms = Array.init num_pts ~f:avg in
    let aa = Array.map ~f:(Array.mapi ~f:(fun i (idx,_) -> idx, norms.(i))) aa in
    Array.iter ~f:(Array.sort ~cmp:comp1) aa;
    transpose (Array.map ~f:(Array.map ~f:snd) aa)

let histogram (type t) ?(cmp=Pervasives.compare) arr =
  let module M = struct
    include Map.Make(struct
      type t_ = t (* required only because OCaml doesn't have type non-rec definitions *)
      type t = t_
      let compare = cmp
      let sexp_of_t _ = assert false
      let t_of_sexp _ = assert false
    end)
  end
  in
  let f (mp : int M.t) (a:t) =
    match M.find mp a with
    | Some e -> M.add mp ~key:a ~data:(e + 1)
    | None -> M.add mp ~key:a ~data:1
  in
  let mp = Array.fold ~f ~init:M.empty arr in
  let ans = M.fold ~f:(fun ~key ~data ans -> (key,data)::ans) mp ~init:[] in
  Array.of_list (List.rev ans)

let prediction_values tp tn fp fn =
  let tp = Float.of_int tp in
  let tn = Float.of_int tn in
  let fp = Float.of_int fp in
  let fn = Float.of_int fn in
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
  (List.fold2_exn ~f ~init:0. a1 a2) /. (Float.of_int ((List.length a1) - 1))

let rank arr =
  let arr = Array.copy arr in
  let arr = Array.mapi ~f:(fun i a -> a,i) arr in
  Array.sort ~cmp:(fun (a,_) (b,_) -> Pervasives.compare a b) arr;
  let g _ il ans =
    let count = List.length il in
    let n = count + (List.length ans) in
    let hi = Float.of_int n in
    let lo = Float.of_int (n - count + 1) in
    let rank = (hi +. lo) /. 2. in
    (List.map ~f:(fun i -> rank,i) il) @ ans
  in
  let f (prev, il, ans) (x,i) =   (* prev is the value that was equal *)
    let count = List.length il in (* il is list of original indices in
                                     reverse for items that were equal *)
    if count = 0 then (* ans is list of ranks and original index pairs
                         in reverse *)
      x, [i], ans
    else if x = prev then
      x, i::il, ans
    else
      x, [i], g prev il ans
  in
  let prev,il,ans = Array.fold ~f ~init:(0.,[],[]) arr in
  let ans = g prev il ans in
  let ans = List.sort ~cmp:(fun (_,a) (_,b) -> Pervasives.compare a b) ans in
  Array.of_list (List.map ~f:fst ans)

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
      raise (ValueError ("Argument to ltqnorm " ^ (Float.to_string p) ^
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
  let arr1 = Array.sub ranked ~pos:0 ~len:l1 in
  let l1,l2 = (Float.of_int l1), (Float.of_int l2) in
  let sum1 =
    let f acc elem = elem +. acc in
    Array.fold ~f ~init:0. arr1
  in
  let expectation = (l1 *. (l1 +. l2 +. 1.)) /. 2. in
  let var = (l1 *. l2 *. ((l1 +. l2 +. 1.) /. 12.)) in
  (sum1 -. expectation) /. (sqrt var)

let wilcoxon_rank_sum_to_p arr1 arr2 =
  (* assumes a two-tailed distribution *)
  let z = wilcoxon_rank_sum_to_z arr1 arr2 in
  2. *. (1. -. (cnd (Float.abs z)))

let wilcoxon_rank_sum ?(alpha=0.05) arr1 arr2 =
  (wilcoxon_rank_sum_to_p arr1 arr2) < alpha

let idxsort (cmp : 'a -> 'a -> int) (a : 'a array) : int array =
  let a = Array.mapi a ~f:(fun i b -> (i, b)) in
  Array.sort ~cmp:(fun a b -> cmp (snd a) (snd b)) a;
  Array.map ~f:fst a

let find_regions ?(max_gap=0) pred a =
  if max_gap < 0 then failwith ("max gap must be non-negative but is " ^ (string_of_int max_gap));
  let size = Array.length a in
  let ans = ref [] in

  (* Add region built up thus far, if any, to ans.
   * curr_index is one beyond what will be considered for inclusion in region *)
  let add_region curr_index start_index currGap =
    if start_index >= 0 then
      let finish_index = curr_index - currGap - 1 in
      ans := (start_index,finish_index)::!ans
  in

  (* i is current array index.
   * start_index is index of a region that has started to be built, -1
     if none started yet.
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
  Array.of_list (List.rev !ans)

let find_min_window ?(init_direction="fwd") a pred i =
  let size = Array.length a in
  if size < 1 then
    [||]
  else
    let v = Range.make_unsafe 0 (size - 1) in
    let pred v = pred v.Range.lo v.Range.hi in
    let ans = Range.find_min_range ~init_direction v pred i in
    match ans with
      | None -> [||]
      | Some ans -> Array.sub a ~pos:ans.Range.lo ~len:(ans.Range.hi - ans.Range.lo + 1)

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
