(** Simple calculation of edit distance bitween 2 strings. Only three 
    operations: Insert, Delete and Substitue are taken into account. 
    All the operations have the same weight equal to 1. The first and 
    the second parameters are the strings to compare.

    This is a standard dynamic programming algorithm that requires 
    (min m n) memory and m*n time (m and n are the lengths of the strings 
    to compare).
*)
let simpleEditDistance t s =
  let s, m, t, n =
    let m, n = String.length s, String.length t in
    if m < n then
      s, m, t, n
    else
      t, n, s, m
  in (* now s is not larger than t and m <= n *)
  let d = Array.init (m + 1) (fun i -> i) in

  (* 3 argument min function (uses 2 arg standard min) *)
  let min a b c = min a (min b c) in
  for j = 1 to n do
    d.(0) <- j;
    let diag = ref (j - 1) in

    for i = 1 to m do
      let old = d.(i) in
      let diag_cost = if s.[i - 1] = t.[j - 1] then 0 else 1 in
      d.(i) <- min (old + 1) (d.(i - 1) + 1) (!diag + diag_cost);

      diag := old
    done;
  done;
  d.(m)
;;
