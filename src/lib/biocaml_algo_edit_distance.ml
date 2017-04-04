(** Allowed edit operations while computing edit distance. This type is for
    internal use only, so it is not exported *)
type edit_operation = 
    Insert of int*char | Delete of int | Substitute of int*char;;

(** Simple edit distance calculation bitween 2 strings. Only three operations: 
    Insert, Delete and Substitue are taken into account. All the operations 
    have the same weight equal to 1. The first and the second parameters
    are the strings to compare.

    This is a dynamic programming algorithm that requires m*n memory and
    m*n time (m and n are the lengths of the strings to compare).
*)
let simpleEditDistance s t =
  let m = String.length s in
  let n = String.length t in
  let d = Array.make_matrix (m + 1) (n + 1) 0 in
  for i = 0 to m do
    d.(i).(0) <- i
  done;
  for j = 0 to n do
    d.(0).(j) <- j
  done;
  for j = 1 to n do
    for i = 1 to m do
      if s.[i - 1] = t.[j - 1] then
        d.(i).(j) <- d.(i - 1).(j - 1)
      else
        d.(i).(j) <- 1 + 
          (min d.(i-1).(j-1) (min d.(i-1).(j) d.(i).(j-1)))
    done;
  done;
  d.(m).(n)
;;

