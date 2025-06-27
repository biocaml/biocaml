type t = Range.t list (* retained in canonical form *)
type range = Range.t

let empty = []
let size t = List.fold_left ~f:(fun ans v -> ans + Range.size v) ~init:0 t
let is_empty t = size t = 0

(* A canonical interval list is one in which adjacent intervals have gap between them and intervals retained in ascending order according to their coordinates. *)
let rec is_canonical (vl : Range.t list) : bool =
  match vl with
  | [] | _ :: [] -> true
  | u :: (v :: _ as tail) -> u.Range.hi < v.Range.lo && is_canonical tail
;;

let to_canonical (vl : Range.t list) : Range.t list =
  (* Order relation such that subset and after are larger. *)
  let compare_intervals u v =
    match Range.compare_containment u v with
    | Some x -> -x
    | None -> (
      match Range.compare_positional u v with
      | Some x -> x
      | None -> assert false)
  in
  let vl = List.sort ~compare:compare_intervals vl in
  let rec canonize ans vl =
    match vl with
    | [] -> ans
    | v :: [] -> v :: ans
    | u :: (v :: vl as tail) ->
      if Poly.equal u v
      then canonize ans tail
      else if Range.superset u v
      then canonize ans (u :: vl)
      else if Range.before u v
      then (
        match Range.union u v with
        | `Joint uv -> canonize ans (uv :: vl)
        | `Disjoint (u, _) -> canonize (u :: ans) tail)
      else invalid_arg "impossible to get here"
  in
  let ans = List.rev (canonize [] vl) in
  assert (is_canonical ans);
  ans
;;

let of_range_list l =
  let f acc (x, y) = if x <= y then Range.make_unsafe x y :: acc else acc in
  to_canonical (List.fold ~f ~init:[] l)
;;

let to_range_list t = List.map ~f:(fun { Range.lo; hi } -> lo, hi) t
let to_list t = List.concat (List.map ~f:Range.to_list t)
let union s t = to_canonical (s @ t) (* better implementation possible *)

let inter s t =
  let rec loop ans s t =
    match s, t with
    | _, [] -> ans
    | [], _ -> ans
    | (u :: s as ul), (v :: t as vl) ->
      if u.Range.lo > v.Range.hi
      then loop ans ul t
      else if u.Range.hi < v.Range.lo
      then loop ans s vl
      else (
        match Range.intersect u v with
        | None -> invalid_arg "impossible to get here"
        | Some w -> (
          match Stdlib.compare u.Range.hi v.Range.hi with
          | -1 -> loop (w :: ans) s vl
          | 0 -> loop (w :: ans) s t
          | 1 -> loop (w :: ans) ul t
          | _ -> invalid_arg "impossible to get here"))
  in
  to_canonical (loop [] s t)
;;

(* canoninicity could maybe be obtained simply by List.rev *)

let diff s t =
  let rec loop ans s t =
    match s, t with
    | _, [] -> ans @ List.rev s
    | [], _ -> ans
    | (u :: s as ul), (v :: t as vl) ->
      if u.Range.lo > v.Range.hi
      then loop ans ul t
      else if u.Range.hi < v.Range.lo
      then loop (u :: ans) s vl
      else (
        match Range.intersect u v with
        | None -> invalid_arg "impossible to get here"
        | Some w ->
          let u_pre = Range.make u.Range.lo (w.Range.lo - 1) |> Result.ok in
          let u_post = Range.make (w.Range.hi + 1) u.Range.hi |> Result.ok in
          (* v_pre = Range.make_opt v.Range.lo (w.Range.lo - 1)
           ** v_pre not needed, note also that u_pre and v_pre cannot both be None *)
          let v_post = Range.make (w.Range.hi + 1) v.Range.hi |> Result.ok in
          let ans =
            match u_pre with
            | None -> ans
            | Some x -> x :: ans
          in
          let s =
            match u_post with
            | None -> s
            | Some x -> x :: s
          in
          let t =
            match v_post with
            | None -> t
            | Some x -> x :: t
          in
          loop ans s t)
  in
  to_canonical (loop [] s t)
;;

let subset s t = is_empty (diff s t)

module Test = struct
  let timesf msg f arg =
    let start = Core_unix.time () in
    let x = f arg in
    let stop = Core_unix.time () in
    printf "%s: %f seconds\n%!" msg (stop -. start);
    x
  ;;

  let make_int_set (l : (int * int) list) : Int.Set.t =
    let f accum (lo, hi) =
      if lo > hi
      then accum
      else (
        let v = Range.make_unsafe lo hi in
        Int.Set.union accum (Int.Set.of_list (Range.to_list v)))
    in
    List.fold_left ~f ~init:Int.Set.empty l
  ;;

  (** [test ul vl] compares performance and correctness of set intersection and
    union. Sets of type {!IntSet.t} and {!t} are constructed from the given [ul] and
    [vl], and the corresponding intersection and union operations are used on the
    two versions. Messages are printed reporting times required to construct the
    sets, and take their intersection and union. Also, it is verified that the
    operations produce identical results. *)
  let test vl1 vl2 =
    let intset1 = timesf "making first IntSet" make_int_set vl1 in
    let intset2 = timesf "making second IntSet" make_int_set vl2 in
    let set1 = timesf "making first efficient set" of_range_list vl1 in
    let set2 = timesf "making second efficient set" of_range_list vl2 in
    let is_good intset_op set_op op_name =
      let ans1 = timesf ("naive " ^ op_name) (intset_op intset1) intset2 in
      let ans2 = timesf ("efficient " ^ op_name) (set_op set1) set2 in
      printf "%s: %b\n" op_name Poly.(Int.Set.to_list ans1 = to_list ans2)
    in
    is_good Int.Set.inter inter "intersection";
    is_good Int.Set.union union "union";
    is_good Int.Set.diff diff "diff"
  ;;

  (** This function generates random lists and uses them as arguments for [test].
    The state of the [Random] module is not modified. *)

  let%expect_test _ =
    let f () =
      List.init 10000 ~f:(fun _ ->
        let x = Random.int 100 in
        x, Random.int 20 + x)
    in
    Random.init 42;
    printf "\n<RSet Benchmarking>\n";
    test (f ()) (f ());
    printf "\n</RSet Benchmarking>\n";
    [%expect "
      <RSet Benchmarking>
      making first IntSet: 0.000000 seconds
      making second IntSet: 0.000000 seconds
      making first efficient set: 0.000000 seconds
      making second efficient set: 0.000000 seconds
      naive intersection: 0.000000 seconds
      efficient intersection: 0.000000 seconds
      intersection: true
      naive union: 0.000000 seconds
      efficient union: 0.000000 seconds
      union: true
      naive diff: 0.000000 seconds
      efficient diff: 0.000000 seconds
      diff: true

      </RSet Benchmarking>"]
  ;;
end
