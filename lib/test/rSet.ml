open Core_kernel.Std
module Range = Biocaml_unix.Range
module RSet = Biocaml_unix.RSet
open OUnit

module Test = struct

  let timesf msg f arg =
    let start = Unix.time () in
    let x = f arg in
    let stop = Unix.time () in
    printf "%s: %f seconds\n%!" msg (stop -. start);
    x


end

let make_int_set (l : (int * int) list) : Int.Set.t =
  let f accum (lo,hi) =
    if lo > hi then
      accum
    else
      let v = Range.make_unsafe lo hi in
      Int.Set.union accum (Int.Set.of_list (Range.to_list v))
  in
  List.fold_left ~f ~init:Int.Set.empty l

(** [test ul vl] compares performance and correctness of set intersection and
    union. Sets of type {!IntSet.t} and {!t} are constructed from the given [ul] and
    [vl], and the corresponding intersection and union operations are used on the
    two versions. Messages are printed reporting times required to construct the
    sets, and take their intersection and union. Also, it is verified that the
    operations produce identical results. *)
let test vl1 vl2 =
  let open RSet in
  let intset1 = Test.timesf "making first IntSet" make_int_set vl1 in
  let intset2 = Test.timesf "making second IntSet" make_int_set vl2 in
  let set1 = Test.timesf "making first efficient set" of_range_list vl1 in
  let set2 = Test.timesf "making second efficient set" of_range_list vl2 in

  let is_good intset_op set_op op_name =
    let ans1 = Test.timesf ("naive " ^ op_name) (intset_op intset1) intset2 in
    let ans2 = Test.timesf ("efficient " ^ op_name) (set_op set1) set2 in
    assert_bool op_name (Int.Set.to_list ans1 = to_list ans2)
  in
  is_good Int.Set.inter inter "intersection";
  is_good Int.Set.union union "union";
  is_good Int.Set.diff diff "diff"


(** This function generates random lists and uses them as arguments for [test].
    The state of the [Random] module is not modified. *)
let default_test () =
  let f () = List.init 10000 ~f:(fun _ -> let x = Random.int 100 in x, Random.int 20 + x) in
  Random.init 42 ;
  printf "\n<RSet Benchmarking>\n";
  test (f ()) (f ());
  printf "\n</RSet Benchmarking>\n";
  ()

let tests = "RSet" >::: [
  "default" >:: default_test;
]
