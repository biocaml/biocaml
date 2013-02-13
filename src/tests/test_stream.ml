open OUnit
open Biocaml_internal_pervasives
open Biocaml.Stream
open Biocaml.Stream.Infix

let int_list_printer il = String.concat ~sep:"; " (List.map il (sprintf "%d"))
let int_list_tuple_printer (al, bl) =
  sprintf "%s, %s" (int_list_printer al) (int_list_printer bl)

let test_exists () =
  let f x = x mod 2 = 0 in
  assert_bool
    "Find an even number"
    (exists (of_list [ 5 ; 9 ; 11 ; 8 ]) ~f) ;
  assert_bool
    "No even number"
    (not (exists (of_list [ -45 ; 11 ]) ~f))

let test_group () =
  let f x = x mod 2 in
  let groups = 
    [ 1 ; 3 ; 4 ; 5 ; 4 ; 2 ; 9]
    |! of_list
    |! group ~f
    |! map ~f:(to_list)
    |! to_list
  in
  assert_equal ~msg:"Find groups with same parity" groups [ [ 1 ; 3 ] ; [ 4 ] ; [ 5 ] ; [ 4 ; 2 ] ; [ 9 ] ] ;

  (* now the same test but visiting the groups in the reverse order
     (triggers forcing of previous streams) *)
  let groups = 
    [ 1 ; 3 ; 4 ; 5 ; 4 ; 2 ; 9]
    |! of_list
    |! group ~f
    |! to_list
    |! List.rev_map ~f:to_list
    |! List.rev
  in
  assert_equal 
    ~msg:"Find groups with same parity (reverse order)"
    groups [ [ 1 ; 3 ] ; [ 4 ] ; [ 5 ] ; [ 4 ; 2 ] ; [ 9 ] ]

let test_concat () =
  let f x = x mod 2 in
  let l = List.init 100 ~f:(fun _ -> Random.int 10) in
  let m =
    of_list l
    |! group ~f
    |! concat
    |! to_list
  in 
  assert_equal
    ~msg:"Concat grouped enum is idempotent"
    l m

let test_uncombine () =
  let l = [ 1,2; 3,4; 5,6; 7,8 ] in
  let s1, s2 = of_list l |! uncombine in
  let l2 = next_exn s2 :: [] in
  let l2 = next_exn s2 :: l2 in
  let l1 = next_exn s1 :: [] in
  let l1 = next_exn s1 :: l1 in
  let l1 = next_exn s1 :: l1 in
  let l2 = next_exn s2 :: l2 in
  let l2 = next_exn s2 :: l2 in
  let l1 = next_exn s1 :: l1 in
  assert_equal ~printer:int_list_printer ~msg:"Check first list"  (List.rev l1) [ 1 ; 3 ; 5 ; 7 ] ;
  assert_equal ~printer:int_list_printer ~msg:"Check second list" (List.rev l2) [ 2 ; 4 ; 6 ; 8 ]

let test_partition () = 
  let f x = x mod 2 = 0 in
  let l = List.init 100 ~f:(fun _ -> Random.int 10) in
  let r1 = Caml.List.partition f l in
  let r2 =
   of_list l
    |! partition ~f
    |! (fun (a, b) -> (to_list a, to_list b))
  in 
  assert_equal
    ~printer:int_list_tuple_printer
    ~msg:"Check stream partition against list partition"
    r1 r2

let test_iter () =
  let l = [ 1;2;3;4;5;6 ] in
  let c = ref [] in
  let f x = c := x :: !c in
  iter (Stream.of_list l) ~f ;
  assert_equal 
    ~printer:int_list_printer
    ~msg:"Check list built with iter"
    l (List.rev !c)

let test_take () =
  assert_equal 
    ~printer:int_list_printer
    ~msg:"Check take"
    [1;2;3] (to_list (take (of_list [1;2;3;4;5]) 3))

let test_range () =
  assert_equal
    ~printer:int_list_printer
    ~msg:"Check the construction of range enumerations"
    [1;2;3;4;5] (to_list (1 -- 5)) ;
  assert_equal
    ~printer:int_list_printer
    ~msg:"Check the construction of reverse range enumerations"
    [1;2;3;4;5] (to_list (1 --- 5)) ;
  assert_equal
    ~printer:int_list_printer
    ~msg:"Check the construction of reverse range enumerations"
    [5;4;3;2;1] (to_list (5 --- 1)) ;
  assert_equal
    ~printer:string_of_int
    ~msg:"Unbounded range stream should have more than 10 elements"
    10 (range 1 |! (Fn.flip take) 10 |! to_list |! List.length)


let test_scan () =
  let factorials =
    scan (1 -- 10) ~f:( * )
    |! to_list
  in
  assert_equal
    ~printer:int_list_printer
    ~msg:"Check the construction of the first factorials"
    [1; 2; 6; 24; 120; 720; 5040; 40320; 362880; 3628800] factorials

let test_merge () =
  let rnd_list () =
    List.(init 20 (fun _ -> Random.int 1000) |! sort ~cmp:Pervasives.compare)
  in
  let f _ =
    let left = rnd_list () and right = rnd_list () in
    let gold = List.sort ~cmp:Pervasives.compare (left @ right)
    and merged =
      merge (of_list left) (of_list right) ~cmp:Pervasives.compare
      |! to_list
    in
    assert_equal
      ~printer:int_list_printer
      ~msg:"Check merged sorted lists equals sorted concatenated lists"
      gold merged
  in
  ignore (Array.init 10 f)

let test_uniq () =
  assert_equal
    ~printer:int_list_printer
    ~msg:"Check uniq'ed lists (by hand and by [uniq]"
    [ 5;6;3;2;6;7;8 ]
    ([ 5 ; 5 ; 6 ; 3 ; 3; 3 ; 2; 6; 7; 8; 8; 8] |! of_list |! uniq |! to_list)

let test_skip () =
  assert_equal
    ~printer:int_list_printer
    ~msg:"Check [skip]'ed lists (by hand and by [uniq]"
    [ 6;-1;-2;7;8 ]
    ([ -5 ; -5 ; -6 ;6;-1;-2;7;8 ] |! of_list |! Fn.flip skip 3 |! to_list) ;
  assert_equal
    ~printer:int_list_printer
    ~msg:"Check [skip]'ed lists (by hand and by [uniq]"
    [ 6;-1;-2;7;8 ]
    ([ -5 ; -5 ; -6 ;6;-1;-2;7;8 ] |! of_list |! skip_while ~f:(( > ) 0) |! to_list)
  

let tests = "Stream" >::: [
  "Exists" >:: test_exists;
  "Range" >:: test_range;
  "Take" >:: test_take ;
  "Iter" >:: test_iter;
  "Scan" >:: test_scan;
  "Skip" >:: test_skip ;
  "Group" >:: test_group;
  "Concat" >:: test_concat;
  "Uncombine" >:: test_uncombine;
  "Merge" >:: test_merge ;
  "Partition" >:: test_partition ;
  "Uniq" >:: test_uniq ;
]



















