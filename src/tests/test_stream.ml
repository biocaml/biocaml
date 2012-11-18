open OUnit
open Core.Std
open Printf

open Biocaml_stream

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


let tests = "Stream" >::: [
  "Exists" >:: test_exists;
  "Group" >:: test_group;
]



















