open Core_kernel
module Pwm = Biocaml_unix.Pwm
open OUnit
open Pwm

let random_dna_char () = match Random.int 8 with
    0 -> 'a'
  | 1 -> 'A'
  | 2 -> 'c'
  | 3 -> 'C'
  | 4 -> 'g'
  | 5 -> 'G'
  | 6 -> 't'
  | 7 -> 'T'
  | _ -> assert false

let random_dna_string n =
  String.init n ~f:(fun _ -> random_dna_char ())

let balmer_freqs = [|
  [| 0.654 ; 0.045 ; 0.262 ; 0.039 |] ;
  [| 0.019 ; 0.01  ; 0.935 ; 0.036 |] ;
  [| 0.042 ; 0.013 ; 0.673 ; 0.272 |] ;
  [| 0.013 ; 0.074 ; 0.133 ; 0.78  |] ;
  [| 0.01  ; 0.819 ; 0.113 ; 0.058 |] ;
  [| 0.893 ; 0.01  ; 0.068 ; 0.029 |]
|]

let balmer_counts =
  Array.map
    ~f:(Array.map ~f:(fun f -> Float.to_int (float 309 *. f)))
    balmer_freqs

let dr5_matrix ?seq () =
  let bg = match seq with
  | Some s -> background_of_sequence s 0.1
  | None -> flat_background () in
  tandem ~orientation:`direct ~spacer:5 balmer_counts balmer_counts bg

let test_c_version_doesnt_crash () =
  let seq = random_dna_string 10000 in
  let mat = dr5_matrix ~seq ()in
  ignore (fast_scan mat seq 10.)

let test_c_and_caml_versions_agree () =
  let seq = random_dna_string 100000 in
  let mat = dr5_matrix ~seq () in
  let c_res = fast_scan mat seq (-10.)
  and ocaml_res = scan mat seq (-10.) in
  assert_bool "Hits number" List.(length c_res = length ocaml_res) ;
  assert_bool "Same positions" List.(map ~f:fst c_res = map ~f:fst ocaml_res) ;
  let eps =
    List.(fold2_exn
	    ~f:(fun accu (_,x1) (_,x2) -> Pervasives.max accu (Float.abs (x1 -. x2)))
	    ~init:0. c_res ocaml_res)
  in assert_bool "Score no more different than eps=1e-4" (eps < 1e-4)

let test_reverse_complement () =
  let bg = flat_background () in
  let m = make balmer_counts bg in
  let m' = reverse_complement m in
  let m'' = reverse_complement m' in
  let a = (m :> float array array)
  and a' = (m' : t :> float array array)
  and a'' = (m'' : t :> float array array) in
  assert_bool
    "Reverse complement should be idempotent"
    (a = a'') ;
  assert_bool
    "Wrong permutation of the first element of the matrix"
    Array.(a.(0).(0) = a'.(length a' - 1).(3))

let test_best_hit () =
  let m = dr5_matrix () in
  let sequences = Array.init 1000 ~f:(fun _ -> random_dna_string 10000) in
  let f s =
    let all_hits = scan m s Float.neg_infinity
    and _, best_hit = best_hit m s in
    let best_of_all_hits = List.fold_left ~f:(fun accu (_, s) -> max accu s) ~init:Float.neg_infinity all_hits in
    best_hit = best_of_all_hits
  in
  assert_bool
    "best_hit returns the best position found by scan"
    Array.(for_all ~f sequences)

let tests = "PhredScore" >::: [
  "C version doesn't crash" >:: test_c_version_doesnt_crash;
  "C/OCaml versions agree" >:: test_c_and_caml_versions_agree;
  "Reverse-complement test" >:: test_reverse_complement ;
  "best_hit and scan give coherent results" >:: test_best_hit;
]
