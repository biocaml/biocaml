open OUnit
open Batteries
open Biocaml_pwm

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
  String.init n (fun _ -> random_dna_char ())

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
    (Array.map (fun f -> int_of_float (float 309 *. f)))
    balmer_freqs

let dr5_matrix seq = 
  let bg = background_of_sequence seq 0.1 in
  tandem ~orientation:`direct ~spacer:5 balmer_counts balmer_counts bg

let test_c_version_doesnt_crash () = 
  let seq = random_dna_string 200 in
  let mat = dr5_matrix seq in
  ignore (stub_scan mat seq 10.)

let tests = "PhredScore" >::: [
  "C version doesn't crash" >:: test_c_version_doesnt_crash;
]
