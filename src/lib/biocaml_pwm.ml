open Biocaml_internal_pervasives

type count_matrix = int array array
type background = float array
type t = float array array

let int_of_char = function
  | 'a' | 'A' -> 0
  | 'c' | 'C' -> 1
  | 'g' | 'G' -> 2
  | 't' | 'T' -> 3
  | _ -> 4

let flat_background () = Caml.Array.make 4 0.25

let background_of_sequence seq pc =
  let counts = Caml.Array.make 4 0
  and n = ref 0 in
  for i = 0 to String.length seq - 1 do
    let k = int_of_char seq.[i] in
    if k < 4 then (
      counts.(k) <- counts.(k) + 1 ;
      incr n
    )
  done ;
  Array.map (fun c -> (float c +. pc) /. (float !n +. 4. *. pc)) counts

let swap t i j =
  let tmp = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tmp

let reverse_complement a = Array.(
  let n = length a in
  init
    n
    (fun i -> 
      let r = copy a.(n - 1 - i) in
      swap r 0 3 ;
      swap r 1 2 ;
      r)
)

let make mat bg = Array.(
  map
    (fun p ->
      let n = fold ~f:( + ) ~init:0 p in
      let r = 
	mapi p ~f:(fun i x -> 
	  log ((float x +. bg.(i)) /. float n /. bg.(i))
	)
      in
      let n_case = 
	range p 
        |! Stream.fold ~f:(fun accu i -> accu +. bg.(i) *. r.(i)) ~init:0. in
      append r [| n_case |])
    mat
)

let tandem ?(orientation = `direct) ~spacer mat1 mat2 bg =
  Array.concat [
    (if orientation = `everted then reverse_complement else ident) (make mat1 bg) ;
    Array.init spacer (fun _ -> Caml.Array.make 5 0.) ;
    (if orientation = `inverted then reverse_complement else ident) (make mat2 bg)
  ]


let gen_scan f init mat seq tol = 
  let r = ref init 
  and n = String.length seq 
  and m = Array.length mat in 
  let seq = Array.init n (fun i -> int_of_char seq.[i]) in
  for i = n - m downto 0 do
    let score = ref 0. in
    for j = 0 to m - 1 do
      score := !score +. Array.(unsafe_get (unsafe_get mat j) (unsafe_get seq (i + j)))
    done ;
    if !score > tol 
    then r := f i !score !r
  done ;
  !r

let scan = gen_scan (fun pos score l -> (pos, score) :: l) []

let best_hit mat seq = 
  let (pos, _) as r = 
    gen_scan (fun p1 s1 ((p2,s2) as r2) -> if s1 > s2 then (p1, s1) else r2) (-1, Float.neg_infinity) mat seq Float.neg_infinity
  in
  if pos < 0 then raise (Invalid_argument "Biocaml_pwm.best_hit: sequence shorter than the matrix")
  else r


external stub_fast_scan : t -> int array -> float -> (int * float) list = "biocaml_pwm_scan"

let fast_scan mat seq tol =  
  let n = String.length seq in
  let seq = Array.init n (fun i -> int_of_char seq.[i]) in
  stub_fast_scan mat seq tol














