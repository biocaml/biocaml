type count_matrix = int array array
type background = float array
type t = float array array

let int_of_char = function
  | 'a' | 'A' -> 0
  | 'c' | 'C' -> 1
  | 'g' | 'G' -> 2
  | 't' | 'T' -> 3
  | _ -> 4
;;

let flat_background () = Caml.Array.make 4 0.25

let background_of_sequence seq pc =
  let counts = Caml.Array.make 4 0
  and n = ref 0 in
  for i = 0 to String.length seq - 1 do
    let k = int_of_char seq.[i] in
    if k < 4
    then (
      counts.(k) <- counts.(k) + 1;
      incr n)
  done;
  Array.map ~f:(fun c -> (float c +. pc) /. (float !n +. (4. *. pc))) counts
;;

let reverse_complement a =
  let open Array in
  let n = length a in
  init n ~f:(fun i ->
    let r = copy a.(n - 1 - i) in
    swap r 0 3;
    swap r 1 2;
    r)
;;

let make mat bg =
  let open Array in
  map mat ~f:(fun p ->
    let n = fold ~f:( + ) ~init:0 p in
    let r = mapi p ~f:(fun i x -> log ((float x +. bg.(i)) /. float n /. bg.(i))) in
    let n_case =
      CFStream.Stream.range 0 ~until:(Array.length p - 1)
      |> CFStream.Stream.fold ~f:(fun accu i -> accu +. (bg.(i) *. r.(i))) ~init:0.
    in
    append r [| n_case |])
;;

let tandem ?(orientation = `direct) ~spacer mat1 mat2 bg =
  Array.concat
    [ (if Poly.(orientation = `everted) then reverse_complement else Fun.id)
        (make mat1 bg)
    ; Array.init spacer ~f:(fun _ -> Caml.Array.make 5 0.)
    ; (if Poly.(orientation = `inverted) then reverse_complement else Fun.id)
        (make mat2 bg)
    ]
;;

let gen_scan f init mat seq tol =
  let r = ref init
  and n = String.length seq
  and m = Array.length mat in
  let seq = Array.init n ~f:(fun i -> int_of_char seq.[i]) in
  for i = n - m downto 0 do
    let score = ref 0. in
    for j = 0 to m - 1 do
      score := !score +. Array.(unsafe_get (unsafe_get mat j) (unsafe_get seq (i + j)))
    done;
    if Float.(!score > tol) then r := f i !score !r
  done;
  !r
;;

let scan = gen_scan (fun pos score l -> (pos, score) :: l) []

let best_hit mat seq =
  let ((pos, _) as r) =
    gen_scan
      (fun p1 s1 ((_, s2) as r2) -> if Float.(s1 > s2) then p1, s1 else r2)
      (-1, Float.neg_infinity)
      mat
      seq
      Float.neg_infinity
  in
  if pos < 0
  then raise (Invalid_argument "Pwm.best_hit: sequence shorter than the matrix")
  else r
;;

external stub_fast_scan
  :  t
  -> int array
  -> float
  -> (int * float) list
  = "biocaml_pwm_scan"

let fast_scan mat seq tol =
  let n = String.length seq in
  let seq = Array.init n ~f:(fun i -> int_of_char seq.[i]) in
  stub_fast_scan mat seq tol
;;

module Test = struct
  let random_dna_char () =
    match Random.int 8 with
    | 0 -> 'a'
    | 1 -> 'A'
    | 2 -> 'c'
    | 3 -> 'C'
    | 4 -> 'g'
    | 5 -> 'G'
    | 6 -> 't'
    | 7 -> 'T'
    | _ -> assert false
  ;;

  let random_dna_string n = String.init n ~f:(fun _ -> random_dna_char ())

  let balmer_freqs =
    [| [| 0.654; 0.045; 0.262; 0.039 |]
     ; [| 0.019; 0.01; 0.935; 0.036 |]
     ; [| 0.042; 0.013; 0.673; 0.272 |]
     ; [| 0.013; 0.074; 0.133; 0.78 |]
     ; [| 0.01; 0.819; 0.113; 0.058 |]
     ; [| 0.893; 0.01; 0.068; 0.029 |]
    |]
  ;;

  let balmer_counts =
    Array.map ~f:(Array.map ~f:(fun f -> Float.to_int (float 309 *. f))) balmer_freqs
  ;;

  let dr5_matrix ?seq () =
    let bg =
      match seq with
      | Some s -> background_of_sequence s 0.1
      | None -> flat_background ()
    in
    tandem ~orientation:`direct ~spacer:5 balmer_counts balmer_counts bg
  ;;

  let%expect_test _ =
    let seq = random_dna_string 10000 in
    let mat = dr5_matrix ~seq () in
    ignore (fast_scan mat seq 10. : _ list);
    print_endline "Doesn't crash";
    [%expect {| Doesn't crash |}]
  ;;

  let%expect_test _ =
    let seq = random_dna_string 100000 in
    let mat = dr5_matrix ~seq () in
    let c_res = fast_scan mat seq (-10.)
    and ocaml_res = scan mat seq (-10.) in
    printf "%s: %b\n" "Hits number" List.(length c_res = length ocaml_res);
    printf
      "%s: %b\n"
      "Same positions"
      List.(Poly.equal (map ~f:fst c_res) (map ~f:fst ocaml_res));
    let eps =
      List.(
        fold2_exn
          ~f:(fun accu (_, x1) (_, x2) -> Float.max accu (Float.abs (x1 -. x2)))
          ~init:0.
          c_res
          ocaml_res)
    in
    printf "%s: %b\n" "Score no more different than eps=1e-4" Float.(eps < 1e-4);
    [%expect
      {|
      Hits number: true
      Same positions: true
      Score no more different than eps=1e-4: true
    |}]
  ;;

  let%expect_test _ =
    let bg = flat_background () in
    let m = make balmer_counts bg in
    let m' = reverse_complement m in
    let m'' = reverse_complement m' in
    let a = (m :> float array array)
    and a' = (m' : t :> float array array)
    and a'' = (m'' : t :> float array array) in
    printf "%s: %b\n" "Reverse complement should be idempotent" (Poly.equal a a'');
    printf
      "%s: %b\n"
      "Wrong permutation of the first element of the matrix"
      Array.(Base.Float.equal a.(0).(0) a'.(length a' - 1).(3));
    [%expect
      {|
      Reverse complement should be idempotent: true
      Wrong permutation of the first element of the matrix: true
    |}]
  ;;

  let%expect_test _ =
    let m = dr5_matrix () in
    let sequences = Array.init 1000 ~f:(fun _ -> random_dna_string 10000) in
    let f s =
      let all_hits = scan m s Float.neg_infinity
      and _, best_hit = best_hit m s in
      let best_of_all_hits =
        List.fold_left
          ~f:(fun accu (_, s) -> Float.max accu s)
          ~init:Float.neg_infinity
          all_hits
      in
      Float.(best_hit = best_of_all_hits)
    in
    printf
      "%s: %b\n"
      "best_hit returns the best position found by scan"
      Array.(for_all ~f sequences);
    [%expect {| best_hit returns the best position found by scan: true |}]
  ;;
end
