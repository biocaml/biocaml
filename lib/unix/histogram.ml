open Core_kernel

type 'a t = {
  cmp: 'a -> 'a -> int; (* comparison function of bin limit type *)
  bin_limits: 'a array;  (* bin limits, length is n+1 for n bins *)
  counts: float array    (* counts, length n for n bins *)
}

let copy hist =
  {cmp = hist.cmp;
   bin_limits = Array.copy hist.bin_limits;
   counts = Array.copy hist.counts}

let make cmp bins =
  let rec is_ordered l =
    match l with
    | [] -> true
    | _::[] -> true
    | x1::x2::l ->
      match cmp x1 x2 with
      | x when x < 0 -> is_ordered (x2::l)
      | _ -> false
  in
  if is_ordered bins then
    Some {cmp=cmp;
          bin_limits = Array.of_list bins;
          counts = Array.create ~len:(List.length bins - 1) 0.0}
  else
    None

let limits_to_bins bl =
  let bl = Array.to_list bl in
  let rec loop ans bl =
    match bl with
    | [] -> failwith "impossible to get here"
    | _::[] -> ans
    | lo::hi::bl -> loop ((lo,hi)::ans) (hi::bl)
  in
  List.rev (loop [] bl)

let to_list hist =
  List.zip_exn (limits_to_bins hist.bin_limits) (Array.to_list hist.counts)

let bin_exn hist k =
  if k >= Array.length hist.counts then
    invalid_argf "invalid bin number %d" k ()
  else
    hist.bin_limits.(k), hist.bin_limits.(k+1)

let bin hist k = try Some (bin_exn hist k) with _ -> None

let count_exn hist k =
  if k >= Array.length hist.counts then
    invalid_argf "invalid bin number %d" k ()
  else
    hist.counts.(k)

let count hist k = Option.try_with (fun () -> count_exn hist k)

let num_bins hist = Array.length hist.counts
let minimum hist = hist.bin_limits.(0)
let maximum hist = hist.bin_limits.(Array.length hist.bin_limits - 1)

let find_bin_index hist x =
  let i = ref (-1) in
  let _ =
    for j = 0 to Array.length hist.bin_limits - 2 do
      let cmp_lo = hist.cmp x hist.bin_limits.(j) in
      let cmp_hi = hist.cmp x hist.bin_limits.(j+1) in
      if (cmp_lo = 1 || cmp_lo = 0) && (cmp_hi = -1) then
        i := j
    done
  in
  if !i >= 0 then Some !i else None

let increment ?(delt=1.0) hist x =
  let hist = copy hist in
  match find_bin_index hist x with
  | None -> hist
  | Some i ->
    hist.counts.(i) <- hist.counts.(i) +. delt;
    hist

let reset hist = {hist with counts = Array.create ~len:(Array.length hist.counts) 0.}

let in_range hist x =
  let cmp_lo = hist.cmp x (minimum hist) in
  let cmp_hi = hist.cmp x (maximum hist) in
    (cmp_lo = 1 || cmp_lo = 0) && (cmp_hi = -1)

let make_uniform min max n =
  if min >= max then
    Error (sprintf "minimum %.3f must be strictly less than maximum %.3f" min max)
  else if n < 1 then
    Error (sprintf "cannot create histogram with %d bins" n)
  else begin
    let delt = (max -. min) /. (Float.of_int n) in
    let bins = Array.init (n+1) ~f:(fun i -> min +. (delt *. Float.of_int i)) in
    bins.(Array.length bins - 1) <- max;
    Result.of_option  (make Pervasives.compare (Array.to_list bins))
      ~error:"not ordered"
  end
