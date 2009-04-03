open Sesame
open Printf
open Tuple

type pt = string * int * int

module IntInt = struct
  type t = int * int
  let compare (lo1,hi1) (lo2,hi2) =
    match Pervasives.compare lo1 lo2 with
      | 0 -> Pervasives.compare hi1 hi2
      | -1 -> -1 | 1 -> 1 | _ -> assert false
end
module Set = Set.Make(IntInt)
type t = Set.t StringMap.t
    
exception Bad of string
let raise_bad msg = raise (Bad msg)

let empty = StringMap.empty

let make_interval (lo,hi) = 
  if lo <= hi then lo,hi
  else raise_bad (sprintf "invalid interval [%d, %d]" lo hi)

let insert (chr,lo,hi) t =
  let x = make_interval (lo,hi) in
  let f x prev =
    match prev with None -> Set.singleton x | Some s -> Set.add x s
  in
  StringMap.add_with chr (f x) t

let mem (chr,lo,hi) t =
  let x = make_interval (lo,hi) in 
  try Set.mem x (StringMap.find chr t)
  with Not_found -> false 

(* Like insert but raise [Bad] if [pt] already in [t]. *)
let insert_no_dup ((chr,lo,hi) as pt) t =
  if mem pt t then raise_bad (sprintf "%s:[%d, %d] is repeated" chr lo hi)
  else insert pt t

let any_overlap t =
  let f _ set ans =
    if ans then ans
    else
      let l = Set.to_list set in
      let l =
        try List.map (Pr.uncurry Range.make) l
        with Range.Bad _ -> failwith (Msg.bug "all intervals should have been well-formed")
      in
      Range.any_overlap l
  in
  StringMap.fold f t false
    
let of_list l = List.fold_left (flip insert) empty l

let to_lists t = StringMap.to_list (StringMap.map Set.to_list t)

let to_list t =
  let f =
    to_lists ->>
    (List.map (fun (chr,l) -> List.map (fun (lo,hi) -> chr,lo,hi) l)) ->>
    List.concat
  in f t

let get_chr chr t =
  try Set.to_list (StringMap.find chr t)
  with Not_found -> []

let of_channel ?(chr_map=identity) ?(increment_lo_hi=(1,0)) cin =
  let inclo,inchi = increment_lo_hi in
  let parse_line line =
    let sl = String.nsplit line "\t" in
    let nth = List.nth sl in
    let nthi = int_of_string <<- nth in
    match List.length sl with
      | 3 ->
          let lo = try nthi 1 with Failure msg -> raise_bad (sprintf "%s is not an int" (nth 1)) in
          let hi = try nthi 2 with Failure msg -> raise_bad (sprintf "%s is not an int" (nth 2)) in
          let lo,hi = make_interval (lo+inclo, hi+inchi) in
          chr_map (nth 0), lo, hi
      | n -> failwith (sprintf "expecting exactly 3 columns but found %d" n)
  in        
  let f ans line = insert (parse_line line) ans in
  try Lines.fold_channel f empty cin
  with Lines.Error (pos,msg) -> raise_bad (Msg.err ~pos msg)
    
let of_file ?(chr_map=identity) ?(increment_lo_hi=(1,0)) file =
  try_finally (of_channel ~chr_map ~increment_lo_hi) close_in (open_in file)
    
let to_channel ?(chr_map=identity) ?(increment_lo_hi=(-1,0)) t cout =
  let inclo,inchi = increment_lo_hi in
  let f chr (lo,hi) = fprintf cout "%s\t%d\t%d\n" chr (lo + inclo) (hi + inchi) in
  let g chr l = Set.iter (f (chr_map chr)) l in
  StringMap.iter g t

let to_file ?(chr_map=identity) ?(increment_lo_hi=(-1,0)) t file =
  try_finally (to_channel ~chr_map ~increment_lo_hi t) close_out (open_out_safe file)

let set_of_rset = 
  RSet.to_range_list ->> (List.map (fun x -> x.Range.lo,x.Range.hi)) ->> Set.of_list

let rset_of_set =
  Set.to_list ->> (List.map (Tuple.Pr.uncurry Range.make)) ->> RSet.of_range_list

let diff bed1 bed2 =
  let f chr bed1_chr_set ans =
    let bed1_chr_set = rset_of_set bed1_chr_set in
    try
      let bed2_chr_set = rset_of_set (StringMap.find chr bed2) in
      StringMap.add chr (RSet.diff bed1_chr_set bed2_chr_set) ans
    with Not_found -> StringMap.add chr bed1_chr_set ans
  in
  let ans = StringMap.fold f bed1 StringMap.empty in
  StringMap.map set_of_rset ans
    
let union bed1 bed2 =
  let f chr bed1_chr_set (ans,bed2) =
    let bed1_chr_set = rset_of_set bed1_chr_set in
    try
      let bed2_chr_set = rset_of_set (StringMap.find chr bed2) in
      let ans = StringMap.add chr (RSet.union bed1_chr_set bed2_chr_set) ans in
      let bed2 = StringMap.remove chr bed2 in
      ans,bed2
    with Not_found -> ans,bed2
  in
  let ans,bed2 = StringMap.fold f bed1 (StringMap.empty,bed2) in
  let ans = StringMap.map set_of_rset ans in
  let ans = StringMap.fold StringMap.add bed2 ans in
  ans
