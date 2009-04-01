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

let of_list l = List.fold_left (fun ans pt -> insert_no_dup pt ans) empty l

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

let string_to_pt ?(chr_map=identity) ?(increment_lo_hi=(1,0)) (s:string) =
  let sl = String.nsplit s "\t" in
  let nth = List.nth sl in
  match List.length sl with
    | 3 ->
        let lo = try int_of_string (nth 1) with Failure msg -> raise_bad (sprintf "%s is not an int" (nth 1)) in
        let hi = try int_of_string (nth 2) with Failure msg -> raise_bad (sprintf "%s is not an int" (nth 2)) in
        let inclo,inchi = increment_lo_hi in
        let lo,hi = lo + inclo, hi + inchi in
        let lo,hi = make_interval (lo,hi) in
        chr_map (nth 0), lo, hi
    | n -> raise_bad (sprintf "expecting exactly 3 columns but found %d" n)
        
let of_channel ?(chr_map=identity) ?(increment_lo_hi=(1,0)) cin =
  let f ans line =
    try insert_no_dup (string_to_pt ~chr_map ~increment_lo_hi line) ans
    with Bad msg -> failwith msg
  in
  try Lines.fold_channel f empty cin
  with Lines.Error (pos,msg) -> raise_bad (Msg.err ~pos msg)

let of_file ?(chr_map=identity) ?(increment_lo_hi=(1,0)) file =
  try_finally (of_channel ~chr_map ~increment_lo_hi) close_in (open_in file)

let pt_to_string ?(chr_map=identity) ?(increment_lo_hi=(-1,0)) (chr,lo,hi) =
  let inclo,inchi = increment_lo_hi in
  String.concat "\t"
    [chr;
    string_of_int (lo + inclo);
    string_of_int (hi + inchi)
    ]
    
let to_channel ?(chr_map=identity) ?(increment_lo_hi=(-1,0)) t cout =
  let f chr (lo,hi) = fprintf cout "%s\n" (pt_to_string ~chr_map ~increment_lo_hi (chr,lo,hi)) in
  let g chr l = Set.iter (f chr) l in
  StringMap.iter g t

let to_file ?(chr_map=identity) ?(increment_lo_hi=(-1,0)) t file =
  try_finally (to_channel ~chr_map ~increment_lo_hi t) close_out (open_out_safe file)

let set_of_rset x = (RSet.to_range_list ->> (List.map (fun l -> l.Range.lo,l.Range.hi)) ->> (List.fold_left (fun acc l -> Set.add l acc) Set.empty)) x

let rset_of_set x = (Set.elements ->> (List.map (fun (a,b) -> Range.make a b)) ->> RSet.of_range_list) x

let rset_function_on_twobeds rset_f bed1 bed2 = 
  let beds = List.map (StringMap.map rset_of_set) [bed1;bed2] in
  let chrs = 
    let f chr elem acc = chr::acc in
    let chrs = StringMap.fold f (List.nth beds 0) [] in
    assert (chrs = (StringMap.fold f (List.nth beds 1) []));
    chrs
  in
  let f acc chr = 
    let rsets = List.map (StringMap.find chr) beds in
    StringMap.add chr (rset_f (List.nth rsets 0) (List.nth rsets 1)) acc
  in
  let ans = List.fold_left f StringMap.empty chrs in
  StringMap.map set_of_rset ans
  
let diff bed1 bed2 = rset_function_on_twobeds RSet.diff bed1 bed2
    
let union bed1 bed2 = rset_function_on_twobeds RSet.union bed1 bed2

(*      
let validate t =
  let rec validate l =
    match l with
      | [] | _::[] -> ()
      | (lo1,hi1)::((lo2,hi2) as x)::l ->
          if lo1 > hi1 then raise_bad (sprintf "invalid interval [%d, %d]" lo1 hi1)
          else if lo2 > hi2 then raise_bad (sprintf "invalid interval [%d, %d]" lo2 hi2)
          else if hi1 >= lo2 then raise_bad (sprintf "interval [%d, %d] cannot be followed by [%d, %d]" lo1 hi1 lo2 hi2)
          else validate (x::l)
  in
  StringMap.iter (fun _ l -> validate l) t

let of_list l =
  let ll = List.npartition (fun (chr1,_,_) (chr2,_,_) -> chr1 = chr2) l in
  let cmp (lo1,_) (lo2,_) = Pervasives.compare lo1 lo2 in
  let f l = Tr.prj1 (List.hd l), List.sort ~cmp (List.map Tr.prj23 l) in
  let ll = List.map f ll in
  let ans = List.fold_left (fun t (chr,l) -> StringMap.add chr l t) StringMap.empty ll in
  validate ans;
  ans      
*)

