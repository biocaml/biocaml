open TylesBase
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

let string_to_pt ?(increment_high=(-1)) (s:string) : pt =
  let sl = String.nsplit s "\t" in
  let nth = List.nth sl in
  match List.length sl with
    | 3 ->
        let lo = try int_of_string (nth 1) with Failure msg -> raise_bad (sprintf "%s is not an int" (nth 1)) in
        let hi = try int_of_string (nth 2) with Failure msg -> raise_bad (sprintf "%s is not an int" (nth 2)) in
        let hi = hi + increment_high in
        let lo,hi = make_interval (lo,hi) in
        nth 0, lo, hi
    | n -> raise_bad (sprintf "expecting exactly 3 columns but found %d" n)
        
let of_channel ?(increment_high=(-1)) cin =
  let f ans line =
    try insert_no_dup (string_to_pt ~increment_high line) ans
    with Bad msg -> failwith msg
  in
  try Lines.fold_channel f empty cin
  with Lines.Error (pos,msg) -> raise_bad (Msg.err ~pos msg)

let of_file ?(increment_high=(-1)) file =
  try_finally (of_channel ~increment_high) close_in (open_in file)

let pt_to_string ?(increment_high=1) (chr,lo,hi) =
  String.concat "\t"
    [chr;
     string_of_int lo;
     string_of_int (hi + increment_high)
    ]
    
let to_channel ?(increment_high=1) t cout =
  let f chr (lo,hi) = fprintf cout "%s\n" (pt_to_string ~increment_high (chr,lo,hi)) in
  let g chr l = Set.iter (f chr) l in
  StringMap.iter g t

let to_file ?(increment_high=1) t file =
  try_finally (to_channel ~increment_high t) close_out (open_out_safe file)



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

