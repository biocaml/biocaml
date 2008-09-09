open TylesBase
open Printf
open Tuple

type pt = string * int * int
type t = (int * int) list StringMap.t

exception Bad of string
let raise_bad msg = raise (Bad msg)

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
    
let to_lists t = List.rev ((StringMap.fold (fun chr l ans -> (chr,l)::ans) t) [])

let to_list t =
  let f =
    to_lists ->>
    (List.map (fun (chr,l) -> List.map (fun (lo,hi) -> chr,lo,hi) l)) ->>
    List.concat
  in f t

let of_line (s:string) : pt =
  let sl = String.nsplit s "\t" in
  let nth = List.nth sl in
  match List.length sl with
    | 3 ->
        let lo = try int_of_string (nth 1) with Failure msg -> raise_bad (sprintf "%s is not an int" (nth 1)) in
        let hi = try int_of_string (nth 2) with Failure msg -> raise_bad (sprintf "%s is not an int" (nth 2)) in
        nth 0, lo, hi
    | n -> raise_bad (sprintf "expecting exactly 3 columns but found %d" n)
        
let to_line (chr,lo,hi) =
  String.concat "\t"
    [chr;
     string_of_int lo;
     string_of_int (hi + 1); (* +1 makes half-open intervals as required *)
    ]
    
let to_channel t cout =
  let f chr (lo,hi) = fprintf cout "%s\n" (to_line (chr,lo,hi)) in
  let g chr l = List.iter (f chr) l in
  StringMap.iter g t

let to_file t file =
  try_finally (to_channel t) close_out (open_out_safe file)


type s = (int * int) list StringMap.t
    
let empty = StringMap.empty
  
let append s (chr,lo,hi) =
  if lo > hi then
    raise_bad (sprintf "invalid interval [%d, %d]" lo hi)
  else
    try
      let l = StringMap.find chr s in
      let (lo',hi') = List.hd l in (* ok because list can never be empty *)
      if hi' >= lo then
        raise_bad (sprintf "interval [%d, %d] cannot be followed by [%d, %d]" lo' hi' lo hi)
      else
        StringMap.add chr ((lo,hi)::l) s
    with
        Not_found -> StringMap.add chr [(lo,hi)] s
          
let singleton = append empty

let complete s =StringMap.map List.rev s
