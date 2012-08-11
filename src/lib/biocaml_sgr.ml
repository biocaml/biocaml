open Biocaml_std

type t = (string * int * float) list
    (* Stored in ascending order by (string,int) pairs. *)

exception Bad of string
let raise_bad msg = raise (Bad msg)
    
let cmpsi (s1,i1,_) (s2,i2,_) = Pervasives.compare (s1,i1) (s2,i2)

let of_list l = List.sort ~cmp:cmpsi l
let to_list t = t

let of_chr_lists l =
  let ans = List.map (fun (a,l) -> List.map (fun (b,c) -> a,b,c) l) l in
  let ans = List.concat ans in
    List.sort ~cmp:cmpsi ans

let to_chr_lists t =
  let eq (s1,_,_) (s2,_,_) = s1 = s2 in
  let ll = List.npartition_exn ~eq t in
  let ll = List.map (fun l -> Tuple.Tr.first (List.hd_exn l), List.map (fun (_,b,c) -> b,c) l) ll in
  ll

let of_channel ?(chr_map=identity) ?(increment_bp=0) cin =
  let parse_line' delim line =
    let sl = String.split line delim in
    let nth = List.nth sl in
    match List.length sl with
      | 3 -> chr_map (nth 0), int_of_string (nth 1) + increment_bp, float_of_string (nth 2)
      | _ -> raise_bad "ill-formed line"
  in
  let parse_line line =
    try parse_line' '\t' line
    with Bad _ ->
      try parse_line' ' ' line
      with Bad msg -> failwith msg
  in
  Lines.of_channel parse_line cin
    
let of_file ?(chr_map=identity) ?(increment_bp=0) file = 
  try_finally_exn (of_channel ~chr_map ~increment_bp) ~fend:close_in (open_in file)

let to_channel ?(chr_map=identity) ?(increment_bp=0) t cout = 
  let f (s,i,v) = fprintf cout "%s\t%d\t%f\n" (chr_map s) (i+increment_bp) v in
  List.iter f t
    
let to_file ?(chr_map=identity) ?(increment_bp=0) t file =
  try_finally_exn (to_channel ~chr_map ~increment_bp t) ~fend:close_out (open_out_safe file) 
 
