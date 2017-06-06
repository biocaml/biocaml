open Core_kernel

type t = (string * int * float) list
    (* Stored in ascending order by (string,int) pairs. *)

exception Bad of string
let raise_bad msg = raise (Bad msg)

let cmpsi (s1,i1,_) (s2,i2,_) = Pervasives.compare (s1,i1) (s2,i2)

let of_list l = List.sort ~cmp:cmpsi l
let to_list t = t

let of_chr_lists l =
  let ans = List.map ~f:(fun (a,l) -> List.map ~f:(fun (b,c) -> a,b,c) l) l in
  let ans = List.concat ans in
  List.sort ~cmp:cmpsi ans

let npartition_exn ~eq l =
  let open List in
  let insertl ll a =
    let rec loop prefix ll =
      match ll with
        | [] -> rev ([a]::prefix)
        | l::ll ->
          if eq a (hd_exn l)
          then (rev ((a::l)::prefix)) @ ll
          else loop (l::prefix) ll
    in loop [] ll
  in
  map ~f:rev (fold_left ~f:insertl ~init:[] l)

let to_chr_lists t =
  let eq (s1,_,_) (s2,_,_) = s1 = s2 in
  let ll = npartition_exn ~eq t in
  let ll =
    List.map ~f:(fun l ->
      let x, _, _  = List.hd_exn l in
      x, List.map ~f:(fun (_,b,c) -> b,c) l) ll in
  ll

let of_channel ?(chr_map=ident) ?(increment_bp=0) cin =
  let parse_line' delim line =
    match String.split line ~on:delim with
    | [c; i; f] ->
       chr_map c, int_of_string i + increment_bp, Float.of_string f
    | _ -> raise_bad "ill-formed line"
  in
  let parse_line line =
    try parse_line' '\t' line
    with Bad _ ->
      try parse_line' ' ' line
      with Bad msg -> failwith msg
  in
  In_channel.input_lines cin
  |> List.map ~f:parse_line

let of_file ?(chr_map=ident) ?(increment_bp=0) file =
  In_channel.with_file file ~f:(of_channel ~chr_map ~increment_bp)

let to_channel ?(chr_map=ident) ?(increment_bp=0) t cout =
  let f (s,i,v) = fprintf cout "%s\t%d\t%f\n" (chr_map s) (i+increment_bp) v in
  List.iter ~f t

let to_file ?(chr_map=ident) ?(increment_bp=0) t file =
  Out_channel.with_file file ~f:(to_channel ~chr_map ~increment_bp t)
