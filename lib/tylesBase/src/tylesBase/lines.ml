module Stream = Stream2
open Pervasives2

exception Error of (Pos.t * string)
let raise_error p m = raise (Error(p,m))

let fold_stream' ?(file="") ?(strict=true) f init cstr =
  let lines = Stream.lines_of_chars cstr in
  let f accum s =
    try f accum s
    with Failure msg ->
      let n = Stream.count lines in
      let pos = if file = "" then Pos.l n else Pos.fl file n in
      if strict then raise_error pos msg else accum
  in
  Stream.fold f init lines
    
let fold_stream ?(strict=true) f init cstr =
  fold_stream' ~strict f init cstr

let iter_stream ?(strict=true) f cstr =
  fold_stream ~strict (fun _ x -> f x) () cstr
    
let fold_channel' ?(file="") ?(strict=true) f init cin =
  fold_stream' ~file ~strict f init (Stream.of_channel cin)
    
let fold_channel ?(strict=true) f init cin =
  fold_stream ~strict f init (Stream.of_channel cin)
    
let iter_channel ?(strict=true) f cin =
  fold_channel ~strict (fun _ x -> f x) () cin
    
let fold_string ?(strict=true) f init s = 
  fold_stream ~strict f init (Stream.of_string s) 
    
let iter_string ?(strict=true) f s =
  fold_string (fun _ x -> f x) () s

let fold_file ?(strict=true) f init file =
  try try_finally (fold_channel' ~file ~strict f init) close_in (open_in file)
  with Error (p,m) -> raise_error (Pos.set_file p file) m

let fold_file2 ?(strict=true) f init file1 file2 = 
  let ic1 = open_in file1 in
  let ic2 = open_in file2 in
  let acc = ref init in
  let line_num1 = ref 0 in
  let line_num2 = ref 0 in
  try
    while true do
      incr line_num1;
      let line1 = input_line ic1 in
      try
        incr line_num2;
        let line2 = input_line ic2 in
        acc := f !acc line1 line2
      with End_of_file -> raise_error (Pos.fl file2 !line_num2) "second file contains fewer lines than first"
    done;
  with End_of_file -> (
    try 
      ignore (input_line ic2);
      raise_error (Pos.fl file1 !line_num1) "first file contains fewer lines than second"
    with End_of_file -> ()
  );
    close_in ic1;
    close_in ic2;
    !acc

let map_file f infile outfile = 
  let ic = open_in infile in
  let oc = open_out outfile in
  try 
    while true do 
      Printf.fprintf oc "%s\n" (f (input_line ic))
    done; 
    assert false
  with End_of_file -> close_in ic; close_out oc; ()

let iter_file ?(strict=true) f file =
  fold_file ~strict (fun _ x -> f x) () file

let of_stream ?(strict=true) f (cstr : char Stream.t) =
  let lines = Stream.lines_of_chars cstr in
  let g ans s =
    try (f s)::ans
    with Failure m ->
      if strict
      then raise_error (Pos.l (Stream.count lines)) m
      else ans
  in List.rev (Stream.fold g [] lines)
  
let of_channel ?(strict=true) f cin =
  of_stream ~strict f (Stream.of_channel cin)
    
let of_string ?(strict=true) f s = 
  of_stream ~strict f (Stream.of_string s) 

let of_file ?(strict=true) f file =
  try try_finally (of_channel ~strict f) close_in (open_in file)
  with Error (p,m) -> raise_error (Pos.set_file p file) m


let to_channel f cout l =
  let g a = output_string cout (f a); output_char cout '\n' in
    List.iter g l

let to_file f file l =
  try_finally (fun cout -> to_channel f cout l) close_out (open_out_safe file)
    
let to_string f l =
  let ans = Buffer.create (List.length l * 100) in
  let rec loop = function
    | [] -> ()
    | a::l ->
        Buffer.add_string ans (f a);
        Buffer.add_char ans '\n';
        loop l
  in loop l; Buffer.contents ans

let to_stream f l = Stream.of_string (to_string f l)


let copy_channel ?(first=1) ?last cin cout =
  let first = max 1 first in
  let line_num = ref 1 in
  try
    while true do
      let after_first = first <= !line_num in
      let before_last = match last with None -> true | Some last -> !line_num <= last in
      if not (before_last) then raise End_of_file;
      let c = input_char cin in
      if c = '\n' then incr line_num;
      if after_first && before_last then output_char cout c;
    done;
    assert false
  with
      End_of_file -> ()
        
let copy_file ?(first=1) ?last in_file out_file =
  let copy (cin,cout) =
    match last with 
      | None -> copy_channel ~first cin cout
      | Some last -> copy_channel ~first ~last cin cout
  in
  let close (cin,cout) = close_in cin; close_out cout in
  try_finally copy close (open_in in_file, open_out_safe out_file)

