open Sesame
open Printf

type t = 
    | ChrX | ChrY | ChrM 
    | ChrN of int (* Invariant: int is strictly greater than 0 *)
    | Unknown of string

(* Container for alphabetic suffixes. *)    
module Alpha = struct
  let x = "X"
  let y = "Y"
  let m = "M"
  let mt = "Mt"
  let mtdna = "MtDNA"
  let all = [x;y;m;mt;mtdna]
end

let of_string s =
  try
    let ss = Pcre.extract ~pat:"^(chr|CHR|Chr)?(.*)$" s in
    let _ = assert(Array.length ss = 3) in
    let c = ss.(2) in
      if c = Alpha.x then
        ChrX
      else if c = Alpha.y then
        ChrY
      else if c = Alpha.m || c = Alpha.mt || c = Alpha.mtdna then
        ChrM
      else
        match RomanNum.of_string c with
          | Some n -> ChrN (RomanNum.to_int n)
          | None ->
              try
                let n = int_of_string c in
                  if n > 0 then ChrN n else Unknown s
              with Failure _ -> Unknown s
  with
      Not_found -> Unknown s
        
let chr = "chr"
  
let non_num_to_string = function
  | ChrX -> chr ^ Alpha.x | ChrY -> chr ^ Alpha.y | ChrM -> chr ^ Alpha.m
  | Unknown s -> s
  | ChrN n -> invalid_arg (Msg.bug (sprintf "non_num_to_string called on numeric chromosome %d" n))
      
let to_arabic t =
  match t with
    | ChrX | ChrY | ChrM | Unknown _ -> non_num_to_string t
    | ChrN n -> chr ^ (string_of_int n)
      
let to_roman t =
  match t with
    | ChrX | ChrY | ChrM | Unknown _ -> non_num_to_string t
    | ChrN n ->
        let n = RomanNum.to_string (RomanNum.of_int_exn n) in
          if List.mem n Alpha.all
          then failwith (sprintf "chromosome %s cannot be represented in Roman form" (to_arabic t))
          else chr ^ n
            
let arabic = of_string ->> to_arabic
let roman = of_string ->> to_roman

let chr_map (s:string) : string =
  if String.starts_with s "chr" then
    String.sub s 3 (String.length s - 3)
  else
    s

let short_roman = roman ->> chr_map
