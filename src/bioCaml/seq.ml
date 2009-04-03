open Sesame

type t = string
exception Bad of string
let raise_bad msg = raise (Bad msg)

module Set = Set.Make(struct type t = char let compare = Pervasives.compare end)
let codes = 
  let codes = ['A';'C';'G';'T';'U';'R';'Y';'K';'M';'S';'W';'B';'D';'H';'V';'N';'X'] in
  List.fold_left (flip Set.add) Set.empty codes

let is_nucleic_acid c = Set.mem (Char.uppercase c) codes

let too_long = Msg.err ("sequence length exceeds " ^ (string_of_int Sys.max_string_length))
let bad_acid c = "invalid sequence element " ^ String.of_char c

let of_buffer b =
  let n = Buffer.length b in
  if n > Sys.max_string_length then raise_bad too_long
  else
    let ans = String.create n in
    for i = 0 to n-1 do
      let c = Char.uppercase (Buffer.nth b i) in
      if is_nucleic_acid c then ans.[i] <- c
      else raise_bad (bad_acid (Buffer.nth b i))
    done;
    ans
      
let of_string b =
  let n = String.length b in
  let ans = String.create n in
  for i = 0 to n-1 do
    let c = Char.uppercase b.[i] in
    if is_nucleic_acid c then ans.[i] <- c
    else raise_bad (bad_acid b.[i])
  done;
  ans

let of_buffer_unsafe = String.uppercase <<- Buffer.contents
let of_string_unsafe = String.uppercase <<- String.copy

let to_string = String.copy
let nth t i = String.get t (i-1)
let length = String.length
let fold_left = String.fold_left
let fold_right = String.fold_right

let slice first last t =
  if first < 1 || last > String.length t || first > last then
    failwith "requesting invalid slice from sequence"
  else
    let first = first - 1 in
    String.slice ~first ~last t
