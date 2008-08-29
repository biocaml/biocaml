open Lib2
open Printf

type 'a t =
    {
      length:int; (* length of the array *)
      equal : 'a -> 'a -> bool;
      default:'a; (* default value for items not stored explicitly *)
      table : (int,'a) Hashtbl.t (* the sparse array *)
    }
      (* Invariant: table does not have an entry for an element with default value. *)

let length t = t.length

let get_unsafe t k =
  try Hashtbl.find t.table k
  with Not_found -> t.default

let set_unsafe t k a =
  if t.equal a t.default
  then Hashtbl.remove t.table k
  else Hashtbl.replace t.table k a
      
let index_good t k = k >= 0 && k < t.length

let get t k =
  if index_good t k
  then get_unsafe t k
  else raise (Invalid_argument "index out of bounds")

let set t k a =
  if index_good t k
  then set_unsafe t k a
  else raise (Invalid_argument "index out of bounds")

let make ?(eq = (=)) ?(sparsity=0.9) length default =
  if sparsity < 0.0 || sparsity > 1.0 then failwith (sprintf "sparsity = %F but must be between 0.0 and 1.0" sparsity);
  if length < 0 then failwith (sprintf "cannot create array of length = %d" length);
  {
    length = length;
    equal = eq;
    default = default;
    table = Hashtbl.create (int_of_float ((1. -. sparsity) *. (float_of_int length)))
  }
    
let iter f t = 
  for i = 0 to t.length - 1 do
    f (get_unsafe t i) 
  done
    
let fold_left f init t =
  let rec loop ans i =
    if i >= t.length then
      ans
    else
      loop (f ans (get_unsafe t i)) (i+1)
  in
    loop init 0

let fold_right f init t =
  let rec loop ans i =
    if i < 0 then
      ans
    else
      loop (f ans (get_unsafe t i)) (i-1)
  in
    loop init (t.length - 1)
