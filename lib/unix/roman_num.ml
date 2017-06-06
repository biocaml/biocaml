open Core_kernel

module Roman = struct
  (* Roman module courtesy of Nathan Mishra Linger, as posted on Caml List. *)

  (****************************************************************************)
  (* arabic <== roman *)

  exception BadNumeral of char

  let eval = function
    | 'I' ->         1
    | 'V' ->         5
    | 'X' ->        10
    | 'L' ->        50
    | 'C' ->       100
    | 'D' ->       500
    | 'M' ->      1000
    | c   -> raise(BadNumeral(c))

  let arabic (x:string) : int =
    let n = String.length x in
    let rec loop i v p =
      if i < n then
        let c = eval (String.get x i) in
        loop (i+1) (if p < c then v-p else v+p) c
      else v + p
    in
    if n > 0
    then let v = eval (String.get x 0) in loop 1 0 v
    else 0

  (****************************************************************************)
  (* (arabic ==> roman) abstract interpretation on SIZES *)

  let numerals_size = 7

  let digit_size = function
    | 0 -> 0 | 1 -> 1 | 2 -> 2 | 3 -> 3 | 4 -> 2
    | 5 -> 1 | 6 -> 2 | 7 -> 3 | 8 -> 4 | 9 -> 2
    | _ -> assert false

  let rec count_size k = function
    | (0,_) -> k
    | (n,j) ->
      if j >= 3
      then count_size (digit_size (n mod 10) + k) (n/10,j-2)
      else n+k

  let roman_size n =
    if n < 0 then 0 else
      count_size 0 (n,numerals_size)

  (****************************************************************************)
  (* arabic ==> roman *)

  exception Negative

  let numerals = ['I';'V';'X';'L';'C';'D';'M']

  let roman (n:int) : string =
    let size = roman_size n in
    let x = String.make size 'M' in
    let ( ++ ) c k = String.set x k c; k-1 in
    let digit d one five ten k =
      match d with
      | 0 -> k
      | 1 ->  one ++ k
      | 2 ->  one ++ (one ++ k)
      | 3 ->  one ++ (one ++ (one ++ k))
      | 4 ->  one ++ (five ++ k)
      | 5 -> five ++ k
      | 6 -> five ++ (one ++ k)
      | 7 -> five ++ (one ++ (one ++ k))
      | 8 -> five ++ (one ++ (one ++ (one ++ k)))
      | 9 ->  one ++ (ten ++ k)
      | _ -> raise Negative
    in
    let rec count k = function
      | 0,_ -> ()
      | _,[_] -> ()
      | n,(one :: five :: ((ten :: _) as next)) ->
        count (digit (n mod 10) one five ten k) (n/10, next)
      | _ -> assert false
    in
    count (size-1) (n,numerals); x

end

(******************************************************************************)
(* Following is by Ashish Agarwal, which just wraps above functions to
   provide API following Biocaml's conventions. Only real deviation is
   that in above [roman 0] returns empty string instead of an
   error. Below, we explicitly disallow 0.
*)

open Roman

type t = int

let of_roman s =
  try Ok (arabic (String.uppercase s))
  with BadNumeral c -> error "invalid char in Roman numeral" c sexp_of_char

let of_arabic i =
  if i > 0
  then Ok i
  else error "invalid non-positive Roman numeral" i sexp_of_int

let to_roman = roman
let to_arabic = Fn.id
