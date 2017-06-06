open Core_kernel

module Error = struct
  type t = [
  | `chromosome_ambiguous_in_roman_form of string
  ]
end

exception Error of Error.t

(* Use stronger type t internally. *)
module I = struct
  type t =
  | ChrX | ChrY | ChrM
  | ChrN of int (* int is strictly greater than 0 *)
  | Unknown of string

  let of_string s =
    let s' = String.lowercase s in
    let c =
      if String.(is_prefix s' ~prefix:"chr") then
        String.(sub s' ~pos:3 ~len:(length s' - 3))
      else
        s'
    in
    match c with
    | "x" -> ChrX
    | "y" -> ChrY
    | "m" | "mt" | "mtdna" -> ChrM
    | _ ->
      match Roman_num.of_roman c with
      | Ok n -> ChrN (Roman_num.to_arabic n)
      | Error _ ->
        try
          let n = int_of_string c in
          if n > 0 then ChrN n else Unknown s
        with Failure _ -> Unknown s

  let non_num_to_string = function
    | ChrX -> "X" | ChrY -> "Y" | ChrM -> "M"
    | Unknown s -> s
    | ChrN _ -> assert false

  let to_string_arabic t =
    match t with
    | ChrX | ChrY | ChrM | Unknown _ -> non_num_to_string t
    | ChrN n -> string_of_int n

  let to_string_roman t =
    let ans = match t with
      | ChrX | ChrY | ChrM | Unknown _ -> non_num_to_string t
      | ChrN n -> Roman_num.to_roman (Roman_num.of_arabic n |> ok_exn)
    in
    if List.mem ~equal:String.( = ) ["x"; "y"; "m"; "mt"; "mtdna"] (String.lowercase ans)
    then Result.Error (`chromosome_ambiguous_in_roman_form (to_string_arabic t))
    else Ok ans

end

let to_arabic s = I.(s |> of_string |> to_string_arabic)
let to_roman s = I.(s |> of_string |> to_string_roman)

let to_roman_exn s = match to_roman s with
  | Ok x -> x
  | Result.Error x -> raise (Error x)
