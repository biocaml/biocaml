open Core_kernel

let msg ?(pre="MSG") ?pos msg =
  match pos with
    | None -> pre ^ ": " ^ msg
    | Some p -> pre ^ "[" ^ Pos.to_string p ^ "] " ^ msg

let err = msg ~pre:"ERROR"
let warn = msg ~pre:"WARNING"
let bug = msg ~pre:"BUG"

let print_msg ?pre ?pos m =
  print_endline(
    match pre,pos with
      | (None, None) -> msg m
      | (Some pre, None) -> msg ~pre m
      | (None, Some pos) -> msg ~pos m
      | (Some pre, Some pos) -> msg ~pre ~pos m
  )

let print_err = print_msg ~pre:"ERROR"
let print_warn = print_msg ~pre:"WARNING"
let print_bug = print_msg ~pre:"BUG"

let max_array_length_error = "Out of memory, possibly because trying to construct array of size greater than " ^ (string_of_int Array.max_length)

module Tree = struct
  type t = T of string * t list

  let leaf msg = T(msg,[])

  let add_child (T(msg,childs)) x = T(msg, childs @ [x])

  let to_string t =
    let rec loop depth (T(msg,sub_msgs)) =
      let pre = String.make (2*depth) ' ' in
      let msg = pre ^ msg in
      if List.length sub_msgs = 0
      then msg
      else msg ^ "\n" ^ (String.concat ~sep:"\n" (List.map ~f:(loop (depth+1)) sub_msgs)) 
    in loop 0 t
end
  
