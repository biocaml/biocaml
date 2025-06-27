let test_data_path = "../etc/test_data"
let test_file fn = Filename.concat test_data_path fn

let with_temp_file pre suff ~f =
  let fn = Filename_unix.temp_file pre suff in
  let r =
    try `Ok (f fn) with
    | e -> `Error e
  in
  Core_unix.unlink fn;
  match r with
  | `Ok y -> y
  | `Error e -> raise e
;;

module Printer = struct
  let string = Fun.id
  let int = string_of_int

  let option f x =
    match x with
    | None -> "None"
    | Some y -> sprintf "Some (%s)" (f y)
  ;;

  let or_error f x =
    match x with
    | Ok y -> sprintf "Ok (%s)" (f y)
    | Error e -> sprintf "Error (%s)" (Error.to_string_hum e)
  ;;
end
