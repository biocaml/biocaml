open Core_kernel

let with_temp_file pre suff ~f =
  let fn = Filename.temp_file pre suff in
  let r = try `Ok (f fn) with e -> `Error e in
  Unix.unlink fn ;
  match r with
  | `Ok y -> y
  | `Error e -> raise e

module Printer = struct

  let string = ident
  let int = string_of_int
  let option f x = match x with
    | None -> "None"
    | Some y -> sprintf "Some (%s)" (f y)

  let or_error f x = match x with
    | Ok y -> sprintf "Ok (%s)" (f y)
    | Error e -> sprintf "Error (%s)" (Error.to_string_hum e)
end
