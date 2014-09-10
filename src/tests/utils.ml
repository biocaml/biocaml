open Core.Std

let with_temp_file pre suff ~f =
  let fn = Filename.temp_file pre suff in
  let r = try `Ok (f fn) with e -> `Error e in
  Unix.unlink fn ;
  match r with
  | `Ok y -> y
  | `Error e -> raise e
