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
