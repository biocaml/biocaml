module Field (* field parsing *) = struct
  type 'a result = ('a, string) Result.t
  type 'a parser = string -> 'a result

  open Result.Monad_infix

  let exn_validate f x msg =
    try Ok (f x) with
    | _ -> Error msg
  ;;

  let int s = exn_validate Int.of_string s "Expected integer value"

  let positive_int s =
    int s >>= fun i -> if i >= 0 then Ok i else Error "Expected positive integer value"
  ;;

  let bounded_int ~lo ~hi s =
    int s
    >>= fun i ->
    if i >= lo && i <= hi
    then Ok i
    else (
      let msg = Printf.sprintf "Expected integer value between %d and %d" lo hi in
      Error msg)
  ;;

  let string_with_no_sep s =
    if String.contains s '\n' || String.contains s '\t'
    then (
      let msg = "Expected string without tab or newline character" in
      Error msg)
    else Ok s
  ;;

  let parse ?(ctx = "<unknown>") f s =
    match f s with
    | Ok _ as r -> r
    | Error msg ->
      let msg = Printf.sprintf "Field %s is incorrect: %s" ctx msg in
      Error msg
  ;;

  let parse_all ?ctx f xs = parse ?ctx (fun xs -> Result.all (List.map xs ~f)) xs
end
