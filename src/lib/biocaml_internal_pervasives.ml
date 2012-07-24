
include Core.Std

module Xmlm = Biocaml_internal_xmlm
  
module Stream = struct
  include Stream

  let next_exn = next
  let next s = try Some (next_exn s) with Stream.Failure -> None

  let lines_of_chars cstr =
    let f _ =
      match peek cstr with
      | None -> None
      | Some _ ->
        let ans = Buffer.create 100 in
        let rec loop () =
          try
            let c = next_exn cstr in
            if c <> '\n' then (Buffer.add_char ans c; loop())
          with Failure -> ()
        in 
        loop();
        Some (Buffer.contents ans)
    in
    from f

  let keep_whilei pred s =
    let f _ =
      match peek s with
      | None -> None
      | Some a ->
        if pred (count s) a
        then (junk s; Some a)
        else None
    in from f
    
  let keep_while pred = keep_whilei (fun _ a -> pred a)
  let truncate k = keep_whilei (fun j _ -> j < k)
  
  let rec skip_whilei pred s =
    match peek s with
    | None -> ()
    | Some a ->
      if pred (count s) a
      then (junk s; skip_whilei pred s)
      else ()

  let skip_while pred = skip_whilei (fun _ a -> pred a)

  let rec fold f accum s =
    match peek s with
      None -> accum
    | Some a -> (junk s; fold f (f accum a) s)

  let to_list t =
    List.rev (fold (fun l b -> b::l) [] t)

  let map f s =
    let f _ =
      try Some (f (next s))
      with Failure -> None
    in from f

  let is_empty s =
    match peek s with None -> true | Some _ -> false

  let lines_of_channel cin =
    let f _ =
      try Some (input_line cin)
      with End_of_file -> None
    in Stream.from f
 
end

