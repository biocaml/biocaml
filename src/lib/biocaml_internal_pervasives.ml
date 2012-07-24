
include Core.Std

let try_finally_exn ~fend f x =
  match try `V (f x) with e -> `E e with
    | `V f_x -> fend x; f_x
    | `E e -> (try fend x with _ -> ()); raise e

let open_out_safe = open_out_gen [Open_wronly; Open_creat; Open_excl; Open_text] 0o666

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

module Lines = struct
  module Pos = Biocaml_pos

  exception Error of (Pos.t * string)
  let raise_error p m = raise (Error(p,m))

  let fold_stream' ?(file="") ?(strict=true) f init cstr =
    let lines = Stream.lines_of_chars cstr in
    let f accum s =
      try f accum s
      with Failure msg ->
        let n = Stream.count lines in
        let pos = if file = "" then Pos.l n else Pos.fl file n in
        if strict then raise_error pos msg else accum
    in
    Stream.fold f init lines

  let fold_stream ?(strict=true) f init cstr =
    fold_stream' ~strict f init cstr

  let fold_channel' ?(file="") ?(strict=true) f init cin =
    fold_stream' ~file ~strict f init (Stream.of_channel cin)

  let fold_channel ?(strict=true) f init cin =
    fold_stream ~strict f init (Stream.of_channel cin)

  let fold_file ?(strict=true) f init file =
    try
      try_finally_exn (fold_channel' ~file ~strict f init)
        ~fend:close_in (open_in file)
    with Error (p,m) -> raise_error (Pos.set_file p file) m

  let iter_file ?(strict=true) f file =
    fold_file ~strict (fun _ x -> f x) () file

  let of_stream ?(strict=true) f (cstr : char Stream.t) =
    let lines = Stream.lines_of_chars cstr in
    let g ans s =
      try (f s)::ans
      with Failure m ->
        if strict
        then raise_error (Pos.l (Stream.count lines)) m
        else ans
    in List.rev (Stream.fold g [] lines)

  let of_channel ?(strict=true) f cin =
    of_stream ~strict f (Stream.of_channel cin)

end
