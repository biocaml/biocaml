
include Core.Std

let try_finally_exn ~fend f x =
  match try `V (f x) with e -> `E e with
    | `V f_x -> fend x; f_x
    | `E e -> (try fend x with _ -> ()); raise e

let open_out_safe = open_out_gen [Open_wronly; Open_creat; Open_excl; Open_text] 0o666

let flip f x y = f y x

module Stream = struct
  include Stream
  let next_exn = next
  let next s = try Some (next_exn s) with Stream.Failure -> None

  module type Streamable = sig
    type 'a streamable
    val stream : 'a streamable -> 'a t
    val of_stream : 'a t -> 'a streamable
  end

  type 'a streamable = 'a t
  let stream x = x
  let of_stream x = x

  let iter xs ~f = iter f xs

  let iter2_exn_msg = "Stream.iter2_exn given streams of different lengths"

  let rec iter2_exn a b ~f = match peek a, peek b with
    | Some x, Some y -> (
        junk a; junk b;
        f x y;
        iter2_exn a b ~f
      )
    | None, None -> ()
    | _, _ -> invalid_arg iter2_exn_msg

  let iter2 a b ~f = 
    try iter2_exn a b ~f 
    with Invalid_argument msg when phys_equal msg iter2_exn_msg -> ()
  (* The use of physical equality in this function is to ensure that
     the [Invalid_argument] exceptions indeed comes from the
     iter2_exn function and not the [~f] argument. *)

  let rec exists xs ~f =
    match peek xs with
    | Some x when f x -> true
    | Some _ -> junk xs ; exists xs ~f
    | None -> false

  let rec for_all xs ~f =
    match peek xs with
    | Some x when not (f x) -> false
    | Some _ -> junk xs ; for_all xs ~f
    | None -> true

  let rec fold xs ~init ~f =
    match peek xs with
    | None -> init
    | Some a -> (junk xs; fold xs ~init:(f init a) ~f)

  let reduce xs ~f =
    match next xs with
    | Some init -> fold xs ~init ~f
    | None -> invalid_arg "Biocaml_stream.reduce: stream should contain at least one element"

  let sum = reduce ~f:( + )
  let fsum = reduce ~f:( +. )

  let fold2_exn = assert false
  let fold2 = assert false
  let scanl = assert false
  let scan = assert false
  let iteri = assert false
  let iter2i_exn = assert false
  let iter2 = assert false
  let foldi = assert false
  let fold2i_exn = assert false
  let fold2i = assert false
  let find = assert false
  let find_exn = assert false
  let find_map = assert false
  let take = assert false
  let take_while = assert false
  let take_whilei = assert false
  let skip = assert false
  let skip_while = assert false
  let skip_whilei = assert false
  let drop = assert false
  let span = assert false
  let group = assert false
  let group_by = assert false
  let mapi = assert false
  let append = assert false
  let concat = assert false
  let combine = assert false
  let uncombine = assert false
  let merge = assert false
  let partition = assert false
  let uniq = assert false
  let range = assert false

  let empty = assert false
  let init = assert false
  let singleton = assert false
  let loop = assert false
  let repeat = assert false
  let cycle = assert false

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
  
  let rec drop_whilei xs ~f =
    match peek xs with
    | None -> ()
    | Some a ->
      if f (count xs) a
      then (junk xs; drop_whilei xs ~f)
      else ()

  let drop_while xs ~f = drop_whilei xs ~f:(fun _ a -> f a)

  let to_list t =
    List.rev (fold ~init:[] ~f:(fun l b -> b::l) t)

  let map xs ~f =
    let f _ = Option.map (next xs) ~f in 
    from f

  let filter xs ~f = 
    let rec aux i =
      match next xs with
      | Some x when not (f x) -> aux i
      | x -> x
    in
    from aux

  let filter_map xs ~f = 
    let rec aux i =
      match next xs with
      | Some x -> (
        match f x with
        | None -> aux i
        | x -> x
      )
      | None -> None
    in
    from aux

  let is_empty s =
    match peek s with None -> true | Some _ -> false

  let lines_of_channel cin =
    let f _ =
      try Some (input_line cin)
      with End_of_file -> None
    in Stream.from f

  let result_to_exn s ~error_to_exn =
    from (fun _ ->
      match next s with
        | None -> None
        | Some result -> match result with
            | Ok x -> Some x
            | Result.Error x -> raise (error_to_exn x)
    )

  module Infix = struct
    let ( -- ) x y = range x ~until:y

    let ( --. ) (a, step) b =
      let n = Int.of_float ((b -. a) /. step) + 1 in
      if n < 0 then
        empty ()
      else
        init n (fun i -> Float.of_int i *. step +. a)

    let ( --^ ) x y = range x ~until:(y-1)

    let ( --- ) x y = 
      if x <= y then x -- y
      else loop x (fun _ prev -> if prev <= y then Some (prev - 1) else None)
       
    let ( /@ ) x f = map x ~f
    let ( // ) x f = filter x ~f
    let ( //@ ) x f = filter_map x ~f
  end

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
    Stream.fold ~f ~init lines

  let fold_stream ?(strict=true) f init cstr =
    fold_stream' ~strict f init cstr

  let fold_channel' ?(file="") ?(strict=true) f init cin =
    fold_stream' ~file ~strict f init (Stream.of_channel cin)

  let fold_channel ?(strict=true) f init cin =
    fold_stream ~strict f init (Stream.of_channel cin)

  let fold_file ?(strict=true) f init file =
    try
      try_finally_exn (fold_channel' ~file ~strict f init)
        ~fend:In_channel.close (open_in file)
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
    in List.rev (Stream.fold ~f:g ~init:[] lines)

  let of_channel ?(strict=true) f cin =
    of_stream ~strict f (Stream.of_channel cin)

end

module With_result = struct

  include Result
    
  let while_ok (type error) l ~(f:(int -> 'a -> ('b, error) Result.t)) =
    let module M = struct
      exception E of error 
      let the_fun () =
        let run () =
          List.mapi l (fun i x ->
            match f i x with
            | Ok o -> o
            | Error e -> raise (E e))
        in
        try Ok (run ())
        with
        | E e -> Error e
    end in
    M.the_fun ()
    
  let output_result r = `output r
  let output_ok o = `output (Ok o)
  let output_error e = `output (Error e)

end
  
module Url = struct

  let escape s =
    let b = Buffer.create (String.length s) in
    String.iter s (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' as c -> Buffer.add_char b c
    | anyother -> Buffer.add_string b (sprintf "%%%02X" (Char.to_int anyother)));
    Buffer.contents b

  let unescape s ~error =
    (* ?(error= fun s -> `wrong_url_string s) s =; *)
    let buf = Buffer.create (String.length s) in
    let rec loop pos = 
      match String.lfindi s ~pos ~f:(fun _ c -> (=) '%' c) with
      | None ->
        Buffer.add_substring buf s pos String.(length s - pos)
      | Some idx ->
        if String.length s >= idx + 2 then (
          let char = Scanf.sscanf (String.sub s (idx + 1) 2) "%x" ident in
          Buffer.add_substring buf s pos String.(idx - pos);
          Buffer.add_char buf (Char.of_int_exn char);
          loop (idx + 3)
        ) else (
          failwith "A"
        )
  in
    try loop 0; Ok (Buffer.contents buf) with
    | e -> Error (error s)
      
end

module Parse = struct

  let escapable_string s ~stop_before =
    let try_escaped s =
      try Some (Scanf.sscanf s "%S%n" (fun s n -> (s,n))) with e -> None in
    let lgth_s = String.length s in
    begin match try_escaped s with
    | Some (found, chars_read) ->
      if chars_read < lgth_s then (
        if List.exists stop_before ((=) s.[chars_read]) then
          (found, Some s.[chars_read],
           String.slice s (chars_read + 1) (String.length s))
        else
          (found, None, String.slice s chars_read (String.length s))
      ) else
        (found, None, "")
    | None ->
      begin match String.lfindi s ~f:(fun _ c -> List.exists stop_before ((=) c)) with
      | Some idx ->
        (String.sub s 0 idx, Some s.[idx],
         String.slice s (idx + 1) (String.length s))
      | None -> (s, None, "")
      end
    end

end


module Order = struct
  let compose cmp1 cmp2 a b =
    let c1 = cmp1 a b in
    let c2 = cmp2 a b in
    let num_true = List.fold_left ~f:(fun cnt b -> if b then cnt+1 else cnt) ~init:0 in
    let bl = [c1 = Some (-1); c1 = Some 1; c2 = Some (-1); c2 = Some 1] in
    assert((num_true bl <= 1) || (c1 = Some 0 && c2 = Some 0));
    match c1 with
    | Some c1 -> c1
    | None ->
      match c2 with
      | Some c2 -> c2
      | None -> invalid_arg "neither partial order given relates given values"
        
  let reverse cmp a b = -(cmp a b)
  let reversep cmp a b = Option.map ~f:(~-) (cmp a b)
    
  let totalify cmp =
    fun a b ->
      match cmp a b with
      | Some c -> c
      | None -> failwith "order relation not defined for given elements"
end

  
module Debug = struct

  let debugged = ref []

  let enable s =
    debugged := s :: !debugged
  let disable s =
    debugged := List.filter !debugged ((<>) s)

  let is_enabled s =
    List.mem !debugged s
      
  let make prefix fmt =
    ksprintf (fun s ->
      if is_enabled prefix then (
        eprintf "%s: " prefix;
        String.iter s (function
        | '\n' -> eprintf "\n    "
        | c -> eprintf "%c" c);
        eprintf "\n%!"
      )
    ) fmt


end
