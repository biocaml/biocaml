open Core

module Stream = struct
  open Stream

  let next_exn = next

  let next s =
    try Some (next_exn s) with
    | Stream.Failure -> None
  ;;

  let is_empty s =
    match peek s with
    | None -> true
    | Some _ -> false
  ;;

  let empty () = from (const None)

  exception Expected_streams_of_equal_length

  let of_list l =
    let lr = ref l in
    let f _ =
      match !lr with
      | h :: t ->
        lr := t;
        Some h
      | [] -> None
    in
    from f
  ;;

  let rec iteri xs ~f =
    match peek xs with
    | Some x ->
      f (count xs) x;
      junk xs;
      iteri xs ~f
    | None -> ()
  ;;

  let iter xs ~f = iteri xs ~f:(const f)

  let rec foldi xs ~init ~f =
    match next xs with
    | None -> init
    | Some x -> foldi xs ~init:(f (count xs - 1) init x) ~f
  ;;

  (* [count xs - 1] because of the call to [next], which increased the
     stream count by one *)

  let fold xs ~init ~f = foldi xs ~init ~f:(const f)

  let take_whilei xs ~f =
    let aux i =
      match peek xs with
      | Some x when f i x ->
        junk xs;
        Some x
      | _ -> None
    in
    from aux
  ;;

  let take_while xs ~f = take_whilei xs ~f:(const f)

  let rec drop_whilei xs ~f =
    match peek xs with
    | Some x when f (count xs) x ->
      junk xs;
      drop_whilei xs ~f
    | _ -> ()
  ;;

  let drop_while xs ~f = drop_whilei xs ~f:(const f)

  let drop xs ~n =
    let i = ref n in
    drop_whilei xs ~f:(fun _ _ ->
      if !i > 0
      then (
        decr i;
        true)
      else false)
  ;;

  let skip xs ~n =
    drop xs ~n;
    xs
  ;;

  let map xs ~f =
    let aux _ = Option.map (next xs) ~f in
    from aux
  ;;

  let mapi2_exn xs ys ~f =
    let aux i =
      match peek xs, peek ys with
      | Some x, Some y ->
        let r = f i x y in
        junk xs;
        junk ys;
        Some r
      | None, None -> None
      | _, _ -> raise Expected_streams_of_equal_length
    in
    from aux
  ;;

  let map2_exn xs ys ~f = mapi2_exn xs ys ~f:(fun _ -> f)

  let filter xs ~f =
    let rec aux i =
      match next xs with
      | Some x when not (f x) -> aux i
      | x -> x
    in
    from aux
  ;;

  let append xs ys =
    let aux _ =
      match next xs with
      | None -> next ys
      | e -> e
    in
    from aux
  ;;

  let concat xs =
    let rec find_next_non_empty_stream xs =
      match peek xs with
      | Some stream when is_empty stream ->
        junk xs;
        find_next_non_empty_stream xs
      | x -> x
    in
    let current = ref (empty ()) in
    let aux _ =
      match next !current with
      | None -> (
        match find_next_non_empty_stream xs with
        | None -> None
        | Some stream ->
          current := stream;
          next stream)
      | x -> x
    in
    from aux
  ;;

  let concat_map l ~f =
    let rec find_next_non_empty_stream xs =
      (* As opposed to concat, we use next here to avoid infinite looping. *)
      match next xs with
      | Some x ->
        let stream = f x in
        if is_empty stream then find_next_non_empty_stream xs else Some stream
      | None -> None
    in
    let current = ref (empty ()) in
    let aux _ =
      match next !current with
      | None -> (
        match find_next_non_empty_stream l with
        | None -> None
        | Some stream ->
          current := stream;
          next stream)
      | x -> x
    in
    from aux
  ;;

  let init n ~f =
    if n < 0
    then empty ()
    else (
      let aux i = if i < n then Some (f i) else None in
      from aux)
  ;;

  let singleton x = init 1 ~f:(const x)
  let to_list t = List.rev (fold ~init:[] ~f:(fun l b -> b :: l) t)

  let result_to_exn s ~error_to_exn =
    from (fun _ ->
      match next s with
      | None -> None
      | Some result -> (
        match result with
        | Ok x -> Some x
        | Result.Error x -> raise (error_to_exn x)))
  ;;

  let unfoldi init ~f =
    let a = ref init in
    from (fun i ->
      match f i !a with
      | Some (b, a_next) ->
        a := a_next;
        Some b
      | None -> None)
  ;;

  let unfold init ~f = unfoldi init ~f:(const f)

  let range ?until n =
    let stop = Option.value_map until ~default:(fun _ -> false) ~f:( < ) in
    unfold n ~f:(fun i -> if stop i then None else Some (i, i + 1))
  ;;

  let of_lazy s =
    let next _ = next (Lazy.force s) in
    from next
  ;;

  (* Default buffer_size set to UNIX_BUFFER_SIZE in OCaml's
   otherlibs/unix/unixsupport.h, but unsure if this is a good
   choice. *)
  let strings_of_channel ?(buffer_size = 65536) inp =
    let buf = Bytes.create buffer_size in
    from (fun _ ->
      match In_channel.input inp ~buf ~pos:0 ~len:buffer_size with
      | 0 -> None
      | len -> Some (Bytes.To_string.sub buf ~pos:0 ~len))
  ;;

  module Result = struct
    let stream_map2_exn = map2_exn
    let stream_fold = fold

    type ('a, 'b) t = ('a, 'b) Result.t Stream.t

    let map2_exn xs ys ~f =
      let f x y =
        match x, y with
        | Ok x, Ok y -> f x y
        | (Error _ as ex), _ -> ex
        | _, (Error _ as ey) -> ey
      in
      stream_map2_exn xs ys ~f
    ;;

    let map2_exn' xs ys ~f =
      let f x y =
        match x, y with
        | Ok x, Ok y -> Ok (f x y)
        | (Error _ as ex), _ -> ex
        | _, (Error _ as ey) -> ey
      in
      stream_map2_exn xs ys ~f
    ;;

    let fold' (type e) rs ~init ~f =
      let module M = struct
        exception E of e
      end
      in
      let f accu = function
        | Ok x -> f accu x
        | Error e -> raise (M.E e)
      in
      try Ok (stream_fold rs ~init ~f) with
      | M.E e -> Error e
    ;;

    let fold (type e) rs ~init ~f =
      let module M = struct
        exception E of e
      end
      in
      let f accu = function
        | Ok x -> (
          match f accu x with
          | Ok r -> r
          | Error e -> raise (M.E e))
        | Error e -> raise (M.E e)
      in
      try Ok (stream_fold rs ~init ~f) with
      | M.E e -> Error e
    ;;
  end
end
