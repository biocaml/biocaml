open Core

module Stream = struct
  include Stream

  let next_exn = next

  let next s =
    try Some (next_exn s) with
    | Stream.Failure -> None
  ;;

  let npeek s n = npeek n s

  let is_empty s =
    match peek s with
    | None -> true
    | Some _ -> false
  ;;

  let empty () = from (const None)
  let to_stream x = x
  let of_stream x = x

  exception Expected_streams_of_equal_length
  exception Premature_end_of_input

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

  let rec iter2i_exn xs ys ~f =
    match peek xs, peek ys with
    | Some x, Some y ->
      f (count xs) (count ys) x y;
      junk xs;
      junk ys;
      iter2i_exn xs ys ~f
    | None, None -> ()
    | _, _ -> raise Expected_streams_of_equal_length
  ;;

  let iter2_exn xs ys ~f = iter2i_exn xs ys ~f:(const (const f))

  let iter2i a b ~f =
    try iter2i_exn a b ~f with
    | Expected_streams_of_equal_length -> ()
  ;;

  let iter2 a b ~f =
    try iter2_exn a b ~f with
    | Expected_streams_of_equal_length -> ()
  ;;

  let rec find_map xs ~f =
    match next xs with
    | Some x -> (
      match f x with
      | Some _ as y -> y
      | None -> find_map xs ~f)
    | None -> None
  ;;

  let find xs ~f = find_map xs ~f:(fun x -> if f x then Some x else None)

  let find_exn xs ~f =
    match find xs ~f with
    | Some x -> x
    | None -> raise Caml.Not_found
  ;;

  let exists xs ~f =
    match find xs ~f with
    | Some _ -> true
    | None -> false
  ;;

  let rec for_all xs ~f =
    match next xs with
    | Some x when not (f x) -> false
    | Some _ -> for_all xs ~f
    | None -> true
  ;;

  let rec foldi xs ~init ~f =
    match next xs with
    | None -> init
    | Some x -> foldi xs ~init:(f (count xs - 1) init x) ~f
  ;;

  (* [count xs - 1] because of the call to [next], which increased the
     stream count by one *)

  let fold xs ~init ~f = foldi xs ~init ~f:(const f)

  let reduce xs ~f =
    match next xs with
    | Some init -> fold xs ~init ~f
    | None -> invalid_arg "Stream.reduce: stream should contain at least one element"
  ;;

  let sum = reduce ~f:( + )
  let fsum = reduce ~f:( +. )

  let rec fold2i_exn xs ys ~init ~f =
    match next xs, next ys with
    | Some x, Some y ->
      let init = f (count xs - 1) (count ys - 1) init x y in
      (* decrease by one because of the calls to [next] *)
      fold2i_exn xs ys ~init ~f
    | None, None -> init
    | _ -> raise Expected_streams_of_equal_length
  ;;

  let fold2_exn xs ys ~init ~f = fold2i_exn xs ys ~init ~f:(const (const f))

  let rec fold2i xs ys ~init ~f =
    match next xs, next ys with
    | Some x, Some y ->
      let init = f (count xs - 1) (count ys - 1) init x y in
      fold2i xs ys ~init ~f
    | _ -> init
  ;;

  let fold2 xs ys ~init ~f = fold2i xs ys ~init ~f:(const (const f))

  let scanl xs ~init ~f =
    let current = ref init in
    let f i =
      if i = 0
      then Some init
      else (
        match next xs with
        | Some x ->
          current := f !current x;
          Some !current
        | None -> None)
    in
    from f
  ;;

  let scan xs ~f =
    match next xs with
    | Some init -> scanl xs ~init ~f
    | None -> empty ()
  ;;

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
  let take xs ~n = take_whilei xs ~f:(fun j _ -> j < n)

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

  let skip_whilei xs ~f =
    drop_whilei xs ~f;
    xs
  ;;

  let skip_while xs ~f = skip_whilei xs ~f:(const f)

  let skip xs ~n =
    drop xs ~n;
    xs
  ;;

  let span xs ~f =
    (*Two possibilities: either the tail has been read
      already -- in which case all head data has been
      copied onto the queue -- or the tail hasn't been
      read -- in which case, stuff should be read from
      [xs] *)
    let queue = Queue.create ()
    and read_from_queue = ref false in
    let head _ =
      if !read_from_queue
      then
        (* Everything from the head has been copied *)
        Queue.dequeue queue
        (* to the queue already                     *)
      else (
        match peek xs with
        | Some x as e when f x ->
          junk xs;
          e
        | _ -> None)
    and tail _ =
      if not !read_from_queue
      then (
        (*Copy everything to the queue         *)
        read_from_queue := true;
        let rec aux () =
          match peek xs with
          | Some x when f x ->
            Queue.enqueue queue x;
            aux ()
          | e -> e
        in
        aux ())
      else next xs
    in
    from head, from tail
  ;;

  let group_aux xs map eq =
    let prev_group_force = ref ignore in
    let for_each_group _ =
      !prev_group_force ();
      match next xs with
      | None -> None
      | Some x ->
        let queue = Queue.create ()
        and forced = ref false
        and mapped_x = map x in
        let aux i =
          if i = 0
          then Some x
          else if !forced
          then Queue.dequeue queue
          else (
            match peek xs with
            | Some y as e when eq (map y) mapped_x ->
              junk xs;
              e
            | _ -> None)
        in
        let force () =
          forced := true;
          let rec loop () =
            match peek xs with
            | Some y when eq (map y) mapped_x ->
              junk xs;
              Queue.enqueue queue y;
              loop ()
            | _ -> ()
          in
          loop ()
        in
        prev_group_force := force;
        Some (from aux)
    in
    from for_each_group
  ;;

  let group xs ~f = group_aux xs f Poly.( = )
  let group_by xs ~eq = group_aux xs Fn.id eq

  let chunk2 xs =
    from (fun _ ->
      match next xs with
      | None -> None
      | Some a -> (
        match next xs with
        | None -> raise Premature_end_of_input
        | Some b -> Some (a, b)))
  ;;

  let chunk3 xs =
    from (fun _ ->
      match next xs with
      | None -> None
      | Some a -> (
        match next xs with
        | None -> raise Premature_end_of_input
        | Some b -> (
          match next xs with
          | None -> raise Premature_end_of_input
          | Some c -> Some (a, b, c))))
  ;;

  let chunk4 xs =
    from (fun _ ->
      match next xs with
      | None -> None
      | Some a -> (
        match next xs with
        | None -> raise Premature_end_of_input
        | Some b -> (
          match next xs with
          | None -> raise Premature_end_of_input
          | Some c -> (
            match next xs with
            | None -> raise Premature_end_of_input
            | Some d -> Some (a, b, c, d)))))
  ;;

  let mapi xs ~f =
    let aux i = Option.map (next xs) ~f:(f i) in
    from aux
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

  let filter_map xs ~f =
    let rec aux i =
      match next xs with
      | Some x -> (
        match f x with
        | None -> aux i
        | x -> x)
      | None -> None
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

  let combine (xs, ys) =
    let aux _ =
      match peek xs, peek ys with
      | Some x, Some y ->
        junk xs;
        junk ys;
        Some (x, y)
      | _ -> None
    in
    from aux
  ;;

  let uncombine xs =
    let whosfirst = ref `left
    and lq = Queue.create ()
    and rq = Queue.create () in
    let rec left i =
      match !whosfirst with
      | `left -> (
        match next xs with
        | None -> None
        | Some (l, r) ->
          Queue.enqueue rq r;
          Some l)
      | `right -> (
        match Queue.dequeue lq with
        | None ->
          whosfirst := `left;
          left i
        | x -> x)
    and right i =
      match !whosfirst with
      | `right -> (
        match next xs with
        | None -> None
        | Some (l, r) ->
          Queue.enqueue lq l;
          Some r)
      | `left -> (
        match Queue.dequeue rq with
        | None ->
          whosfirst := `right;
          right i
        | x -> x)
    in
    from left, from right
  ;;

  let merge xs ys ~cmp =
    let aux _ =
      match peek xs, peek ys with
      | (Some x as ex), Some y when cmp x y <= 0 ->
        junk xs;
        ex
      | Some _, (Some _ as ey) ->
        junk ys;
        ey
      | (Some _ as ex), None ->
        junk xs;
        ex
      | None, (Some _ as ey) ->
        junk ys;
        ey
      | None, None -> None
    in
    from aux
  ;;

  let partition xs ~f =
    let pos_queue = Queue.create ()
    and neg_queue = Queue.create () in
    let rec pos i =
      match Queue.dequeue pos_queue with
      | None -> (
        match next xs with
        | Some x when not (f x) ->
          Queue.enqueue neg_queue x;
          pos i
        | e -> e)
      | e -> e
    and neg i =
      match Queue.dequeue neg_queue with
      | None -> (
        match next xs with
        | Some x when f x ->
          Queue.enqueue pos_queue x;
          neg i
        | e -> e)
      | e -> e
    in
    from pos, from neg
  ;;

  let uniq xs =
    match peek xs with
    | None -> empty ()
    | Some first ->
      let prev = ref first in
      let rec aux i =
        if i = 0
        then Some first
        else (
          match next xs with
          | None -> None
          | Some x ->
            if Poly.(x = !prev)
            then aux i
            else (
              prev := x;
              Some x))
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

  let of_array a =
    Stream.from (fun i ->
      try Some a.(i) with
      | Invalid_argument _ -> None)
  ;;

  let to_array strm = Array.of_list (to_list strm)
  let of_hashtbl t = of_list (Hashtbl.to_alist t)

  let to_hashtbl xs =
    let t = Hashtbl.Poly.create () in
    iter xs ~f:(fun (key, data) -> Hashtbl.Poly.set t ~key ~data);
    t
  ;;

  let of_map t = of_list (Map.to_alist t)

  let to_map xs =
    fold xs ~init:Map.Poly.empty ~f:(fun accu (key, data) -> Map.Poly.set accu ~key ~data)
  ;;

  let of_set t = of_list (Set.to_list t)
  let to_set xs = fold xs ~init:Set.Poly.empty ~f:(fun accu e -> Set.Poly.add accu e)

  module Result = struct
    let stream_map = map
    let stream_map2_exn = map2_exn
    let stream_fold = fold

    type ('a, 'b) t = ('a, 'b) Result.t Stream.t

    module Impl = struct
      let all_gen (type e) g (xs : ('a, e) t) ~f =
        let module M = struct
          exception E of e
        end
        in
        let error_to_exn e = M.E e in
        try g (f (result_to_exn xs ~error_to_exn)) with
        | M.E e -> Result.Error e
      ;;

      let all xs ~f = all_gen Fn.id xs ~f
      let all' xs ~f = all_gen (fun x -> Ok x) xs ~f
      let to_exn = result_to_exn

      let map' rs ~f =
        let f = function
          | Ok x -> Ok (f x)
          | Error _ as e -> e
        in
        stream_map rs ~f
      ;;

      let map rs ~f =
        let f = function
          | Ok x -> f x
          | Error _ as e -> e
        in
        stream_map rs ~f
      ;;

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

    include Impl
  end

  module Or_error = struct
    type 'a t = 'a Or_error.t Stream.t

    include Result.Impl
  end
end
