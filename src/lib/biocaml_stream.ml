open Core.Std

include Stream
let next_exn = next
let next s = try Some (next_exn s) with Stream.Failure -> None

let is_empty s =
  match peek s with 
  | None -> true 
  | Some _ -> false

let empty () = from (const None)

type 'a streamable = 'a t
let stream x = x
let of_stream x = x

exception Expected_streams_of_equal_length

let of_list l =
  let lr = ref l in
  let f _ = match !lr with
    | h :: t -> lr := t ; Some h
    | [] -> None 
  in
  from f

let rec iteri xs ~f =
  match peek xs with
  | Some x -> f (count xs) x ; junk xs ; iteri xs ~f
  | None -> ()

let iter xs ~f = iteri xs ~f:(const f)

let rec iter2i_exn xs ys ~f =
  match peek xs, peek ys with
  | Some x, Some y -> (
    f (count xs) (count ys) x y;
    junk xs ;
    junk ys ;
    iter2i_exn xs ys ~f
  )
  | None, None -> ()
  | _, _ -> raise Expected_streams_of_equal_length

let iter2_exn xs ys ~f = iter2i_exn xs ys ~f:(const (const f))

let iter2i a b ~f = 
  try iter2i_exn a b ~f
  with Expected_streams_of_equal_length -> ()

let iter2 a b ~f = 
  try iter2_exn a b ~f
  with Expected_streams_of_equal_length -> ()

let rec find_map xs ~f =
  match next xs with
  | Some x -> (
    match f x with 
    | Some x as y -> y
    | None -> find_map xs ~f
  )
  | None -> None

let find xs ~f = find_map xs ~f:(fun x -> if f x then Some x else None)

let find_exn xs ~f = match find xs ~f with
| Some x -> x
| None -> raise Not_found

let exists xs ~f = match find xs ~f with 
| Some _ -> true
| None -> false

let rec for_all xs ~f =
  match next xs with
  | Some x when not (f x) -> false
  | Some _ -> for_all xs ~f
  | None -> true

let rec foldi xs ~init ~f =
  match next xs with
  | None -> init
  | Some x -> foldi xs ~init:(f (count xs - 1) init x) ~f
  (* [count xs - 1] because of the call to [next], which increased the
     stream count by one *)

let fold xs ~init ~f = foldi xs ~init ~f:(const f)

let reduce xs ~f =
  match next xs with
  | Some init -> fold xs ~init ~f
  | None -> invalid_arg "Stream.reduce: stream should contain at least one element"

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

let fold2_exn xs ys ~init ~f = fold2i_exn xs ys ~init ~f:(const (const f))

let rec fold2i xs ys ~init ~f = 
  match next xs, next ys with
  | Some x, Some y ->
      let init = f (count xs - 1) (count ys - 1) init x y in
      fold2i xs ys ~init ~f
  | _ -> init

let fold2 xs ys ~init ~f = fold2i xs ys ~init ~f:(const (const f))

let scanl xs ~init ~f =
  let current = ref init in
  let f i =
    if i = 0 then Some init
    else (
      match next xs with
      | Some x -> 
          current := f !current x ;
          Some !current
      | None -> None
    )
  in
  from f

let scan xs ~f =
  match next xs with 
  | Some init -> scanl xs ~init ~f
  | None -> invalid_arg "Stream.scan: input stream should contain at least one value"

let take_whilei xs ~f =
  let aux i =
    match peek xs with
    | Some x when f i x -> junk xs ; Some x
    | _ -> None
  in 
  from aux
    
let take_while xs ~f = take_whilei xs ~f:(const f)

let take k xs = 
  let end_index = count xs + k in
  take_whilei xs ~f:(fun j _ -> j < end_index)
    
let rec drop_whilei xs ~f =
  match peek xs with
  | Some x when f (count xs) x -> junk xs ; drop_whilei xs ~f
  | _ -> ()

let drop_while xs ~f = drop_whilei xs ~f:(const f)

let drop n xs =
  let end_index = count xs + n in
  drop_whilei xs ~f:(fun j _ -> j < end_index)

let skip_whilei xs ~f = 
  drop_whilei xs ~f ; 
  from (fun _ -> next xs)

let skip_while xs ~f = skip_whilei xs ~f:(const f)
  
let skip n xs =
  drop n xs ;
  from (fun _ -> next xs)

let span xs ~f =
    (*Two possibilities: either the tail has been read
      already -- in which case all head data has been 
      copied onto the queue -- or the tail hasn't been
      read -- in which case, stuff should be read from
      [xs] *)
  let queue           = Queue.create () 
  and read_from_queue = ref false in

  let head _ =
    if !read_from_queue then (* Everything from the head has been copied *)
      Queue.dequeue queue    (* to the queue already                     *)
    else 
      match peek xs with
      | Some x as e when f x -> junk xs ; e
      | _ -> None

  and tail _ =
    if not !read_from_queue then (*Copy everything to the queue         *)
      begin
        read_from_queue := true;
        let rec aux () = 
          match peek xs with
          | Some x when f x -> Queue.enqueue queue x ; aux ()
          | e -> e
        in aux ()
      end
    else next xs
  in 
  (from head, from tail)

let group_aux xs map eq =
  let prev_group_force = ref ignore in
  let for_each_group _ =
    !prev_group_force () ;
    match next xs with
    | None -> None
    | Some x ->
        let queue = Queue.create () 
        and forced = ref false 
        and mapped_x = map x in

        let aux i = 
          if i = 0 then Some x
          else (
            if !forced then
              Queue.dequeue queue
            else (
              match peek xs with
              | Some y as e when eq (map y) mapped_x -> junk xs ; e
              | _ -> None
            )
          ) in

        let force () =
          forced := true ;
          let rec loop () =
            match peek xs with
            | Some y when eq (map y) mapped_x -> 
                junk xs ; 
                Queue.enqueue queue y ; 
                loop ()
            | _ -> ()
          in
          loop ()

        in
        prev_group_force := force ;
        Some (from aux)
  in
  from for_each_group

let group xs ~f = group_aux xs f ( = )

let group_by xs ~eq = group_aux xs ident eq

let mapi xs ~f =
  let aux i = Option.map (next xs) ~f:(f i) in 
  from aux

let map xs ~f =
  let aux _ = Option.map (next xs) ~f in 
  from aux

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

let append xs ys =
  let aux _ =
    match next xs with
    | None -> next ys
    | e -> e
  in 
  from aux

let concat xs =
  let rec find_next_non_empty_stream xs =
    match peek xs with
    | Some stream when is_empty stream -> 
        junk xs ;
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
          current := stream ;
          next stream
    )
    | x -> x
  in
  from aux


let combine (xs, ys) =
  let aux _ =
    match peek xs, peek ys with
    | Some x, Some y -> 
        junk xs ;
        junk ys ;
        Some (x,y)
    | _ -> None
  in
  from aux

let uncombine xs =
  let whosfirst = ref `left
  and lq = Queue.create ()
  and rq = Queue.create () in
  
  let rec left i =
    match !whosfirst with
    | `left -> (
      match next xs with
      | None -> None
      | Some (l,r) ->
          Queue.enqueue rq r ;
          Some l
    )
    | `right -> (
      match Queue.dequeue lq with
      | None -> 
          whosfirst := `left ;
          left i
      | x -> x
    )
  and right i =
    match !whosfirst with
    | `right -> (
      match next xs with
      | None -> None
      | Some (l,r) ->
          Queue.enqueue lq l ;
          Some r
    )
    | `left -> (
      match Queue.dequeue rq with
      | None -> 
          whosfirst := `right ;
          right i
      | x -> x
    )
  in
  from left, from right

let merge xs ys ~cmp =
  let aux _ =
    match peek xs, peek ys with
    | Some x as ex, Some y when cmp x y <= 0 -> junk xs ; ex
    | Some _, (Some _ as ey) -> junk ys ; ey
    | Some _ as ex, None -> junk xs ; ex
    | None, (Some _ as ey) -> junk ys ; ey
    | None, None -> None
  in
  from aux

let partition xs ~f =
  let pos_queue = Queue.create ()
  and neg_queue = Queue.create () in
  
  let rec pos i =
    match Queue.dequeue pos_queue with
    | None -> (
      match next xs with
      | Some x when not (f x) -> Queue.enqueue neg_queue x ; pos i
      | e -> e
    )
    | e -> e

  and neg i =
    match Queue.dequeue neg_queue with
    | None -> (
      match next xs with
      | Some x when f x -> Queue.enqueue pos_queue x ; neg i
      | e -> e
    )
    | e -> e
  in
  from pos, from neg

let uniq xs =
  match peek xs with
  | None -> empty ()
  | Some first ->
      let prev = ref first in
      let rec aux i =
        if i = 0 then Some first
        else (
          match next xs with
          | None -> None
          | Some x ->
              if x = !prev then
                aux i
              else (
                prev := x ;
                Some x
              )
        )
      in 
      from aux

let init n ~f =
  if n < 0 then empty ()
  else (
    let aux i = 
      if i < n then Some (f i)
      else None
    in
    from aux
  )

let singleton x = init 1 (const x)

let loop init ~f =
  let prev = ref init in
  let aux i =
    if i = 0 then Some !prev
    else 
      match f i !prev with
      | Some next as e -> prev := next ; e
      | e -> e
  in
  from aux

let range ?until n =
  let stop = Option.value_map until ~default:(fun _ -> true) ~f:( <= ) in
  loop n (fun _ i -> if stop i then None else Some (i + 1))

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

let to_list t =
  List.rev (fold ~init:[] ~f:(fun l b -> b::l) t)

let lines_of_channel cin =
  let f _ =
    try Some (input_line cin)
    with End_of_file -> None
  in Stream.from f

let lines_to_channel xs oc =
  iter
    xs
    ~f:(fun l -> output_string oc l ; output_char oc '\n')

let result_to_exn s ~error_to_exn =
  from (fun _ ->
    match next s with
    | None -> None
    | Some result -> match result with
      | Ok x -> Some x
      | Result.Error x -> raise (error_to_exn x)
  )

let unfold init f =
  let a = ref init in
  from (fun _ -> match f !a with
    | Some (b, a_next) -> (a := a_next; Some b)
    | None -> None
  )

module Infix = struct
  let ( -- ) x y = range x ~until:y

  let ( --. ) (a, step) b =
    let n = Int.of_float ((b -. a) /. step) + 1 in
    if n < 0 then
      empty ()
    else
      init n ~f:(fun i -> Float.of_int i *. step +. a)

  let ( --^ ) x y = range x ~until:(y-1)

  let ( --- ) x y = 
    if x <= y then x -- y
    else loop x ~f:(fun _ prev -> if prev <= y then Some (prev - 1) else None)
      
  let ( /@ ) x f = map x ~f
  let ( // ) x f = filter x ~f
  let ( //@ ) x f = filter_map x ~f
end

