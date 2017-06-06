open Core_kernel
open CFStream

type ('a,'b,'c,'d) t = {
  table : ('b,'d) Hashtbl.t ;
  zero : 'd ;
  proj : 'a -> 'b ;
  add : 'c -> 'd -> 'd
}

let create ?(n = 251) ~bin:proj ~zero ~add () = {
  table = Hashtbl.Poly.create ~size:n () ;
  zero ; proj ; add
}

let add (t : ('a,'b,'c,'d) t) x y =
  let bin = t.proj x in
  let accu = Option.value (Hashtbl.find t.table bin) ~default:t.zero in
  Hashtbl.set t.table ~key:bin ~data:(t.add y accu)

let stream t = Stream.of_hashtbl t.table

let get t = Hashtbl.find t.table

module Counter = struct
  type nonrec 'a t = ('a, 'a, int, int) t
  let create ?n () = create ?n ~zero:0 ~bin:ident ~add:( + ) ()
  let add = add
  let tick accu x = add accu x 1
  let stream = stream
  let of_stream e =
    let c = create () in
    Stream.iter ~f:(tick c) e ;
    c
end

let counts e =
  stream (Counter.of_stream e)

let product ?filter f l1 l2 = Counter.(
  let c = create () in
  let tick = match filter with
  | Some p -> fun e1 e2 -> if p e1 e2 then tick c (f e1 e2)
  | None   -> fun e1 e2 -> tick c (f e1 e2)
  in
  List.iter ~f:(fun e1 -> List.iter ~f:(tick e1) l2) l1 ;
  stream c
)

module Relation = struct
  type nonrec ('a, 'b) t = ('a,'a,'b,'b list) t
  let create ?n () =
    create ?n ~zero:[] ~bin:ident ~add:(fun x xs -> x :: xs) ()
  let add = add
  let stream = stream
  let of_stream xs =
    let r = create () in
    Stream.iter
      ~f:(fun (x,y) -> add r x y)
      xs ;
    r
end

let relation xs = stream (Relation.of_stream xs)
