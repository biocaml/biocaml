open Batteries

type ('a,'b,'c,'d) t = {
  table : ('b,'d) Hashtbl.t ;
  zero : 'd ;
  proj : 'a -> 'b ;
  add : 'c -> 'd -> 'd
}

let create ?(n = 251) zero proj add = {
  table = Hashtbl.create 251 ;
  zero ; proj ; add
}

let add (t : ('a,'b,'c,'d) t) x y =
  let bin = t.proj x in
  let accu =
    try Hashtbl.find t.table bin
    with Not_found -> t.zero in
  Hashtbl.replace t.table bin (t.add y accu)

let enum t = Hashtbl.enum t.table

let get t = Hashtbl.find t.table

type 'instance counter = ('instance, 'instance, int, int) t

module Counter = struct
  type 'a t = 'a counter
  let create ?n () = create ?n 0 identity ( + )
  let add = add
  let tick accu x = add accu x 1
end

let counts f e = 
  let c = Counter.create () in
  Enum.iter (Counter.tick c) (e /@ f) ;
  enum c


type ('a, 'b) relation = ('a,'a,'b,'b list) t

module Relation = struct
  type ('a, 'b) t = ('a,'b) relation
  let create ?n () = 
    create [] identity (fun x xs -> x :: xs)
  let add = add
end

let relation xs = 
  let r = Relation.create () in
  Enum.iter
    (fun (x,y) -> Relation.add r x y)
    xs ;
  enum r




















