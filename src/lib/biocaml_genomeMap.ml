open Core.Std
open Biocaml_internal_utils
open Stream.Infix

type range = Biocaml_range.t
type 'a location = 'a * range

module Range = Biocaml_range
module Accu = Biocaml_accu

module Map = struct
  include Map.Poly.Tree

  let to_stream t = Stream.of_list (to_alist t)
  let of_stream xs =
    Stream.fold xs ~init:empty ~f:(fun accu (key,data) -> add accu ~key ~data)
end

module Selection = struct
  type 'a t = ('a, Biocaml_iset.t) Map.t

  let inter u v =
    Map.fold u ~init:Map.empty ~f:(fun ~key:k ~data:set_u accu ->
      match Map.find v k with
      | Some set_v -> Map.add accu ~key:k ~data:(Biocaml_iset.inter set_u set_v)
      | None -> accu
    )

  let union u v =
    let keys = List.dedup (Map.keys u @ Map.keys v) in
    List.fold keys ~init:Map.empty ~f:(fun accu k ->
      Map.add accu ~key:k ~data:(
        Biocaml_iset.union
          (Option.value (Map.find u k) ~default:Biocaml_iset.empty)
          (Option.value (Map.find v k) ~default:Biocaml_iset.empty)
      )
    )

  let diff u v =
    Map.fold u ~init:Map.empty ~f:(fun ~key:k ~data:set_u accu ->
      let set_u' =
	match Map.find v k with
	| Some set_v -> Biocaml_iset.diff set_u set_v
	| None -> set_u
      in 
      Map.add ~key:k ~data:set_u' accu
    )

  let size x =
    Map.fold x ~init:0 ~f:(fun ~key ~data:set accu -> Biocaml_iset.cardinal set + accu)

  let intersection_size (k,r) dom = Biocaml_iset.(
    match Map.find dom k with
    | Some x -> 
      inter Range.(add_range empty r.lo r.hi) x
      |! cardinal
    | None -> 0
  )

  let intersects (k,r) dom =
    Option.value_map
      (Map.find dom k)
      ~default:false
      ~f:(fun x -> Range.(Biocaml_iset.intersects_range x r.lo r.hi))
      
  let to_stream dom =
    (Map.to_stream dom) 
    /@ (fun (k,s) -> Stream.map ~f:(fun (lo,hi) -> k, ok_exn (Range.make lo hi)) (Biocaml_iset.to_stream s))
      |! Stream.concat

  let of_stream e =
    let accu = Accu.create Biocaml_iset.empty fst (fun (_,r) -> Range.(fun x -> Biocaml_iset.add_range x r.lo r.hi)) in
    Stream.iter ~f:(fun loc -> Accu.add accu loc loc ) e ;
    Map.of_stream (Accu.stream accu)
end


module type Signal = sig
  type ('a,'b) t
  val make : ('b list -> 'c) -> ('a location * 'c) Stream.t -> ('a,'c) t

  val eval : 'a -> int -> ('a,'b) t -> 'b

  val fold : ('a -> Range.t -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c

  val to_stream : ('a,'b) t -> ('a location * 'b) Stream.t
end

module LMap = struct
  module T = Biocaml_interval_tree

  type ('a,'b) t = ('a, 'b T.t) Map.t

  let intersects (k,r) lmap =
    Option.value_map (Map.find lmap k) ~default:false ~f:(fun x -> Range.(T.intersects x r.lo r.hi))

  let closest (k,r) lmap = 
    Option.bind
      (Map.find lmap k)
      Range.(fun x ->
	try
	  let lo,hi,label,d = T.find_closest r.lo r.hi x in
	  Some ((k, ok_exn (make lo hi)), label, d)
	with T.Empty_tree -> None
      )

  let intersecting_elems (k, { Range.lo ; hi }) lmap =
    match Map.find lmap k with
    | Some x ->
      T.find_intersecting_elem lo hi x
      /@ (fun (lo,hi,x) -> (k, ok_exn (Range.make lo hi)), x)
    | None -> Stream.empty ()

  let to_stream dom =
    (Map.to_stream dom) 
    /@ (fun (k,t) -> Stream.map ~f:(fun (lo,hi,x) -> (k, ok_exn (Range.make lo hi)), x) (T.to_stream t))
    |! Stream.concat

  let of_stream e =
    let accu =
      Accu.create T.empty (fun x -> fst x |! fst)
        (fun ((_,r),v) -> Range.(T.add ~data:v ~low:r.lo ~high:r.hi)) in
    Stream.iter ~f:(fun loc -> Accu.add accu loc loc ) e ;
    Map.of_stream (Accu.stream accu)

end

module LSet = struct
  module T = Biocaml_interval_tree

  type 'a t = ('a, unit T.t) Map.t

  let intersects = LMap.intersects

  let closest loc lset =
    Option.map (LMap.closest loc lset) ~f:(fun (loc', (), d) -> loc', d)

  let intersecting_elems loc lset = 
    LMap.intersecting_elems loc lset /@ fst

  let to_stream lset = LMap.to_stream lset /@ fst
  let of_stream e = e /@ (fun x -> x, ()) |! LMap.of_stream

end


module type LMap_spec = sig
  type ('a,'b) t

  val make : ('a location * 'b) Stream.t -> ('a,'b) t

  val fold : ('a -> Range.t -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c

  val pwfold : ('a -> Range.t -> 'b list -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

  val intersects : 'a location -> ('a,'b) t -> bool

  val to_stream : ('a,'b) t -> ('a location * 'b) Stream.t

  val union : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val add : 'a location -> 'b -> ('a,'b) t -> ('a,'b) t
end

