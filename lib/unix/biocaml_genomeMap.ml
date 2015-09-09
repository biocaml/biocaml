open Core.Std
open Biocaml_internal_utils
open Stream.Infix

module type Chromosome = sig
  type t
  val compare : t -> t -> int
end

module Make(Chromosome : Chromosome) = struct
  type range = Biocaml_range.t
  type location = Chromosome.t * range

  module Range = Biocaml_range
  module Accu = Biocaml_accu

  module Map = struct
    include Map.Make(struct
        include Chromosome
        let sexp_of_t _ = assert false
        let t_of_sexp _ = assert false
      end)

    let to_stream t = Stream.of_list (to_alist t)
    let of_stream xs =
      Stream.fold xs ~init:empty ~f:(fun accu (key,data) -> add accu key data)
  end

  module Selection = struct
    type t = Biocaml_iset.t Map.t

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

    let overlap sel (k,r) = Biocaml_iset.(
        match Map.find sel k with
        | Some x ->
          inter Range.(add_range empty r.lo r.hi) x
          |> cardinal
        | None -> 0
      )

    let intersects sel (k,r) =
      Option.value_map
        (Map.find sel k)
        ~default:false
        ~f:(fun x -> Range.(Biocaml_iset.intersects_range x r.lo r.hi))

    let to_stream sel =
      (Map.to_stream sel)
      /@ (fun (k,s) -> Stream.map ~f:(fun (lo,hi) -> k, ok_exn (Range.make lo hi)) (Biocaml_iset.to_stream s))
      |> Stream.concat

    let of_stream e =
      let accu = Accu.create Biocaml_iset.empty fst (fun (_,r) -> Range.(fun x -> Biocaml_iset.add_range x r.lo r.hi)) in
      Stream.iter ~f:(fun loc -> Accu.add accu loc loc ) e ;
      Map.of_stream (Accu.stream accu)
  end


  module type Signal = sig
    type 'a t

    val eval : 'a t -> default:'a -> Chromosome.t -> int -> 'a
    (** function evaluation at some point in the genome *)

    val fold : 'a t -> init:'c -> f:('c -> location -> 'b -> 'c) -> 'c
    (** folds on constant intervals of the function, in increasing order *)

    val to_stream : 'a t -> (location * 'a) Stream.t
    (** enumeration over all constant intervals of the function, in
        increasing order *)

    val of_stream : ('a -> 'a -> 'a) -> (location * 'a) Stream.t -> 'a t
  end

  module LMap = struct
    module T = Biocaml_interval_tree

    type 'a t = 'a T.t Map.t

  let intersects lmap (k,r) =
    Option.value_map (Map.find lmap k) ~default:false ~f:(fun x -> Range.(T.intersects x r.lo r.hi))

  let closest lmap (k,r) =
    Option.bind
      (Map.find lmap k)
      Range.(fun x ->
          try
            let lo,hi,label,d = T.find_closest r.lo r.hi x in
            Some ((k, ok_exn (make lo hi)), label, d)
          with T.Empty_tree -> None
        )

  let intersecting_elems lmap (k, { Range.lo ; hi }) =
    match Map.find lmap k with
    | Some x ->
      T.find_intersecting_elem lo hi x
      /@ (fun (lo,hi,x) -> (k, ok_exn (Range.make lo hi)), x)
    | None -> Stream.empty ()

  let to_stream lmap =
    (Map.to_stream lmap)
    /@ (fun (k,t) -> Stream.map ~f:(fun (lo,hi,x) -> (k, ok_exn (Range.make lo hi)), x) (T.to_stream t))
    |> Stream.concat

  let of_stream e =
    let accu =
      Accu.create T.empty (fun x -> fst x |> fst)
        (fun ((_,r),v) -> Range.(T.add ~data:v ~low:r.lo ~high:r.hi)) in
    Stream.iter ~f:(fun loc -> Accu.add accu loc loc ) e ;
    Map.of_stream (Accu.stream accu)

  end

  module LSet = struct
    module T = Biocaml_interval_tree

    type t = unit T.t Map.t

    let intersects = LMap.intersects

    let closest lset loc =
      Option.map (LMap.closest lset loc) ~f:(fun (loc', (), d) -> loc', d)

    let intersecting_elems lset loc =
      LMap.intersecting_elems lset loc /@ fst

    let to_stream lset = LMap.to_stream lset /@ fst
    let of_stream e = e /@ (fun x -> x, ()) |> LMap.of_stream

  end

end
