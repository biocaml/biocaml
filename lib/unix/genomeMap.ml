module type Chromosome = sig
  type t

  val compare : t -> t -> int
end

module Make (Chromosome : Chromosome) = struct
  type range = Range.t
  type location = Chromosome.t * range

  module Map = struct
    include Map.Make (struct
        include Chromosome

        let sexp_of_t _ = assert false
        let t_of_sexp _ = assert false
      end)

    let to_stream t = CFStream.of_list (Map.to_alist t)

    let of_stream xs =
      CFStream.fold xs ~init:empty ~f:(fun accu (key, data) -> Map.set accu ~key ~data)
    ;;
  end

  module Selection = struct
    type t = Iset.t Map.t

    let empty = Map.empty

    let add sel (chr, { Range.lo; hi }) =
      let set_chr =
        match Base.Map.find sel chr with
        | None -> Iset.empty
        | Some s -> s
      in
      let set_chr = Iset.add_range set_chr lo hi in
      Base.Map.set sel ~key:chr ~data:set_chr
    ;;

    let inter u v =
      Base.Map.fold u ~init:Map.empty ~f:(fun ~key:k ~data:set_u accu ->
        match Base.Map.find v k with
        | Some set_v -> Base.Map.set accu ~key:k ~data:(Iset.inter set_u set_v)
        | None -> accu)
    ;;

    let union u v =
      let keys =
        List.dedup_and_sort ~compare:Chromosome.compare (Base.Map.keys u @ Base.Map.keys v)
      in
      List.fold keys ~init:Map.empty ~f:(fun accu k ->
        Base.Map.set
          accu
          ~key:k
          ~data:
            (Iset.union
               (Option.value (Base.Map.find u k) ~default:Iset.empty)
               (Option.value (Base.Map.find v k) ~default:Iset.empty)))
    ;;

    let diff u v =
      Base.Map.fold u ~init:Map.empty ~f:(fun ~key:k ~data:set_u accu ->
        let set_u' =
          match Base.Map.find v k with
          | Some set_v -> Iset.diff set_u set_v
          | None -> set_u
        in
        Base.Map.set ~key:k ~data:set_u' accu)
    ;;

    let size x =
      Base.Map.fold x ~init:0 ~f:(fun ~key:_ ~data:set accu -> Iset.cardinal set + accu)
    ;;

    let overlap sel (k, r) =
      Iset.(
        match Base.Map.find sel k with
        | Some x -> inter Range.(add_range empty r.lo r.hi) x |> cardinal
        | None -> 0)
    ;;

    let intersects sel (k, r) =
      Option.value_map (Base.Map.find sel k) ~default:false ~f:(fun x ->
        Range.(Iset.intersects_range x r.lo r.hi))
    ;;

    let to_stream sel =
      Map.to_stream sel
      |> CFStream.map ~f:(fun (k, s) ->
        CFStream.map (Iset.to_stream s) ~f:(fun (lo, hi) -> k, ok_exn (Range.make lo hi)))
      |> CFStream.concat
    ;;

    let of_stream e =
      let accu =
        Accu.create
          ~bin:fst
          ~zero:Iset.empty
          ~add:(fun (_, r) -> Range.(fun x -> Iset.add_range x r.lo r.hi))
          ()
      in
      CFStream.iter ~f:(fun loc -> Accu.add accu loc loc) e;
      Map.of_stream (Accu.stream accu)
    ;;
  end

  module type Signal = sig
    type 'a t

    (** function evaluation at some point in the genome *)
    val eval : 'a t -> default:'a -> Chromosome.t -> int -> 'a

    (** folds on constant intervals of the function, in increasing order *)
    val fold : 'a t -> init:'c -> f:('c -> location -> 'b -> 'c) -> 'c

    (** enumeration over all constant intervals of the function, in
        increasing order *)
    val to_stream : 'a t -> (location * 'a) Stream.t

    val of_stream : ('a -> 'a -> 'a) -> (location * 'a) Stream.t -> 'a t
  end

  module LMap = struct
    module T = Interval_tree

    type 'a t = 'a T.t Map.t

    let intersects lmap (k, r) =
      Option.value_map (Base.Map.find lmap k) ~default:false ~f:(fun x ->
        Range.(T.intersects x ~low:r.lo ~high:r.hi))
    ;;

    let closest lmap (k, r) =
      Option.bind
        (Base.Map.find lmap k)
        ~f:
          Range.(
            fun x ->
              try
                let lo, hi, label, d = T.find_closest r.lo r.hi x in
                Some ((k, ok_exn (make lo hi)), label, d)
              with
              | T.Empty_tree -> None)
    ;;

    let intersecting_elems lmap (k, { Range.lo; hi }) =
      match Base.Map.find lmap k with
      | Some x ->
        T.find_intersecting_elem lo hi x
        |> CFStream.map ~f:(fun (lo, hi, x) -> (k, ok_exn (Range.make lo hi)), x)
      | None -> CFStream.empty ()
    ;;

    let to_stream lmap =
      Map.to_stream lmap
      |> CFStream.map ~f:(fun (k, t) ->
        CFStream.map
          ~f:(fun (lo, hi, x) -> (k, ok_exn (Range.make lo hi)), x)
          (T.to_stream t))
      |> CFStream.concat
    ;;

    let of_stream e =
      let accu =
        Accu.create
          ~bin:(fun x -> fst x |> fst)
          ~zero:T.empty
          ~add:(fun ((_, r), v) -> Range.(T.add ~data:v ~low:r.lo ~high:r.hi))
          ()
      in
      CFStream.iter ~f:(fun loc -> Accu.add accu loc loc) e;
      Map.of_stream (Accu.stream accu)
    ;;
  end

  module LSet = struct
    module T = Interval_tree

    type t = unit T.t Map.t

    let intersects = LMap.intersects

    let closest lset loc =
      Option.map (LMap.closest lset loc) ~f:(fun (loc', (), d) -> loc', d)
    ;;

    let intersecting_elems lset loc =
      LMap.intersecting_elems lset loc |> CFStream.map ~f:fst
    ;;

    let to_stream lset = LMap.to_stream lset |> CFStream.map ~f:fst
    let of_stream e = e |> CFStream.map ~f:(fun x -> x, ()) |> LMap.of_stream
  end
end
