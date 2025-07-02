(*
 * This implementation is largely inspired from the Set implementation of the
 * standard library and the BatSet module (from Batteries) for enum-related
 * functions
 *)

type 'a t =
  | Empty
  | Node of 'a node

and 'a node =
  { left : 'a t (* left child *)
  ; lo : int (* left-end of the interval at this node *)
  ; hi : int (* right-end of the interval at this node *)
  ; elt : 'a (* element at this node *)
  ; right : 'a t (* right child *)
  ; height : int (* height of the tree (for balancing purpose) *)
  ; left_end : int (* left-end of the left-most interval in the tree *)
  ; right_end : int (* right-end of the right-most interval in the tree *)
  }

exception Empty_tree

let interval_compare lo hi lo' hi' =
  let c = compare lo lo' in
  if c = 0 then compare hi hi' else c
;;

let is_empty = function
  | Empty -> true
  | _ -> false
;;

let height = function
  | Empty -> 0
  | Node n -> n.height
;;

let left_end = function
  | Empty -> Int.max_value
  | Node n -> n.left_end
;;

let right_end = function
  | Empty -> Int.min_value
  | Node n -> n.right_end
;;

let rec cardinal = function
  | Empty -> 0
  | Node n -> cardinal n.left + 1 + cardinal n.right
;;

let node_contents n = n.lo, n.hi, n.elt
let interval_overlap lo hi lo' hi' = (lo <= lo' && lo' <= hi) || (lo' <= lo && lo <= hi')

let rec intersects t ~low ~high =
  match t with
  | Empty -> false
  | Node n ->
    if interval_overlap low high n.lo n.hi
    then true
    else if interval_overlap low high n.left_end n.right_end
    then intersects n.left ~low ~high || intersects n.right ~low ~high
    else false
;;

let interval_distance lo hi lo' hi' =
  if interval_overlap lo hi lo' hi' then 0 else min (abs (lo' - hi)) (abs (lo - hi'))
;;

let tree_distance lo hi = function
  | Empty -> Int.max_value
  | Node n -> interval_distance lo hi n.left_end n.right_end
;;

let empty = Empty

let create l lo hi elt r =
  let hl =
    match l with
    | Empty -> 0
    | Node n -> n.height
  and hr =
    match r with
    | Empty -> 0
    | Node n -> n.height
  in
  Node
    { left = l
    ; right = r
    ; hi
    ; lo
    ; elt
    ; height = (if hl >= hr then hl + 1 else hr + 1)
    ; left_end =
        (let le = left_end l in
         if lo < le then lo else le)
    ; right_end =
        (let lre = right_end l
         and rre = right_end r in
         if hi > lre then if hi > rre then hi else rre else if lre > rre then lre else rre)
    }
;;

let bal l lo hi elt r =
  let hl =
    match l with
    | Empty -> 0
    | Node n -> n.height
  and hr =
    match r with
    | Empty -> 0
    | Node n -> n.height
  in
  if hl > hr + 2
  then (
    match l with
    | Empty -> assert false
    | Node ln ->
      if height ln.left >= height ln.right
      then create ln.left ln.lo ln.hi ln.elt (create ln.right lo hi elt r)
      else (
        match ln.right with
        | Empty -> assert false
        | Node lrn ->
          create
            (create ln.left ln.lo ln.hi ln.elt lrn.left)
            lrn.lo
            lrn.hi
            lrn.elt
            (create lrn.right lo hi elt r)))
  else if hr > hl + 2
  then (
    match r with
    | Empty -> assert false
    | Node rn ->
      if height rn.right >= height rn.left
      then create (create l lo hi elt rn.left) rn.lo rn.hi rn.elt rn.right
      else (
        match rn.left with
        | Empty -> assert false
        | Node rln ->
          create
            (create l lo hi elt rln.left)
            rln.lo
            rln.hi
            rln.elt
            (create rln.right rn.lo rn.hi rn.elt rn.right)))
  else
    Node
      { left = l
      ; lo
      ; hi
      ; elt
      ; right = r
      ; left_end =
          (let le = left_end l in
           if lo < le then lo else le)
      ; right_end =
          (let lre = right_end l
           and rre = right_end r in
           if hi > lre
           then if hi > rre then hi else rre
           else if lre > rre
           then lre
           else rre)
      ; height = (if hl >= hr then hl + 1 else hr + 1)
      }
;;

let add t ~low ~high ~data =
  let rec aux lo hi elt = function
    | Empty -> create Empty lo hi elt Empty
    | Node n ->
      let c = interval_compare lo hi n.lo n.hi in
      if c <= 0
      then bal (aux lo hi elt n.left) n.lo n.hi n.elt n.right
      else bal n.left n.lo n.hi n.elt (aux lo hi elt n.right)
  in
  aux low high data t
;;

let rec elements_aux accu = function
  | Empty -> accu
  | Node n -> elements_aux (node_contents n :: elements_aux accu n.right) n.left
;;

let elements s = elements_aux [] s

type 'a iter =
  | E
  | C of 'a node * 'a t * 'a iter

let rec cons_iter s t =
  match s with
  | Empty -> t
  | Node n -> cons_iter n.left (C (n, n.right, t))
;;

let rec rev_cons_iter s t =
  match s with
  | Empty -> t
  | Node n -> rev_cons_iter n.right (C (n, n.left, t))
;;

let stream_next l _ =
  match !l with
  | E -> None
  | C (n, s, t) ->
    l := cons_iter s t;
    Some (node_contents n)
;;

let stream_backwards_next l _ =
  match !l with
  | E -> None
  | C (n, s, t) ->
    l := rev_cons_iter s t;
    Some (node_contents n)
;;

let to_stream t =
  let l = ref (cons_iter t E) in
  CFStream.Stream.from (stream_next l)
;;

let to_backwards_stream t =
  let l = ref (rev_cons_iter t E) in
  CFStream.Stream.from (stream_backwards_next l)
;;

let rec find_closest_aux lo hi = function
  | Empty -> None
  | Node n ->
    let dc = interval_distance lo hi n.lo n.hi in
    if dc = 0
    then Some (n, 0)
    else (
      let dl_lb = tree_distance lo hi n.left
      and dr_lb = tree_distance lo hi n.right in
      let optval, optnode =
        if dl_lb < dc
        then (
          match find_closest_aux lo hi n.left with
          | Some (nl, dl) when dl < dc -> dl, nl
          | _ -> dc, n)
        else dc, n
      in
      let optval, optnode =
        if dr_lb < optval
        then (
          match find_closest_aux lo hi n.right with
          | Some (nr, dr) when dr < optval -> dr, nr
          | _ -> optval, optnode)
        else optval, optnode
      in
      Some (optnode, optval))
;;

let find_closest lo hi t =
  match find_closest_aux lo hi t with
  | Some (n, d) ->
    let lo', hi', v = node_contents n in
    lo', hi', v, d
  | None -> raise Empty_tree
;;

let rec check_height_integrity = function
  | Empty -> 0
  | Node n ->
    let h = max (check_height_integrity n.left) (check_height_integrity n.right) + 1 in
    assert (h = n.height);
    assert (abs (height n.left - height n.right) <= 2);
    h
;;

let check_integrity t = ignore (check_height_integrity t : int)

let rec print_aux margin = function
  | Empty -> ()
  | Node n ->
    let space = String.make margin ' ' in
    printf "%s(%d,%d)_(%d,%d)\n" space n.lo n.hi n.left_end n.right_end;
    printf "%sleft\n" space;
    print_aux (margin + 2) n.left;
    printf "%sright\n" space;
    print_aux (margin + 2) n.right
;;

let print t = print_aux 0 t

let find_intersecting_elem lo hi t =
  let rec loop = function
    | [] -> None
    | h :: t -> (
      match h with
      | Empty -> loop t
      | Node n ->
        if interval_overlap lo hi n.left_end n.right_end
        then (
          let t = n.left :: n.right :: t in
          if interval_overlap lo hi n.lo n.hi then Some (node_contents n, t) else loop t)
        else loop t)
  in
  CFStream.Stream.unfold [ t ] ~f:loop
;;

let filter_overlapping t ~low ~high =
  let res = ref empty in
  let rec loop = function
    | [] -> ()
    | h :: t -> (
      match h with
      | Empty -> loop t
      | Node n ->
        if interval_overlap low high n.left_end n.right_end
        then (
          let t = n.left :: n.right :: t in
          if interval_overlap low high n.lo n.hi
          then res := add !res ~low:n.lo ~high:n.hi ~data:n.elt;
          loop t)
        else loop t)
  in
  loop [ t ];
  !res
;;

module Test = struct
  module ListImpl = struct
    type 'a t = (int * int * 'a) list

    exception Empty_tree

    let empty = []

    (* let is_empty = function
      | [] -> true
      | _ :: _ -> false
    ;; *)

    let add t ~low ~high ~data = (low, high, data) :: t

    (* let cardinal = List.length *)
    (* let elements x = x *)
    let to_stream x = List.sort ~compare:Poly.compare x |> CFStream.Stream.of_list

    (* let to_backwards_stream x =
      List.sort ~compare:(Fn.flip Poly.compare) x |> CFStream.Stream.of_list
    ;; *)

    (* let pos x = if x < 0 then 0 else x *)

    let interval_overlap lo hi lo' hi' =
      (lo <= lo' && lo' <= hi) || (lo' <= lo && lo <= hi')
    ;;

    let intersects l ~low ~high =
      List.exists ~f:(fun (lo', hi', _) -> interval_overlap low high lo' hi') l
    ;;

    let interval_distance lo hi lo' hi' =
      if interval_overlap lo hi lo' hi' then 0 else min (abs (lo' - hi)) (abs (lo - hi'))
    ;;

    let interval_dist = interval_distance

    let find_closest lo hi = function
      | [] -> raise Empty_tree
      | h :: t ->
        let lo', hi', x =
          List.fold_left
            t
            ~init:h
            ~f:(fun ((lo', hi', _) as interval') ((lo'', hi'', _) as interval'') ->
            if interval_dist lo hi lo' hi' <= interval_dist lo hi lo'' hi''
            then interval'
            else interval'')
        in
        lo', hi', x, interval_dist lo hi lo' hi'
    ;;

    let find_intersecting_elem lo hi t =
      CFStream.Stream.filter
        ~f:(fun (x, y, _) -> interval_overlap lo hi x y)
        (to_stream t)
    ;;

    (* let filter_overlapping t ~low ~high =
      List.filter ~f:(fun (x, y, _) -> interval_overlap low high x y) t
    ;; *)

    (* let print _ = assert false *)
    (* let check_integrity _ = assert false *)
  end

  let random_interval ?(lb = 0) ?(ub = 100) ?(minw = 1) ?(maxw = 30) _ =
    assert (maxw < ub - lb);
    let w = Random.int (maxw - minw) + minw in
    let lo = Random.int (ub - lb - w) + lb in
    lo, lo + w, ()
  ;;

  let random_intervals ?(lb = 0) ?(ub = 100) ?(minw = 1) ?(maxw = 30) n =
    CFStream.Stream.range 1 ~until:n
    |> CFStream.Stream.map ~f:(random_interval ~lb ~ub ~minw ~maxw)
  ;;

  module TestAdditions (I : sig
    type 'a t

    val empty : 'a t
    val add : 'a t -> low:int -> high:int -> data:'a -> 'a t
  end) =
  struct
    let of_list l =
      List.fold_left l ~init:I.empty ~f:(fun accu (low, high, data) ->
        I.add accu ~low ~high ~data)
    ;;
  end

  module T = TestAdditions (struct
    type nonrec 'a t = 'a t

    let empty = empty
    let add = add
  end)

  module L = TestAdditions (ListImpl)

  let%expect_test "test_add" =
    for _ = 1 to 100 do
      let intervals = random_intervals 100 |> CFStream.Stream.to_list in
      ignore
        (List.fold_left intervals ~init:empty ~f:(fun accu (lo, hi, _) ->
           let r = add accu ~low:lo ~high:hi ~data:() in
           check_integrity r;
           r)
          : unit t)
    done;
    [%expect {|
    |}]
  ;;

  let%expect_test "test_creation" =
    let all_good =
      List.init 100 ~f:Fn.id
      |> List.for_all ~f:(fun _ ->
           let intervals = random_intervals 100 |> CFStream.Stream.to_list in
           let lres =
             ListImpl.(intervals |> L.of_list |> to_stream |> CFStream.Stream.to_list)
           and tres = intervals |> T.of_list |> to_stream |> CFStream.Stream.to_list in
           Int.equal 100 (List.length lres)
           && Int.equal 100 (List.length tres)
           && Poly.equal lres tres)
    in
    printf "%b\n" all_good;
    [%expect {|
      true
    |}]
  ;;

  let%expect_test "test_intersection" =
    let all_good =
      List.init 1000 ~f:Fn.id
      |> List.for_all ~f:(fun _ ->
           let intervals = random_intervals ~ub:1000 1000 |> CFStream.Stream.to_list
           and low, high, _ = random_interval ~ub:1000 () in
           let l = ListImpl.(intersects (L.of_list intervals) ~low ~high)
           and t = intersects (T.of_list intervals) ~low ~high in
           Bool.equal l t)
    in
    printf "%b\n" all_good;
    [%expect {|
      true
    |}]
  ;;

  let%expect_test "test_find_closest" =
    let all_good =
      List.init 1000 ~f:Fn.id
      |> List.for_all ~f:(fun _ ->
           let intervals = random_intervals ~ub:1000 1000 |> CFStream.Stream.to_list
           and lo, hi, _ = random_interval ~ub:1000 () in
           let llo, lhi, _, dl = ListImpl.(find_closest lo hi (L.of_list intervals))
           and tlo, thi, _, dt = find_closest lo hi (T.of_list intervals)
           and dist (x, y) = ListImpl.interval_dist lo hi x y in
           let cmp x y = dist x = dist y in
           (* let printer (lo', hi') =
             sprintf "[%d,%d](%d to [%d,%d])" lo' hi' (dist (lo', hi')) lo hi
           in *)
           let x = llo, lhi in
           let y = tlo, thi in
           (* let () = printf "%s\n%s\n" (printer x) (printer y) in *)
           cmp x y && Poly.equal dl dt && Poly.equal dl (dist (llo, lhi)))
    in
    printf "%b\n" all_good;
    [%expect {|
      true
    |}]
  ;;

  let%expect_test "test_find_intersecting_elem1" =
    let u =
      add
        ~low:69130
        ~high:69630
        ~data:()
        (add ~low:69911 ~high:70411 ~data:() (add ~low:70501 ~high:71001 ~data:() empty))
    in
    printf
      "%b\n"
      (Int.equal
         1
         (find_intersecting_elem 70163 70163 u |> CFStream.Stream.to_list |> List.length));
    printf
      "%b\n"
      (Int.equal
         3
         (find_intersecting_elem 65163 75163 u |> CFStream.Stream.to_list |> List.length));
    [%expect {|
      true
      true
    |}]
  ;;

  let%expect_test "test_find_intersecting_elem2" =
    let all_good =
      List.init 1000 ~f:Fn.id
      |> List.for_all ~f:(fun _ ->
           let intervals = random_intervals ~ub:1000 1000 |> CFStream.Stream.to_list
           and lo, hi, _ = random_interval ~ub:1000 () in
           let l =
             ListImpl.(find_intersecting_elem lo hi (L.of_list intervals))
             |> CFStream.Stream.to_set
           and t =
             find_intersecting_elem lo hi (T.of_list intervals) |> CFStream.Stream.to_set
           in
           Int.equal 0 (Set.length (Set.union (Set.diff l t) (Set.diff t l)))
           (* [assert_equal l t] is not a valid test because sets cannot be
         compared with ( = ). Indeed, a set is an AVL tree whose structure
         may depend on the order its elements were added: ( = ) compare
         the structure directly and may for this reason fail to see two
         sets have the same elements *))
    in
    printf "%b\n" all_good;
    [%expect {|
      true
    |}]
  ;;
end
