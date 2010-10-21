open Pervasives2
module DynArray = DynArray2
module List = List2
module Array = Array2

module type OrderedType = sig
  type t 
  val compare : t -> t -> int
end
  
module type S = sig
  type elt
  type appendage
  type body
  val empty : body
  val construct : elt list -> body
  val within : ('a -> elt -> bool) -> body -> 'a -> elt list
end

module Make (Ord : OrderedType) = struct
  type elt = Ord.t * Ord.t
  type appendage = Arm of elt | Leg
  type body = Body of elt * appendage * body * appendage | Tail
      
  let empty = Tail

(* sbf = list 'sorted by first' *)
(* rsbs = list 'reversed, sorted by second' *)
  let insert sbf rsbs monster = 
    let rec ins b = 
      match b with
        | Tail -> 
            begin
              match sbf,rsbs with
                | Some ((f1,f2) as sbf), Some ((r1,r2) as rsbs) ->  
                    let mx fst snd = if Ord.compare fst snd > 0 then fst else snd in
                    let mn fst snd = if Ord.compare fst snd > 0 then snd else fst in
                    Body (((mn f1 r1),(mx f2 r2)), Arm (sbf), Tail, Arm (rsbs))
                | None, Some (rsbs) -> Body (rsbs, Leg, Tail, Leg)
                | _ -> failwith "Bad data given to Monster.insert"
            end
        | Body (elt, l, b, r) -> Body (elt, l, ins b, r)
    in 
    ins monster

  let construct (lst:elt list) = 
    let cmp (a,b) (c,d) = 
      let ac = Ord.compare a c in
      if ac != 0 then ac else Ord.compare b d
    in
    let rev_cmp (a,b) (c,d) = 
      let db = Ord.compare d b in
      if db != 0 then db else Ord.compare c a
    in
    (* sorted by first *)
    let sbf = ((List.sort ~cmp:cmp) ->> DynArray.of_list) lst in
    (* reverse sorted by second *)
    let rsbs = ((List.sort ~cmp:rev_cmp) ->> DynArray.of_list) lst in
    let rec aux monster = 
      if (DynArray.empty sbf) && (DynArray.empty rsbs) then monster 
      else 
        let get_opt dynarr = 
          try Some (DynArray.get dynarr 0)
          with DynArray.Invalid_arg _ -> None
        in
        let a,b = get_opt sbf, get_opt rsbs in
        if (Some a = Some b) then 
          begin
            DynArray.delete sbf 0; aux monster
          end
        else
          let catch f x = try f x with DynArray.Invalid_arg _ -> () in
          let bsearch cmp elem dynarr = 
            match elem with
              | Some n -> Some (DynArray.binary_search cmp n dynarr)
              | None -> None
          in
          let delete a idx = 
            match idx with
              | Some n -> catch (DynArray.delete a) n
              | None -> ()
          in
          delete sbf (Some 0);
          delete rsbs (Some 0);
          delete rsbs (bsearch rev_cmp a rsbs);
          delete sbf (bsearch cmp b sbf);
          aux (insert a b monster)
    in 
    aux empty

  let within (cmp:'a -> elt -> bool) monster elem : elt list = 
    let build_list acc l r = 
      match (cmp elem l),(cmp elem r) with
        | true, true -> l::r::acc
        | true, false -> l::acc
        | false, true -> r::acc
        | false, false -> acc
    in
    let rec aux acc m = 
      match m with
        | Body (elt, Leg, next, Leg) -> 
            begin
              match cmp elem elt with
                | true -> aux (elt::acc) next
                | false -> acc
            end
        | Body (elt, Arm l, next, Arm r) ->
            begin
              match cmp elem elt with
                |  true -> aux (build_list acc l r) next
                |  false -> build_list acc l r
            end
        | Tail -> acc
        | _ -> failwith "Invalid argument given to aux in Monster.within."
    in
    aux [] monster
end
