include DynArray

let pad_set darr idx v default =
  let pad_size = idx - DynArray.length darr + 1 in
    if pad_size <= 0 then
      DynArray.set darr idx v
    else
      (DynArray.append (DynArray.init pad_size (fun _ -> default)) darr;
       DynArray.set darr idx v)

let binary_search cmp elem dynarr = 
  let last = DynArray.length dynarr - 1 in
  let get idx = DynArray.get dynarr idx in
  if last = -1 then -1 
  else match cmp elem (get 0) with
    | -1 -> -1 
    |  0 ->  0
    |  1 -> 
         begin
           let asserter lo = 
             assert ((get lo) <= elem); -lo - 2
           in
           let rec aux lo hi =
             let mid = (lo + hi + 1) / 2 in
             match cmp elem (get mid) with
               | -1 -> if hi = mid || lo >= mid then asserter lo else
                    aux lo mid
               |  0 -> mid
               |  1 -> if mid >= hi then asserter lo else aux mid hi
               |  _ -> failwith "Comparison should only give -1, 0, or 1." 
           in
           aux 0 last
         end
    |  _ -> failwith "Comparison should only give -1, 0, or 1."
