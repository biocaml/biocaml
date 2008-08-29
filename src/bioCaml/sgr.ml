open TylesBase

type t = (string * int * float) list
    (* Stored in ascending order by (string,int) pairs. *)

exception Bad of string
let raise_bad msg = raise (Bad msg)
    
let cmpsi (s1,i1,_) (s2,i2,_) = Pervasives.compare (s1,i1) (s2,i2)

let of_list l = List.sort ~cmp:cmpsi l
let to_list t = t

let of_chr_lists l =
  let ans = List.map (fun (a,l) -> List.map (fun (b,c) -> a,b,c) l) l in
  let ans = List.concat ans in
    List.sort ~cmp:cmpsi ans

let to_chr_lists t =
  let eq (s1,_,_) (s2,_,_) = s1 = s2 in
  let ll = List.npartition eq t in
  let ll = List.map (fun l -> Tuple.Tr.prj1 (List.hd l), List.map (fun (_,b,c) -> b,c) l) ll in
    ll
      
let of_file_exn file =
  let of_channel cin =
    let lines = Stream.lines_of_channel cin in
    let err msg = Msg.err ~pos:(Pos.fl file (Stream.count lines)) msg in
    let parse_line l =
      let ans = String.nsplit l "\t" in
        if List.length ans = 3 then
          ans
        else
          let ans = String.nsplit l " " in
            if List.length ans = 3 then
              ans
            else
              raise_bad "column separator must be tab or single space"
    in
      try
        let lines = Stream.to_list (Stream.map parse_line lines) in
        let f sl = List.nth sl 0, int_of_string (List.nth sl 1), float_of_string (List.nth sl 2) in
          of_list (List.map f lines)
      with
          Failure m | Bad m -> raise_bad (err m)
  in
    try_finally of_channel close_in (open_in file)

let of_file file = try Some (of_file_exn file) with Bad _ -> None

let to_file file t =
  let to_channel t cout =
    let f (s,i,v) =
      output_string cout (String.concat "\t" [s; string_of_int i; string_of_float v]);
      output_char cout '\n'
    in
      List.iter f t
  in
    try_finally (to_channel t) close_out (open_out_safe file)
      
