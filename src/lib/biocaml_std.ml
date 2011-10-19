
include Printf
let (<<-) f g = fun x -> f (g x)
let identity = fun x -> x

let try_finally_exn ~fend f x =
  match try `V (f x) with e -> `E e with
    | `V f_x -> fend x; f_x
    | `E e -> (try fend x with _ -> ()); raise e

let open_out_safe = open_out_gen [Open_wronly; Open_creat; Open_excl; Open_text] 0o666
let output_endline cout s = output_string cout s; output_string cout "\n"

include BatStd

module List = struct 
  include List
  include ListLabels
  include BatList
  include BatList.Labels

  let npartition ~eq l =
    let insertl ll a =
      let rec loop prefix ll =
        match ll with
        | [] -> rev ([a]::prefix)
        | l::ll ->
          if eq a (hd l)
          then (rev ((a::l)::prefix)) @ ll 
          else loop (l::prefix) ll
      in loop [] ll
    in 
    map ~f:rev (fold_left ~f:insertl ~init:[] l)

end

module String = struct
  include StringLabels
  include BatString
  include BatString.Exceptionless

  (* String.for_all was in Sesame and heavily used in Biocaml. *)
  let for_all ~f s =
    fold_left (fun b c -> b && f c) true s

  let exists ~f s =
    fold_left (fun ans c -> f c || ans) false s

  let strip_final_cr s =
    let l = length s in
    if l > 0 && s.[l-1] = '\r'
    then sub s 0 (l-1)
    else s

  let fold_lefti ~f acc str = 
    let r = ref acc in
    for i = 0 to (String.length str - 1) do
      r := f !r i str.[i]
    done;
    !r

end

module Array = struct
  include Array 
  include BatArray 
  include BatArray.Labels
      
  let is_rectangular d =
    if length d <= 1 then
      true
    else
      let allLengths = map length d in
      let firstLength = get allLengths 0 in
      let lengthEqualsFirstLength k = k = firstLength in
      for_all lengthEqualsFirstLength allLengths
end


module Char = struct
  include Char
  include BatChar

  let is_space = is_whitespace
  let is_alpha_num c = is_letter c || is_digit c

end

module Enum = struct
  include BatEnum
  include BatEnum.Labels
  include BatEnum.Labels.LExceptionless
end  

module Stream = struct
  include Stream

  let lines_of_chars cstr =
    let f _ =
      match peek cstr with
      | None -> None
      | Some _ ->
        let ans = Buffer.create 100 in
        let rec loop () =
          try
            let c = next cstr in
            if c <> '\n' then (Buffer.add_char ans c; loop())
          with Failure -> ()
        in 
        loop();
        Some (Buffer.contents ans)
    in
    from f

  let keep_whilei pred s =
    let f _ =
      match peek s with
      | None -> None
      | Some a ->
        if pred (count s) a
        then (junk s; Some a)
        else None
    in from f
    
  let keep_while pred = keep_whilei (fun _ a -> pred a)
  let truncate k = keep_whilei (fun j _ -> j < k)
  
  let rec skip_whilei pred s =
    match peek s with
    | None -> ()
    | Some a ->
      if pred (count s) a
      then (junk s; skip_whilei pred s)
      else ()

  let skip_while pred = skip_whilei (fun _ a -> pred a)

  let rec fold f accum s =
    match peek s with
      None -> accum
    | Some a -> (junk s; fold f (f accum a) s)

  let to_list t =
    List.rev (fold (fun l b -> b::l) [] t)

  let map f s =
    let f _ =
      try Some (f (next s))
      with Failure -> None
    in from f

  let is_empty s =
    match peek s with None -> true | Some _ -> false

  let lines_of_channel cin =
    let f _ =
      try Some (input_line cin)
      with End_of_file -> None
    in Stream.from f
 
end


module Msg = Biocaml_msg
module Pos = Biocaml_pos

module Lines = struct

  exception Error of (Pos.t * string)
  let raise_error p m = raise (Error(p,m))

  let fold_stream' ?(file="") ?(strict=true) f init cstr =
    let lines = Stream.lines_of_chars cstr in
    let f accum s =
      try f accum s
      with Failure msg ->
        let n = Stream.count lines in
        let pos = if file = "" then Pos.l n else Pos.fl file n in
        if strict then raise_error pos msg else accum
    in
    Stream.fold f init lines

  let fold_stream ?(strict=true) f init cstr =
    fold_stream' ~strict f init cstr
      
  let fold_channel' ?(file="") ?(strict=true) f init cin =
    fold_stream' ~file ~strict f init (Stream.of_channel cin)
    
  let fold_channel ?(strict=true) f init cin =
    fold_stream ~strict f init (Stream.of_channel cin)

  let fold_file ?(strict=true) f init file =
    try 
      try_finally_exn (fold_channel' ~file ~strict f init)
        ~fend:close_in (open_in file)
    with Error (p,m) -> raise_error (Pos.set_file p file) m

  let iter_file ?(strict=true) f file =
    fold_file ~strict (fun _ x -> f x) () file

  let of_stream ?(strict=true) f (cstr : char Stream.t) =
    let lines = Stream.lines_of_chars cstr in
    let g ans s =
      try (f s)::ans
      with Failure m ->
        if strict
        then raise_error (Pos.l (Stream.count lines)) m
        else ans
    in List.rev (Stream.fold g [] lines)
  
  let of_channel ?(strict=true) f cin =
    of_stream ~strict f (Stream.of_channel cin)

end

module Set = struct
  module Make (Ord : BatSet.OrderedType) = struct
    module S = BatSet.Make(Ord)
    include S
    include S.Labels
    include S.Exceptionless
  end
end
module IntSet = struct 
  include BatSet.IntSet
  include BatSet.IntSet.Labels
  include BatSet.IntSet.Exceptionless
  let of_list l =
    of_enum (List.enum l)
  let to_list t =
    List.of_enum (enum t)
end

module Option = struct
  module M = struct
    include BatOption
    include BatOption.Labels
  end
  module With_monad = struct
    include M
    include BatOption.Monad
    let (>>=) = bind
  end
  include M
end

module PMap = struct
  include BatPMap
  include BatPMap.Exceptionless
end

module IO = BatIO

module StringMap = struct
  include BatMap.StringMap
  include BatMap.StringMap.Labels
  include BatMap.StringMap.Exceptionless

  let add_with x f m =
    add x (f (find x m)) m

  let size t = fold ~f:(fun ~key ~data ans -> ans + 1) ~init:0 t

end

module Tuple = struct
  include BatTuple
  module Pr = struct
    include Tuple2
    let make a b = (a,b)
  end
  module Tr = struct
    include Tuple3
    let make a b c = (a,b,c)
  end
end

module Order = struct
  let compose cmp1 cmp2 a b =
    let c1 = cmp1 a b in
    let c2 = cmp2 a b in
    let num_true = List.fold_left ~f:(fun cnt b -> if b then cnt+1 else cnt) ~init:0 in
    let bl = [c1 = Some (-1); c1 = Some 1; c2 = Some (-1); c2 = Some 1] in
    assert((num_true bl <= 1) || (c1 = Some 0 && c2 = Some 0));
    match c1 with
    | Some c1 -> c1
    | None ->
      match c2 with
      | Some c2 -> c2
      | None -> invalid_arg "neither partial order given relates given values"
        
  let reverse cmp a b = -(cmp a b)
  let reversep cmp a b = Option.map (~-) (cmp a b)

  let totalify cmp =
    fun a b ->
      match cmp a b with
      | Some c -> c
      | None -> failwith "order relation not defined for given elements"
end

module Test = struct
  let time ?(cout=stderr) f a =
    let init = Sys.time() in
    let b = f a in
    let delt = Sys.time() -. init in
    fprintf cout "finished in %.2f seconds\n%!" delt;
    b
      
  let sf ?(cout=stderr) msg f a =
    fprintf cout "%s... %!" msg;
    let b = f a in
    fprintf cout "finished\n%!";
    b
      
  let timesf ?(cout=stderr) msg f a =
    let init = Sys.time() in
    fprintf cout "%s... %!" msg;
    let b = f a in
    let delt = Sys.time() -. init in
    fprintf cout "finished in %.2f seconds\n%!" delt;
    b
      
  let get_time f a =
    let init = Sys.time() in
    let b = f a in
    let delt = Sys.time() -. init in
    (b, delt)
      
  let repeat n f =
    if n < 1 then failwith "cannot execute a function less than 1 time"
    else
      fun a ->
        for i = 1 to n-1 do ignore (f a) done;
        f a
end
