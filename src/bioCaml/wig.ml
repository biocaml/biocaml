open TylesBase
open Printf
open Tuple

exception Bad of string
let raise_bad msg = raise (Bad msg)

(** String conversions. *)
let stoi (s:string) : int = try int_of_string s with Failure _ -> raise_bad (sprintf "%s is not an int" s)
let stof (s:string) : float = try float_of_string s with Failure _ -> raise_bad (sprintf "%s is not a float" s)
  
(** split on whitespace *)
let split_ws = Str.split (Str.regexp "[ \t\r]+")

(** split tag=value *)
let split_eq (s:string) : string * string =
  let sl = Str.split_delim (Str.regexp "=") s in
  let nth = List.nth sl in
  match List.length sl with
    | 2 -> nth 0, nth 1
    | _ -> raise_bad (sprintf "expected exactly one '=' sign in: %s" s)

type bt = (int * int * float) list StringMap.t
    (* list items are (lo,hi,x) triples *)
    
type vt = {vspan:int; vdata : (int * float) list StringMap.t}
    (* list items are (lo,x) pairs, range is [lo, lo + vspan - 1] *)
    
type ft = {fspan:int; fdata : (int * int * float list) StringMap.t}
    (* For each chromosome there is a list of (start,step,xl) triples.
       For the i'th value in xl, the associated range is [lo,hi] where
       - lo = start + i*step
       - hi = lo + span - 1
    *)
    
type t = B of bt | V of vt | F of ft
type pt = string * int * int * float
type format = Bed | VariableStep | FixedStep
  
let get_format = function B _ -> Bed | V _ -> VariableStep | F _ -> FixedStep

(* compact variable-step to fixed-step if possible *)
let v_to_f vt : ft option =
  (* get the constant step or return None if step is not constant *)
  let get_step (l : (int * float) list) : int option =
    let rec f step = function
      | [] | _::[] -> Some step
      | (lo1,_)::((lo2,_)::_ as l) ->
          if step = lo2 - lo1 then f step l else None
    in
    match l with
      | [] -> assert false
      | _::[] -> Some vt.vspan (* arbitrarily choosing step to equal span *)
      | (lo0,_)::(lo1,_)::_ -> f (lo1 - lo0) l
  in
  
  let convert_one (l : (int * float) list) : (int * int * float list) option =
    match get_step l with
      | None -> None
      | Some step -> Some (fst (List.hd l), step, List.map snd l)
  in
  
  let g chr l ans =
    match ans with
      | None -> None
      | Some ans ->
          match convert_one l with
            | None -> None
            | Some dat -> Some (StringMap.add chr dat ans)
  in
  match StringMap.fold g vt.vdata (Some StringMap.empty) with
    | None -> None
    | Some fdat -> Some {fspan = vt.vspan; fdata = fdat}

(* compact bed to variable-step if possible *)
let b_to_v bt : vt option =
  (* get the constant span or return None if span is not constant *)
  let get_span_chr (l : (int * int * float) list) : int option =
    let rec f span = function
      | [] -> Some span
      | (lo,hi,_)::l -> if span = hi - lo + 1 then f span l else None
    in
    match l with
      | [] -> assert false
      | (lo,hi,_)::l -> f (hi - lo + 1) l
  in
  
  let get_span bt : int option =
    let spans = StringMap.fold (fun _ l ans -> (get_span_chr l)::ans) bt [] in
    if List.length spans = 0 then
      None (* if no chromosomes, do not compact *)
    else if List.exists Option.is_none spans then
      None
    else
      let spans = List.map Option.get spans in
      let span = List.hd spans in
      if List.for_all ((=) span) spans then
        Some span
      else
        None
  in
  match get_span bt with
    | None -> None
    | Some span ->
        let vdat = StringMap.map (List.map Tr.prj13) bt in
        Some {vspan=span; vdata = vdat}
          
(* compact bed to fixed-step if possible *)
let b_to_f bt : ft option =
  match b_to_v bt with None -> None | Some vt -> v_to_f vt

(* compact as much as possible *)
let rec compact t = match t with
  | B x -> (match b_to_v x with None -> t | Some vt -> compact (V vt))
  | V x -> (match v_to_f x with None -> t | Some ft -> F ft)
  | F x -> F x

(* expand variable-step to bed *)
let v_to_b vt : bt =
  let f l = List.map (fun (lo,x) -> lo, lo + vt.vspan - 1, x) l in
  StringMap.map f vt.vdata

(* expand fixed-step to variable-step *)
let f_to_v ft : vt =
  let f (start,step,xl) = List.mapi (fun i x -> start + i*step, x) xl in
  {vspan = ft.fspan; vdata = StringMap.map f ft.fdata}

(* expand fixed-step to bed *)
let f_to_b ft : bt =
  let f (start,step,l) = 
    let g i x =
      let lo = start + i*step in
      let hi = lo + ft.fspan - 1 in
      lo,hi,x
    in
    List.mapi g l
  in
  StringMap.map f ft.fdata
    
(* convert [t] to [fmt] if possible *)
let to_format fmt t : t option = match fmt,t with
  | Bed, B _ -> Some t
  | Bed, V vt -> Some (B (v_to_b vt))
  | Bed, F ft -> Some (B (f_to_b ft))
  | VariableStep, B bt -> (match b_to_v bt with None -> None | Some vt -> Some (V vt))
  | VariableStep, V _ -> Some t
  | VariableStep, F ft -> Some (V (f_to_v ft))
  | FixedStep, B bt -> (match b_to_f bt with None -> None | Some ft -> Some (F ft))
  | FixedStep, V vt -> (match v_to_f vt with None -> None | Some ft -> Some (F ft))
  | FixedStep, F _ -> Some t

let iter f = function 
  | B t -> 
      let g chr (lo,hi,x) = f(chr,lo,hi,x) in
      let h chr l = List.iter (g chr) l in
      StringMap.iter h t
  | V t ->
      let g chr (lo,x) = f(chr, lo, lo + t.vspan - 1, x) in
      let h chr l = List.iter (g chr) l in
      StringMap.iter h t.vdata
  | F t ->
      let g chr start step i x =
        let lo = start + i*step in
        let hi = lo + t.fspan - 1 in
        f(chr,lo,hi,x)
      in
      let h chr (start,step,l) = List.iteri (g chr start step) l in
      StringMap.iter h t.fdata
        
let fold f init t =
  let ans = ref init in
  iter (fun pt -> ans := f !ans pt) t;
  !ans
    

(* BED format *)
module B = struct
  type datum = string * int * int * float

  type s = (int * int * float) list StringMap.t
      (* List is ordered in reverse by coordinate. *)         
      
  let validate_datum (_,lo,hi,_) = 
    if lo > hi then raise_bad (sprintf "invalid range [%d, %d)" lo (hi+1));
    if lo < 0 then raise_bad (sprintf "start coordinate %d must be >= 0" lo)
      
  (* return unit if first point can be followed by second, assume they are on same chromosome *)
  let can_append (lo1,hi1,_) (lo2,hi2,_) =
    if hi1 >= lo2 then raise_bad (sprintf "range [%d, %d] cannot be followed by [%d, %d]" lo1 hi1 lo2 hi2)
      
  let datum_of_string s =
    let sl = split_ws s in
    let nth = List.nth sl in
    match List.length sl with
      | 4 -> 
          let chr = nth 0 in
          let lo = stoi (nth 1) in
          let hi = stoi (nth 2) - 1 in (* specification requires half-open intervals [lo,hi) *)
          let x = stof (nth 3) in
          let d = (chr,lo,hi,x) in
          validate_datum d;
          d
      | n -> raise_bad (sprintf "expecting exactly 4 columns but found %d" n)
          
  let empty = StringMap.empty

  let append_datum s ((chr,lo2,hi2,x2) as d) =
    validate_datum d;
    try
      let l = StringMap.find chr s in
      match l with
        | [] -> StringMap.add chr [(lo2,hi2,x2)] s
        | (lo1,hi1,x1)::_ ->
            can_append (lo1,hi1,x1) (lo2,hi2,x2);
            StringMap.add chr ((lo2,hi2,x2)::l) s
    with
        Not_found -> StringMap.add chr [(lo2,hi2,x2)] s
          
  let singleton = append_datum empty
    
  let datum_to_string (chr,lo,hi,x) =
    String.concat "\t" [chr; string_of_int lo; string_of_int (hi+1); string_of_float x]
      
  let complete s = 
    let ans = B (StringMap.map List.rev s) in
    compact ans
      
end

(* variable-step format *)
module V = struct
  type header = string * int (* header gives chromosome name and span >= 1 *)
  type datum = int * float  (* data point is a low coordinate >= 0 and a value *)
          
  type s = {
    spanv : int;
    datav : datum list StringMap.t; (* list items stored in descending order by coordinate *)
    curr_chrv : string (* current chromosome under which datum should be added *)
  }

  let header_of_string (s:string) : header =
    let sl = split_ws s in
    let nth = List.nth sl in
    match List.length sl with
      | 3 ->
          if nth 0 <> "variableStep" then raise_bad "expecting keyword \"variableStep\"";
          let tag,value = split_eq (nth 1) in
          let chr =
            if (tag = "chrom") then value
            else raise_bad "second field must be in form chrom=string"
          in
          let tag,value = split_eq (nth 2) in
          let span =
            if (tag = "span") then stoi value
            else raise_bad "third field must be in form span=integer"
          in
          chr,span
      | n -> raise_bad (sprintf "expecting exactly 3 space separated fields but found %d" n)
          
  let datum_of_string s =
    let sl = split_ws s in
    let nth = List.nth sl in
    match List.length sl with
      | 2 -> 
          let lo = stoi (nth 0) - 1 in (* specification uses 1-based indexing, we convert to 0-based *)
          let x = stof (nth 1) in
          if lo < 0 then raise_bad (sprintf "start coordinate %d must be >= 1" (lo+1));
          lo,x
      | n -> raise_bad (sprintf "expecting exactly 2 columns but found %d" n)
          
  (** Start a data set with first header. *)
  let empty (chr,span) = {spanv=span; curr_chrv=chr; datav = StringMap.add chr [] StringMap.empty}
    
  let set_header s (chr,span) =
    if span <> s.spanv then raise_bad (sprintf "previously span = %d, cannot change to %d" s.spanv span);
    let f chr' l = 
      if chr = chr' then
        raise_bad (sprintf "data section for chromosome %s previously provided" chr)
      else if List.length l = 0 then
        raise_bad (sprintf "no data provided for chromosome %s" chr')
    in
    StringMap.iter f s.datav;
    {spanv=span; curr_chrv=chr; datav = StringMap.add chr [] s.datav}
      
  let append_datum s (lo2,x2) =
    if lo2 < 0 then raise_bad (sprintf "start coordinate %d must be >= 0" lo2);
    try
      let l = StringMap.find s.curr_chrv s.datav in
      match l with
        | [] -> {s with datav = StringMap.add s.curr_chrv [(lo2,x2)] s.datav}
        | (lo1,_)::_ ->
            let hi1 = lo1 + s.spanv - 1 in
            let hi2 = lo2 + s.spanv - 1 in
            if hi1 >= lo2 then raise_bad (sprintf "based on span %d, we have range [%d, %d] followed by [%d, %d] which is not allowed" s.spanv lo1 hi1 lo2 hi2);
            {s with datav = StringMap.add s.curr_chrv ((lo2,x2)::l) s.datav}
    with
        Not_found -> failwith (Msg.bug (sprintf "given StringMap should have had entry for chromosome %s" s.curr_chrv))
          
  let complete s =
    try
      let l = StringMap.find s.curr_chrv s.datav in
      if List.length l = 0 then raise_bad (sprintf "no data provided for chromosome %s" s.curr_chrv);
      let ans = V {vspan = s.spanv; vdata = StringMap.map List.rev s.datav} in
      compact ans
    with
        Not_found -> failwith (Msg.bug (sprintf "given StringMap should have had entry for chromosome %s" s.curr_chrv))
          
  let header_to_string (chr,span) = sprintf "variableStep\tchrom=%s\tspan=%d" chr span
  let datum_to_string (lo,x) = sprintf "%d\t%f" (lo+1) x

end
  
(* fixed-step format *)  
module F = struct
  type header = string * int * int * int (* header gives chromosome, start, step, and span *)
  type datum = float (* a data point is just a float *)

  type s = {
    spanf:int;
    dataf : (int * int * datum list) StringMap.t; (* list items in reverse *)
    curr_chrf : string
  }

  let header_of_string s =
    let sl = split_ws s in
    let nth = List.nth sl in
    match List.length sl with
      | 5 ->
          if nth 0 <> "fixedStep" then raise_bad "expecting keyword \"fixedStep\"";
          let tag,value = split_eq (nth 1) in
          let chr =
            if (tag = "chrom") then value
            else raise_bad "second field must be in form chrom=string"
          in
          let tag,value = split_eq (nth 2) in
          let start =
            if (tag = "start") then (stoi value) - 1
            else raise_bad "third field must be in form start=integer"
          in
          let tag,value = split_eq (nth 3) in
          let step =
            if (tag = "step") then stoi value
            else raise_bad "fourth field must be in form step=integer"
          in
          let tag,value = split_eq (nth 4) in
          let span = 
            if (tag = "span") then stoi value
            else raise_bad "fifth field must be in form span=integer"
          in
          if start < 0 then raise_bad (sprintf "start=%d but minimum value is 1" (start+1));
          if step < 0 then raise_bad (sprintf "step=%d but must be positive" step);
          if span < 0 then raise_bad (sprintf "span=%d but must be positive" span);
          if step < span then raise_bad (sprintf "step=%d cannot be less than span=%d" step span);
          chr,start,step,span
      | n -> raise_bad (sprintf "expecting exactly 5 space separated fields but found %d" n)
          
  let datum_of_string = stof
    
  (** Start a data set with first header. *)
  let empty (chr,start,step,span) = 
    {spanf=span; curr_chrf=chr; dataf = StringMap.add chr (start,step,[]) StringMap.empty}

  let set_header s (chr,start,step,span) =
    if span <> s.spanf then raise_bad (sprintf "previously span = %d, cannot change to %d" s.spanf span);
    let f chr' (_,_,l) =
      if chr = chr' then
        raise_bad (sprintf "data section for chromosome %s previously provided" chr)
      else if List.length l = 0 then
        raise_bad (sprintf "no data provided for chromosome %s" chr')
    in
    StringMap.iter f s.dataf;
    {spanf=span; curr_chrf=chr; dataf = StringMap.add chr (start,step,[]) s.dataf}
      
  let append_datum s x2 =
    try
      let start,step,xl = StringMap.find s.curr_chrf s.dataf in
      match List.length xl with
        | 0 -> {s with dataf = StringMap.add s.curr_chrf (start,step,[x2]) s.dataf}
        | n ->
            let i1 = n - 1 in
            let i2 = i1 + 1 in
            let lo1 = start + i1*step in
            let hi1 = lo1 + s.spanf - 1 in
            let lo2 = start + i2*step in
            let hi2 = lo2 + s.spanf - 1 in
            if hi1 >= lo2 then raise_bad (sprintf "based on start %d, step %d, span %d, we have range [%d, %d] on datum %d followed by [%d, %d] on datum %d which is not allowed" start step s.spanf lo1 hi1 i1 lo2 hi2 i2);
            {s with dataf = StringMap.add s.curr_chrf (start,step,x2::xl) s.dataf}
    with
        Not_found -> failwith (Msg.bug (sprintf "given StringMap should have had entry for chromosome %s" s.curr_chrf))
          
  let complete s =
    try
      let _,_,l = StringMap.find s.curr_chrf s.dataf in
      if List.length l = 0 then raise_bad (sprintf "no data provided for chromosome %s" s.curr_chrf);
      let ans = F {fspan = s.spanf; fdata = StringMap.map (fun (a,b,l) -> a, b, List.rev l) s.dataf} in
      compact ans
    with
        Not_found -> failwith (Msg.bug (sprintf "given StringMap should have had entry for chromosome %s" s.curr_chrf))

  let header_to_string (chr,start,step,span) = 
    sprintf "fixedStep\tchrom=%s\tstart=%d\tstep=%d\tspan=%d" chr start step span

  let datum_to_string = string_of_float

end
  
let rec of_list ?(sort=false) l =
  if not sort then
    (compact <<- B.complete <<- (List.fold_left B.append_datum B.empty)) l
  else
    let cmp (chr1,lo1,_,_) (chr2,lo2,_,_) =
      let c = compare chr1 chr2 in
      if c <> 0 then c
      else compare lo1 lo2
    in
    of_list (List.sort ~cmp l)
      
let to_list t = List.rev (fold (fun ans pt -> pt::ans) [] t)
  
let to_channel ?fmt t cout =
  let print_as_is = function
    | B bt ->
        let f chr (lo,hi,x) = fprintf cout "%s\t%d\t%d\t%f\n" chr lo (hi+1) x in
        let g chr l = List.iter (f chr) l in
        StringMap.iter g bt
    | V vt ->
        let f (lo,x) = fprintf cout "%d\t%f\n" (lo+1) x in
        let g chr l =
          fprintf cout "variableStep\tchrom=%s\tspan=%d\n" chr vt.vspan;
          List.iter f l
        in
        StringMap.iter g vt.vdata
    | F ft ->
        let g chr (start,step,l) =
          fprintf cout "fixedStep\tchrom=%s\tstart=%d\tstep=%d\tspan=%d\n" chr (start+1) step ft.fspan;
          List.iter (fprintf cout "%f\n") l
        in
        StringMap.iter g ft.fdata
  in
  match fmt with
    | None -> print_as_is (compact t)
    | Some fmt -> match to_format fmt t with
        | None ->
            let fmt = match fmt with Bed -> "bed" | VariableStep -> "variable-step" | FixedStep -> "fixed-step" in
            failwith (sprintf "given data cannot be represented in %s format" fmt)
        | Some t -> print_as_is t
            
let to_file ?fmt t file =
  let f = match fmt with None -> to_channel t | Some fmt -> to_channel ~fmt t in
  try_finally f close_out (open_out_safe file)
    

(* Don't think I need this anymore. ***********
let first_item t : pt option =
  match t with
    | B t ->
        if StringMap.is_empty t then
          None
        else
          let chr,l = StringMap.first t in
          if List.length l = 0 then
            None
          else
            let lo,hi,x = List.hd l in
            Some (chr,lo,hi,x)
    | V t ->
        if StringMap.is_empty t.vdata then
          None
        else
          let chr,l = StringMap.first t.vdata in
          if List.length l = 0 then
            None
          else
            let lo,x = List.hd l in
            Some (chr, lo, lo + t.vspan - 1, x)
    | F t ->
        if StringMap.is_empty t.fdata then
          None
        else
          let chr,(start,step,l) = StringMap.first t.fdata in
          if List.length l = 0 then
            None
          else
            Some (chr, start, start + t.fspan - 1, List.hd l)
        
let validate = function
  | B t -> (
      match first_item t with
        | None -> ()
        | Some first ->
            is_range first;
            let prev = ref first in
            let f (curr : pt) =
              is_range curr;
              can_follow !prev curr;
              prev := curr
            in
            iter f t
    )
  | V t -> (
      if t.span < 1 then raise_bad (sprintf "invalid span %d" t.span);
      match first_item t with
        | None -> ()
        | Some first ->
            let prev = ref first in
            let f (curr : pt) =
              can_follow !prev curr;
              prev := curr
            in
            iter f t
    )
  | F t -> (
      if t.span < 1 then raise_bad (sprintf "invalid span %d" t.span);
      let f chr (_,step,_) =
        if step < t.span 
        then raise_bad (sprintf "for chromosome %s, step = %d is less than span = %d" chr step t.span)
      in
      StringMap.iter f t.dat
    )

   ************ *)
