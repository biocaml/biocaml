open Biocaml_std
open Tuple


exception Bad of string
let raise_bad msg = raise (Bad msg)

type bt = (int * int * float) list StringMap.t
    (* list items are (lo,hi,x) triples *)
    
type vt = {vspan:int; vdata : (int * float) list StringMap.t}
    (* list items are (lo,x) pairs, range is [lo, lo + vspan - 1] *)
    
type ft = {fspan:int; fdata : (int * int * float list) StringMap.t}
    (* For each chromosome there is a list of (start,step,xl) triples.
       For the i'th value in xl, the associated range is [lo,hi] where
       - lo = start + i*step
       - hi = lo + fspan - 1
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
      | Some step -> Some (fst (List.hd_exn l), step, List.map snd l)
  in
  
  let g ~key ~data:l ans =
    match ans with
      | None -> None
      | Some ans ->
          match convert_one l with
            | None -> None
            | Some dat -> Some (StringMap.add key dat ans)
  in
  match StringMap.fold ~f:g vt.vdata ~init:(Some StringMap.empty) with
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
    let spans = StringMap.fold ~f:(fun ~key ~data:l ans -> (get_span_chr l)::ans) bt ~init:[] in
    if List.length spans = 0 then
      None (* if no chromosomes, do not compact *)
    else if List.exists Option.is_none spans then
      None
    else
      let spans = List.map Option.get spans in
      let span = List.hd_exn spans in
      if List.for_all ((=) span) spans then
        Some span
      else
        None
  in
  match get_span bt with
    | None -> None
    | Some span ->
        let vdat = StringMap.map (List.map ~f:Tr.prj13) bt in
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
      let h ~key:chr ~data:l = List.iter (g chr) l in
      StringMap.iter ~f:h t
  | V t ->
      let g chr (lo,x) = f(chr, lo, lo + t.vspan - 1, x) in
      let h ~key:chr ~data:l = List.iter (g chr) l in
      StringMap.iter ~f:h t.vdata
  | F t ->
      let g chr start step i x =
        let lo = start + i*step in
        let hi = lo + t.fspan - 1 in
        f(chr,lo,hi,x)
      in
      let h ~key:chr ~data:(start,step,l) = List.iteri (g chr start step) l in
      StringMap.iter h t.fdata
        
let fold f init t =
  let ans = ref init in
  iter (fun pt -> ans := f !ans pt) t;
  !ans

let rec of_list l =
  let f ans (chr,lo,hi,x) =
    if lo > hi then
      raise_bad (sprintf "invalid interval [%d, %d]" lo hi)
    else
      let append prev = match prev with None -> [lo,hi,x] | Some l -> (lo,hi,x)::l in
      StringMap.add_with chr append ans
  in
  let ans = List.fold_left ~f ~init:StringMap.empty l in
  let ans = StringMap.map List.rev ans in
  compact (B ans)
    
let of_channel ?fmt ?(chr_map=identity) ?(header=true) ?increment_lo_hi ic = 
  let of_bed_channel chr_map header increment_lo_hi ic =
    let inclo,inchi = increment_lo_hi in
    let f acc l = 
      let lst = String.nsplit l "\t" in
      match List.length lst with
        | 4 ->
            let chr = chr_map (List.nth lst 0) in
            let lo = int_of_string (List.nth lst 1) + inclo in
            let hi = int_of_string (List.nth lst 2) + inchi in
            let v = float_of_string (List.nth lst 3) in
            let insert prev = match prev with None -> [lo,hi,v] | Some l -> (lo,hi,v)::l in
            StringMap.add_with chr insert acc
        | n -> failwith (sprintf "expecting exactly 4 columns but found %d" n) 
    in
    if header then (ignore (input_line ic));
    let ans = Lines.fold_channel f StringMap.empty ic in
    let ans = StringMap.map List.rev ans in
    compact (B ans)
  in
  match fmt with
    | None -> failwith "must specify WIG file format" (* TO DO: infer format automatically *)
    | Some Bed ->
        let increment_lo_hi = match increment_lo_hi with Some x -> x | None -> (1,0) in
        of_bed_channel chr_map header increment_lo_hi ic
    | Some VariableStep -> failwith "parsing of WIG files in variable-step format not yet implemented"
    | Some FixedStep -> failwith "parsing of WIG files in fixed-step format not yet implemented"
        
let of_file ?fmt ?(chr_map=identity) ?(header=true) ?increment_lo_hi file =
  let fend = close_in in
  match fmt,increment_lo_hi with
    | None, None ->
      try_finally_exn (of_channel ~chr_map ~header) ~fend (open_in file)
    | Some fmt, None ->
        try_finally_exn (of_channel ~fmt ~chr_map ~header) ~fend (open_in file)
    | None, Some increment_lo_hi ->
        try_finally_exn (of_channel ~chr_map ~header ~increment_lo_hi) ~fend (open_in file)
    | Some fmt, Some increment_lo_hi->
        try_finally_exn (of_channel ~fmt ~chr_map ~header ~increment_lo_hi) ~fend (open_in file)

let to_list t = List.rev (fold (fun ans pt -> pt::ans) [] t)
  
let to_channel ?fmt t cout =
  let print_as_is = function
    | B bt ->
        let f chr (lo,hi,x) = fprintf cout "%s\t%d\t%d\t%s\n" chr (lo-1) hi (string_of_float x) in
        let g ~key:chr ~data:l = List.iter (f chr) l in
        StringMap.iter g bt
    | V vt ->
        let g ~key:chr ~data:l =
          fprintf cout "variableStep\tchrom=%s\tspan=%d\n" chr vt.vspan;
          let f (lo,x) = fprintf cout "%d\t%s\n" lo (string_of_float x) in
          List.iter f l
        in
        StringMap.iter g vt.vdata
    | F ft ->
        let g ~key:chr ~data:(start,step,l) =
          fprintf cout "fixedStep\tchrom=%s\tstart=%d\tstep=%d\tspan=%d\n" chr start step ft.fspan;
          List.iter (fun x -> fprintf cout "%s\n" (string_of_float x)) l
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
  try_finally_exn f ~fend:close_out (open_out_safe file)


(**** Some parsing functions that could be useful when support for parsing variable-step and fixed-step are added.
module V = struct
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
          
  let datum_of_string (s:string) : datum =
    let sl = split_ws s in
    let nth = List.nth sl in
    match List.length sl with
      | 2 -> 
          let lo = stoi (nth 0) in
          let x = stof (nth 1) in
          if lo < 1 then raise_bad (sprintf "start coordinate %d must be >= 1" lo);
          lo,x
      | n -> raise_bad (sprintf "expecting exactly 2 columns but found %d" n)
          
end

module F = struct
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
          
end
*)

(***** Old Parser, not working. ****
(** split tag=value *)
let split_eq (s:string) : string * string =
  let sl = Str.split_delim (Str.regexp "=") s in
  let nth = List.nth sl in
  match List.length sl with
    | 2 -> nth 0, nth 1
    | _ -> raise_bad (sprintf "expected exactly one '=' sign in: %s" s)

type line_type = BLine | VHeaderLine | VLine | FHeaderLine | FLine

let line_type s =
  try ignore (B.datum_of_string s); Some BLine with Bad _ ->
  try ignore (V.header_of_string s); Some VHeaderLine with Bad _ ->
  try ignore (V.datum_of_string s); Some VLine with Bad _ ->
  try ignore (F.header_of_string s); Some FHeaderLine with Bad _ ->
  try ignore (F.datum_of_string s); Some FLine with Bad _ ->
  None

module Parser = struct
  type state = Start | InB of B.s | InV of V.s | InF of F.s
    (* states of the finite-state-machine parser *)

  exception Errors of (line_type * string) list
    (* indicates a list of possible errors for why a parse failed,
       each message is associated with the line_type that the parse was attempted on *)

  let errors_to_string (msgs : (line_type * string) list) : string =
    let line_type (lt:line_type) : string =
      let s = match lt with
        | BLine -> "bed"
        | VLine -> "variable step"
        | VHeaderLine -> "variable step header"
        | FLine -> "fixed step"
        | FHeaderLine -> "fixed step header"
      in sprintf ("- bad %s line: ") s
    in

    let error_to_string (lt,msg) = (line_type lt) ^ msg in
    let msgs = List.map error_to_string msgs in
    let msgs = "parse failed for one of the following reasons:"::msgs in
    String.concat "\n" msgs

  (* [add_line st line] adds [line] to [st] and returns the new state, raise [Errors] if any errors *)
  let add_line (st:state) (line:string) : state = 
    let msgs = ref [] in
    let raise_errors () = raise (Errors (List.rev !msgs)) in
    match st with
      | Start -> (
          try InB (B.singleton (B.datum_of_string line)) with Bad m ->
          try msgs := (BLine,m)::!msgs; InV (V.empty (V.header_of_string line)) with Bad m ->
          try msgs := (VHeaderLine,m)::!msgs; InF (F.empty (F.header_of_string line)) with Bad m ->
          msgs := (FHeaderLine,m)::!msgs; 
          raise_errors()
        )
      | InB s -> (
          try InB (B.append_datum s (B.datum_of_string line)) with Bad m ->
          msgs := (BLine,m)::!msgs;
          raise_errors()
        )
      | InV s -> (
          try InV (V.append_datum s (V.datum_of_string line)) with Bad m ->
          try msgs := (VLine,m)::!msgs; InV (V.set_header s (V.header_of_string line)) with Bad m ->
          msgs := (VHeaderLine,m)::!msgs;
          raise_errors()
        )
      | InF s -> (
          try InF (F.append_datum s (F.datum_of_string line)) with Bad m ->
          try msgs := (FLine,m)::!msgs; InF (F.set_header s (F.header_of_string line)) with Bad m ->
          msgs := (FHeaderLine,m)::!msgs;
          raise_errors()
        )

  let of_channel cin =
    let line_num = ref 0 in
    let ans = ref Start in
    try
      while true do
        incr line_num;
        ans := add_line !ans (input_line cin)
      done;
      assert false
    with
      | End_of_file -> (
          match !ans with
            | Start -> raise_bad "no data"
            | InB s -> B.complete s
            | InV s -> V.complete s
            | InF s -> F.complete s
        )
      | Errors msgs -> raise_bad (Msg.err ~pos:(Pos.l !line_num) (errors_to_string msgs))
      | Bad _ -> assert false (* all Bad should have been caught by add_line *)

  let of_stream (strm : char Stream.t) =
    let lines = Stream.lines_of_chars strm in
    try
      let ans = Stream.fold add_line Start lines in
      match ans with
        | Start -> raise_bad "no data"
        | InB s -> B.complete s
        | InV s -> V.complete s
        | InF s -> F.complete s
    with
      | Errors msgs -> raise_bad (Msg.err ~pos:(Pos.l (Stream.count lines)) (errors_to_string msgs))
      | Bad _ -> assert false (* all Bad should have been caught by add_line *)
       
end

let of_channel = Parser.of_channel
let of_stream = Parser.of_stream
let of_file file = try_finally of_channel close_in (open_in file)

******************)

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
