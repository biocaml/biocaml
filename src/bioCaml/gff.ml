open TylesBase
open Printf

exception Bad of string
let raise_bad msg = raise (Bad msg)

type strand = Sense | Antisense | Unknown | Unstranded
type attribute = TagValue of string * string | Something of string
type row = {chr:string; source:string; feature:string; pos:(int*int); score : float option; strand:strand; phase:int option; attributes : attribute list}
type t = row list
let fold = List.fold_left
let iter = List.iter
let to_list t = t

let get_attribute row attr_name =
  let is_attr_name = function TagValue (tg,_) -> attr_name = tg | Something _ -> false in
  let attrs = List.filter is_attr_name row.attributes in
  match List.length attrs with
    | 1 -> (
        match List.nth attrs 0 with
          | TagValue (_,v) -> v
          | Something _ -> assert false
      )
    | 0 -> failwith (sprintf "attribute %s not found" attr_name)
    | n -> failwith (sprintf "attribute %s occurs %d times" attr_name n)

let has_attribute row attr_name =
  let pred = function TagValue (x,_) -> x = attr_name | Something _ -> false in
  List.exists pred row.attributes

let set_attribute x y r =
  let got_set = ref false in
  let f attr = match attr with
    | TagValue (x',_) -> 
        if x' = x 
        then (got_set := true; TagValue(x,y))
        else attr
    | Something _ -> attr
  in
  let attributes = List.map f r.attributes in
  if !got_set then
    {r with attributes=attributes}
  else
    {r with attributes = attributes @ [TagValue(x,y)]}
  

type version = Two | Three

let make_version (x:int) : version =
  match x with
    | 2 -> Two
    | 3 -> Three
    | _ -> failwith (sprintf "invalid version: %d" x)


let row_to_string ?(version=3) r =
  let ver = make_version version in
  let sep = match ver with Two -> " " | Three -> "=" in
  let attribute = function
    | TagValue (tg,vl) -> sprintf "%s%s%s" tg sep vl
    | Something x -> x
  in
  let attributes l = String.concat ";" (List.map attribute l) in
  let strand = function
    | Sense -> "+"
    | Antisense -> "-"
    | Unstranded -> "."
    | Unknown -> "?"
  in
  let score = function None -> "." | Some x -> string_of_float x in
  let phase = function None -> "." | Some x -> string_of_int x in
  String.concat "\t" [
    r.chr;
    r.source;
    r.feature;
    string_of_int (fst r.pos);
    string_of_int (snd r.pos);
    score r.score;
    strand r.strand;
    phase r.phase;
    attributes r.attributes
  ]

let to_channel ?(version=3) t cout =
  let f r = output_string cout (row_to_string r); output_char cout '\n' in
  List.iter f t
    
let to_file ?(version=3) t file =
  try_finally (to_channel ~version t) close_out (open_out_safe file)
    
module Parser = struct
  let chr s = s
  let source s = s
  let feat s = s

  let phase s =
    if s = "." then
      None
    else
      try Some (int_of_string s)
      with Failure msg -> failwith (sprintf "%s: invalid phase %s" msg s)

  let interval s1 s2 =
    let lo =
      try int_of_string s1
      with Failure msg -> failwith (sprintf "%s: invalid start coordinate %s" msg s1)
    in
    let hi =
      try int_of_string s2
      with Failure msg -> failwith (sprintf "%s: invalid end coordinate %s" msg s2)
    in
    try ignore (Range.make lo hi); lo,hi
    with Range.Bad msg -> failwith msg
      
  let score (s:string) : float option =
    match s with
      | "." -> None
      | _ -> 
          try Some (float_of_string s)
          with Failure msg -> failwith (sprintf "%s: invalid score %s" msg s)
            
  let strand (s:string) : strand =
    match s with
      | "+" -> Sense
      | "-" -> Antisense
      | "." -> Unstranded
      | "?" -> Unknown
      | _ -> failwith (sprintf "invalid strand %s" s)
	  
  let attributes (ver:version) (s:string) : attribute list =
    let sep = match ver with Two -> " " | Three -> "=" in
    let attribute (s:string) : attribute =
      try
        let tg,vl = String.split s sep in
        let tg = String.strip tg in
        let vl = String.strip vl in
	TagValue (tg,vl)
      with
          ExtString.Invalid_string -> Something s
    in
    let ans = List.map attribute (List.map String.strip (String.nsplit s ";")) in
(*
    let tags = List.filter_map (function TagValue (x,_) -> Some x | Something _ -> None) ans in
    try failwith (sprintf "multiply defined attribute %s" (List.first_repeat tags))
    with Not_found -> ans
*)
    ans

  let row (ver:version) (s:string) : row option =
    let s = String.strip s in
    if String.length s > 0 && s.[0] = '#' then
      None
    else if String.for_all Char.is_space s then
      None
    else
      let sl = String.nsplit s "\t" in
      let nth = List.nth sl in
      let attributes_str = match List.length sl with
        | 9 -> nth 8
        | 8 -> ""
        | k -> failwith (sprintf "expecting 9 columns but found %d" k)
      in
      Some {
        chr = chr (nth 0);
        strand = strand (nth 6);
        pos = interval (nth 3) (nth 4);
        source = source (nth 1);
	feature = feat (nth 2);
        phase = phase (nth 7);
	score = score (nth 5);
	attributes = attributes ver attributes_str
      }
        
end

let of_file ?(version=3) ?(strict=true) file =
  let ver = make_version version in
  let f ans s =
    match Parser.row ver s with
      | None -> ans
      | Some x -> x::ans
  in
  try List.rev (Lines.fold_file ~strict f [] file)
  with Lines.Error(pos,msg) -> raise_bad (Msg.err ~pos msg)
    
let fold_file ?(version=3) ?(strict=true) f init file =
  let ver = make_version version in
  let g accum line =
    match Parser.row ver line with
      | None -> accum
      | Some x -> f accum x
  in
  try Lines.fold_file ~strict g init file
  with Lines.Error(pos,msg) -> raise_bad (Msg.err ~pos msg)

let iter_file ?(version=3) ?(strict=true) f file =
  let f _ r = f r in
  fold_file ~version ~strict f () file

let to_map t =
  let append _ (prev : row list option) r : row list =
    match prev with
      | None -> [r]
      | Some prev -> r::prev
  in
  let f ans r = StringMap.add_with append r.chr r ans in
  let ans = List.fold_left f StringMap.empty t in
  StringMap.map List.rev ans

let map_of_file ?(version=3) ?(strict=true) file =
  let append _ (prev : row list option) r : row list =
    match prev with
      | None -> [r]
      | Some prev -> r::prev
  in
  let f ans r = StringMap.add_with append r.chr r ans in
  let ans = fold_file ~version ~strict f StringMap.empty file in
  StringMap.map List.rev ans


(* ************************
module Attr = struct
  (* return number of attr's in list with given tag *)
  let num_tags (tg:string) (attrs: attr list) : int =
    let incr = function 
      | TagValue (tg',_) -> if tg' = tg then 1 else 0 
      | Something _ -> 0
    in
    List.fold_left (fun sum a -> incr a + sum) 0 attrs
      
  let syntactic_equal a b =
    match a,b with
      | (TagValue (a1,a2), TagValue (b1,b2)) -> a1 = b1 && a2 = b2
      | (Something a, Something b) -> a = b
      | (TagValue _, Something _)
      | (Something _, TagValue _) -> false
end
  
module Strand = struct
  let syntactic_equal a b =
    match a,b with (Sense,Sense) | (Antisense,Antisense) | (Unknown,Unknown) | (Unstranded,Unstranded) -> true | _ -> false
end

module Annt = struct
  let interval a = Range.make a.start a.finish
  let start a = a.start
  let finish a = a.finish
  let strand a = a.strand
  let chr a = a.chr
  let source a = a.source
  let typ a = a.typ
  let score a = a.score
  let attrs a = a.attrs
  let phase a =
    if a.typ = "CDS"
    then try Option.get a.phase with Option.No_value -> raise (Invalid_argument "BUG: no phase for annotation of type CDS")
    else raise (Failure "phase available only for annotations of type CDS")


  let syntactic_equal a b =
    (a.chr = b.chr)
    && (a.source = b.source)
    && (a.typ = b.typ)
    && (a.start = b.start)
    && (a.finish = b.finish)
    && (a.score = b.score)
    && (Strand.syntactic_equal a.strand b.strand)
    && (a.phase = b.phase)
    && (List.length a.attrs = List.length b.attrs && List.for_all2 Attr.syntactic_equal a.attrs b.attrs)

  (** Return None if given annotation is well-formed, or Some "msg" if not, where msg explains the error. For internal use only, annotations produced by constructors should be well-formed. *)
  let is_ok (a:annt) : string option =
    if a.start > a.finish then
      Some "start coordinate cannot be greater than finish coordinate"
    else if a.typ = "CDS" then
      match a.phase with
        | None -> Some "CDS annotations must be provided with a phase"
        | Some p ->
            if p = 0 || p = 1 || p = 2
            then None
            else Some ("invalid phase " ^ (string_of_int p))
    else
      None
end

module Tree = KeyTree.Make(struct
  type node = annt
  type key = string
  let compare_keys = String.compare
  let key_to_string s = s
  let key_of_node node =
    match Attr.num_tags "ID" node.attrs with
      | 1 -> Some (Annt.get_attr node "ID")
      | 0 -> None
      | _ -> raise (Failure "attribute ID occurs more than once in annotation")
end)
type t = Tree.t

****************** *)
