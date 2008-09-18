open TylesBase
open Printf

exception Bad of string
let raise_bad msg = raise (Bad msg)

type strand = Sense | Antisense | Unknown | Unstranded
type attribute = TagValue of string * string | Something of string
type row = {chr:string; source:string; feature:string; pos:Range.t; score : float option; strand:strand; phase:int option; attributes : attribute list}
type 'a t = 'a list

let to_list t = t

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
    try Range.make lo hi
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
	  
  (* attribute list in version 3 format *)
  let attributes3 (s:string) : attribute list =
    let attribute3 (s:string) : attribute =
      let sl = String.nsplit s "=" in
      match List.length sl with
        | 0 | 1 -> Something s
        | 2 ->
            let tg = String.strip (List.nth sl 0) in
            let vl = String.strip (List.nth sl 1) in
	    TagValue (tg,vl)
        | _ -> failwith (sprintf "invalid tag-value pair %s" s)
    in
    List.map attribute3 (List.map String.strip (String.nsplit s ";"))
	
  let row (s:string) : row option =
    let s = String.strip s in
    if String.length s > 2 && s.[0] = '#' && s.[1] = '#' then
      None
    else
      let sl = String.nsplit s "\t" in
      let nth = List.nth sl in
      Some (
        match List.length sl with
          | 9 ->
              {chr = chr (nth 0);
               strand = strand (nth 6);
               pos = interval (nth 3) (nth 4);
               source = source (nth 1);
	       feature = feat (nth 2);
               phase = phase (nth 7);
	       score = score (nth 5);
	       attributes = attributes3 (nth 8)
	      }
          | k -> failwith (sprintf "expecting 9 columns but found %d" k)
      )
        
end

let of_file p file =
  let f ans s =
    match Parser.row s with
      | None -> ans
      | Some row -> match p row with None -> ans | Some x -> x::ans
  in
  try List.rev (Lines.fold_file f [] file)
  with Lines.Error(pos,msg) -> raise_bad (Msg.err ~pos msg)
    
let fold_file f init file =
  let g accum line =
    match Parser.row line with
      | None -> accum
      | Some v -> f accum v
  in
  try Lines.fold_file ~strict:false g init file
  with Lines.Error(pos,msg) -> raise_bad (Msg.err ~pos msg)



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

  let get_attr a attr_name =
    let is_attr_name = function TagValue (tg,_) -> attr_name = tg | Something _ -> false in
    let attrs = List.filter is_attr_name a.attrs in
    let l = List.length attrs in
      if l = 1 then
        (match List.nth attrs 0 with
          | TagValue (_,v) -> v
          | Something _ -> raise (Invalid_argument "Impossible to get here") )
      else if l = 0 then
        raise (Failure ("attribute " ^ attr_name ^ " not found in annotation"))
      else
        raise (Failure ("attribute " ^ attr_name ^ " occurs more than once in annotation"))          

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
