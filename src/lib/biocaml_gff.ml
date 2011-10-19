open Biocaml_std

module Range = Biocaml_range

exception Bad of string
let raise_bad msg = raise (Bad msg)

type strand = Sense | Antisense | Unknown | Unstranded
type attribute = TagValue of string * string | Something of string
type row = {chr:string; source:string; feature:string; pos:(int*int); score : float option; strand:strand; phase:int option; attributes : attribute list}
type t = row list
let fold f init l = List.fold_left ~f ~init l
let iter f l = List.iter ~f l
let to_list t = t
let enum t = List.enum t

let strip_quotes s =
  let n = String.length s in
  if n >= 2 && s.[0] = '"' && s.[n-1] = '"'
  then String.slice ~first:1 ~last:(n-1) s
  else s
    
let attribute_names row =
  let rec loop ans = function
    | [] -> ans
    | (Something _)::l -> loop ans l
    | (TagValue(x,_))::l -> loop ((strip_quotes x)::ans) l
  in List.rev (loop [] row.attributes)

let get_attributel row attr_name =
  let f attr = match attr with
    | TagValue (x,y) -> if x = attr_name then Some (strip_quotes y) else None
    | Something _ -> None
  in
  List.filter_map f row.attributes
    
let get_attribute row attr_name =
  match get_attributel row attr_name with
    | [] -> failwith (sprintf "attribute %s undefined" attr_name)
    | x::[] -> x
    | _ -> failwith (sprintf "attribute %s multiply defined" attr_name)
        
let has_attribute row attr_name =
  let pred = function TagValue (x,_) -> x = attr_name | Something _ -> false in
  List.exists pred row.attributes

let add_attribute x y r =
  {r with attributes = r.attributes @ [TagValue(x,y)]}
  
let delete_attribute x r =
  let pred attr = match attr with
    | TagValue (x',_) -> x <> x'
    | Something _ -> true
  in
  {r with attributes = List.filter pred r.attributes}

let set_attribute x y r =
  add_attribute x y (delete_attribute x r)
    
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
      match String.split s sep with
      | Some (tg,vl) ->
        let tg = String.strip tg in
        let vl = String.strip vl in
	TagValue (tg,vl)
      | None -> Something s
    in
    List.map attribute (List.map String.strip (String.nsplit s ";"))
      
  let row (chr_map : string -> string) (ver:version) (s:string) : row option =
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
        chr = chr_map (nth 0);
        strand = strand(nth 6);
        pos = interval (nth 3) (nth 4);
        source = nth 1;
	feature = nth 2;
        phase = phase (nth 7);
	score = score (nth 5);
	attributes = attributes ver attributes_str
      }
        
end

let of_list xl = xl

let of_file ?(chr_map=identity) ?(version=3) ?(strict=true) file =
  let ver = make_version version in
  let f ans s =
    match Parser.row chr_map ver s with
      | None -> ans
      | Some x -> x::ans
  in
  try List.rev (Lines.fold_file ~strict f [] file)
  with Lines.Error(pos,msg) -> raise_bad (Msg.err ~pos msg)
    
let fold_file ?(version=3) ?(strict=true) f init file =
  let ver = make_version version in
  let g accum line =
    match Parser.row identity ver line with
      | None -> accum
      | Some x -> f accum x
  in
  try Lines.fold_file ~strict g init file
  with Lines.Error(pos,msg) -> raise_bad (Msg.err ~pos msg)

let iter_file ?(version=3) ?(strict=true) f file =
  let f _ r = f r in
  fold_file ~version ~strict f () file

let to_map t =
  let append r (prev : row list option) : row list =
    match prev with
      | None -> [r]
      | Some prev -> r::prev
  in
  let f ans r = StringMap.add_with r.chr (append r) ans in
  let ans = List.fold_left ~f ~init:StringMap.empty t in
  StringMap.map List.rev ans

let map_of_file ?(version=3) ?(strict=true) file =
  let append r (prev : row list option) : row list =
    match prev with
      | None -> [r]
      | Some prev -> r::prev
  in
  let f ans r = StringMap.add_with r.chr (append r) ans in
  let ans = fold_file ~version ~strict f StringMap.empty file in
  StringMap.map List.rev ans

let index_by_attribute attr t =
  let get_vals r attr = match attr with
    | "CHR" -> [r.chr]
    | "SOURCE" -> [r.source]
    | "FEATURE" -> [r.feature]
    | _ -> get_attributel r attr
  in
  let f ans r =
    let ys = get_vals r attr in
    let append prev = match prev with None -> [r] | Some l -> r::l in
    List.fold_left ~f:(fun ans y -> StringMap.add_with y append ans) ~init:ans ys
  in
  let map = fold f StringMap.empty t in
  let tbl = Hashtbl.create (StringMap.size map) in
  StringMap.iter (fun x y -> Hashtbl.add tbl x (List.rev y)) map;
  tbl
