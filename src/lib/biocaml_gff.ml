open Biocaml_internal_pervasives

  (*
    Version 2:
    http://www.sanger.ac.uk/resources/software/gff/spec.html
    http://gmod.org/wiki/GFF2

    Version 3:
    http://www.sequenceontology.org/gff3.shtml
    http://gmod.org/wiki/GFF3
  *)

type t = {
  seqname: string;
  source: string option;
  feature: string option;
  pos: int * int;

  score: float option;
  strand: [`plus | `minus | `not_applicable | `unknown ];
  phase: int option;
  attributes: (string * string) list;
}

type stream_item = [ `comment of string | `record of t ]

type parse_error = 
[ `cannot_parse_float of Biocaml_pos.t * string
| `cannot_parse_int of Biocaml_pos.t * string
| `cannot_parse_strand of Biocaml_pos.t * string
| `cannot_parse_string of Biocaml_pos.t * string
| `empty_line of Biocaml_pos.t
| `incomplete_input of
    Biocaml_pos.t * string list * string option
| `wrong_attributes of Biocaml_pos.t * string
| `wrong_row of Biocaml_pos.t * string
| `wrong_url_escaping of Biocaml_pos.t * string ]

open Result

let url_escape s =
  let b = Buffer.create (String.length s) in
  String.iter s (function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' as c -> Buffer.add_char b c
  | anyother -> Buffer.add_string b (sprintf "%%%02X" (Char.to_int anyother)));
  Buffer.contents b

let url_unescape pos s =
  let buf = Buffer.create (String.length s) in
  let rec loop pos = 
    match String.lfindi s ~pos ~f:(fun _ c -> (=) '%' c) with
    | None ->
      Buffer.add_substring buf s pos String.(length s - pos)
    | Some idx ->
      if String.length s >= idx + 2 then (
        let char = Scanf.sscanf (String.sub s (idx + 1) 2) "%x" ident in
        Buffer.add_substring buf s pos String.(idx - pos);
        Buffer.add_char buf (Char.of_int_exn char);
        loop (idx + 3)
      ) else (
        failwith "A"
      )
  in
  try loop 0; Ok (Buffer.contents buf) with
  | e -> Error (`wrong_url_escaping (pos, s))
  
let parse_string msg pos i =
  begin try Ok (Scanf.sscanf i "%S " ident) with
  | e ->
    begin match (Scanf.sscanf i "%s " ident) with
    | "" -> Error (`cannot_parse_string (pos, msg))
    | s -> url_unescape pos s
    end
  end
let parse_string_opt m pos i =
  parse_string m pos i >>= fun s ->
  begin match s with
  | "." -> return None
  | s -> return (Some s)
  end

let parse_int msg pos i =
  parse_string msg pos i >>= fun s ->
  (try return (Int.of_string s)
   with e -> fail (`cannot_parse_int (pos, msg)))
    
let parse_float_opt msg pos i =
  parse_string_opt msg pos i >>= function
  | Some s ->
    (try return (Some (Float.of_string s))
     with e -> fail (`cannot_parse_float (pos, msg)))
  | None -> return None
  
let parse_int_opt msg pos i =
  parse_string_opt msg pos i >>= function
  | Some s ->
    (try return (Some (Int.of_string s))
     with e -> fail (`cannot_parse_int (pos, msg)))
  | None -> return None
    
let parse_attributes_version_3 position i =
  let whole_thing = String.concat ~sep:"\t" i in
  (*   let b = Buffer.create 42 in *)
  (*   String.iter (String.concat ~sep:"\t" i) (function *)
  (*   | ' ' -> Buffer.add_string b "%20" *)
  (*   | c -> Buffer.add_char b c); *)
  (*   Buffer.contents b *)
  (* in *)
  let rec loop pos acc =
    begin match String.lfindi whole_thing ~pos ~f:(fun _ c -> c = '=') with
    | Some equal ->
      parse_string "tag" position (String.slice whole_thing pos equal)
      >>= fun tag ->
      let pos = equal + 1 in
      begin match String.lfindi whole_thing ~pos ~f:(fun _ c -> c = ';') with
      | Some semicolon ->
        let delimited = String.slice whole_thing pos semicolon in
        parse_string "value" position delimited >>= fun value ->
        loop (semicolon + 1) ((tag, value) :: acc)
      | None ->
        let delimited = String.(sub whole_thing pos (length whole_thing - pos)) in
        parse_string "value" position delimited >>= fun value ->
        return ((tag, value) :: acc)
      end
    | None ->
      if pos >= String.length whole_thing then
        return acc
      else
        fail (`wrong_attributes (position, whole_thing))
    end
  in
  (try loop 0 [] with e -> fail (`wrong_attributes (position, whole_thing)))
  >>| List.rev

let parse_attributes_version_2 position l =
  let whole_thing = String.(concat ~sep:"\t" l |! strip) in
  let parse_string i =
    begin try Some (Scanf.bscanf i "%S " ident) with
    | e ->
      begin match (Scanf.bscanf i "%s " ident) with
      | "" -> None
      | s -> Some s
      end
    end
  in
  let inch = Scanf.Scanning.from_string whole_thing in
  let tokens = Stream.(from (fun _ -> parse_string inch) |! npeek max_int) in
  let rec go_3_by_3 acc = function
    | k  :: v :: ";" :: rest -> go_3_by_3 ((k, v) :: acc) rest
    | [] | [";"] -> return (List.rev acc)
    | problem -> fail (`wrong_attributes (position, whole_thing))
  in
  go_3_by_3 [] tokens

  
let parse_row ~version pos s =
  let output_result = function  Ok o -> `output o | Error e -> `error e in
  let fields = String.split ~on:'\t' s in
  begin match fields with
  | seqname :: source :: feature :: start :: stop :: score :: strand :: phase
    :: rest ->
    let result =
      parse_string "Sequence name" pos seqname >>= fun seqname ->
      parse_string_opt "Source" pos source >>= fun source ->
      parse_string_opt "Feature" pos feature >>= fun feature ->
      parse_int "Start Position" pos start >>= fun start ->
      parse_int "Stop Position" pos stop >>= fun stop ->
      parse_float_opt "Score" pos score >>= fun score ->
      parse_string_opt "Strand" pos strand
      >>= (function
      | Some "+" -> return `plus
      | None -> return `not_applicable
      | Some "-" -> return `minus
      | Some "?" -> return `unknown
      | Some s -> fail (`cannot_parse_strand (pos, s)))
      >>= fun strand ->
      parse_int_opt "Phase/Frame" pos phase >>= fun phase ->
      begin match version with
      | `two -> parse_attributes_version_2 pos rest
      | `three -> parse_attributes_version_3 pos rest
      end
      >>= fun attributes ->
      return (`record {seqname; source; feature; pos = (start, stop); score;
                       strand; phase; attributes})
    in
    output_result result

  | other ->
    `error (`wrong_row (pos, s))
  end
  
let rec next ?(pedantic=true) ?(sharp_comments=true) ?(version=`three) p =
  let open Biocaml_transform.Line_oriented in
  let open Result in
  match next_line p with
  | None -> `not_ready
  | Some "" ->
    if pedantic then `error (`empty_line (current_position p)) else `not_ready
  | Some l when sharp_comments && String.(is_prefix (strip l) ~prefix:"#") ->
    `output (`comment String.(sub l ~pos:1 ~len:(length l - 1)))
  | Some l -> parse_row ~version (current_position p) l

let parser ?filename ?pedantic ?version () =
  let name = sprintf "gff_parser:%s" Option.(value ~default:"<>" filename) in
  let module LOP =  Biocaml_transform.Line_oriented  in
  let lo_parser = LOP.parser ?filename () in
  Biocaml_transform.make_stoppable ~name ()
    ~feed:(LOP.feed_string lo_parser)
    ~next:(fun stopped ->
      match next ?pedantic ?version lo_parser with
      | `output r -> `output r
      | `error e -> `error e
      | `not_ready ->
        if stopped then (
          match LOP.finish lo_parser with
          | `ok -> `end_of_stream
          | `error (l, o) ->
            `error (`incomplete_input (LOP.current_position lo_parser, l, o))
        ) else
          `not_ready)
    
    
let printer () =
  let module PQ = Biocaml_transform.Printer_queue in
  let printer =
    PQ.make () ~to_string:(function
    | `comment c -> sprintf "#%s\n" c
    | `record t ->
      let optescape o = Option.value_map ~default:"." o ~f:url_escape in
      String.concat ~sep:"\t" [
        url_escape t.seqname;
        optescape t.source;
        optescape t.feature;
        sprintf "%d" (fst t.pos);
        sprintf "%d" (snd t.pos);
        Option.value_map ~default:"." ~f:(sprintf "%g") t.score;
        (match t.strand with`plus -> "+" | `minus -> "-"
        | `not_applicable -> "." | `unknown -> "?");
        Option.value_map ~default:"." ~f:(sprintf "%d") t.phase;
        String.concat ~sep:";"
          (List.map t.attributes (fun (k,v) ->
            sprintf "%s=%s" (url_escape k) (url_escape v)));
      ] ^ "\n"
    ) in
  Biocaml_transform.make_stoppable ~name:"gff_printer" ()
    ~feed:(fun r -> PQ.feed printer r)
    ~next:(fun stopped ->
      match (PQ.flush printer) with
      | "" -> if stopped then `end_of_stream else `not_ready
      | s -> `output s)
(*
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
  try_finally_exn (to_channel ~version t) ~fend:close_out (open_out_safe file)
    
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
  StringMap.iter ~f:(fun ~key ~data -> Hashtbl.add tbl key (List.rev data)) map;
  tbl
*)
