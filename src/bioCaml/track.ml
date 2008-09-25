open TylesBase
open Tuple
open Printf

module TrackLine = struct
  module Map = StringMap
  type t = string Map.t

  exception Bad of string

  let raise_bad msg = raise (Bad msg)
  let get t attr = try Some (Map.find attr t) with Not_found -> None
  let empty = Map.empty
  let to_list t = Map.fold (fun x y ans -> (x,y)::ans) t []
  let unset t attr = Map.remove attr t
  let find t attr = Map.find attr t
  let mem t a x = try x = find t a with Not_found -> false

  (* error messages *) 
  let err attr value msg = sprintf "%s=%s is invalid: %s" attr value msg    
  let too_long k = sprintf "length exceeds %d characters" k
  let illegal_chars = "contains illegal characters"
  let quotes_reqd = "must be enclosed in quotes when includes space"
    
  let quote_enclosed s = (* if s contains space, then must be quote enclosed *)
    if String.exists' ((=) ' ') s
    then String.length s >= 3 && s.[0] = '"' && s.[String.length s - 1] = '"'
    else true
      
  let good_chars s = (* s must contain only alphanumeric characters, spaces, or double quotes *)
    String.for_all (fun c -> Char.is_alpha_num c || Char.is_space c || c = '"') s 
      
  let is_int s = try ignore (int_of_string s); true with Failure _ -> false
  let is_float s = try ignore (float_of_string s); true with Failure _ -> false
    
  let is_rgb s = (* s must be in form R,G,B *)
    let sl = String.nsplit s "," in
    let is_color s =
      try let x = int_of_string s in 0 <= x && x <= 255
      with Failure _ -> false
    in
    (List.length sl = 3) && (List.for_all is_color sl)
      
  let validate_type t =
    match get t "type" with 
      | None | Some "wiggle_0" -> () 
      | Some y -> raise_bad (err "type" y "must be unset or set to \"wiggle_0\"")

  let validate_name t =    
    match get t "name" with
      | None -> ()
      | Some y ->
          let err = err "name" y in
          if String.length y > 15 then raise_bad (err (too_long 15));
          if not (good_chars y) then raise_bad (err illegal_chars);
          if not (quote_enclosed y) then raise_bad (err quotes_reqd)

  let validate_description t =    
    match get t "description" with
      | None -> ()
      | Some y ->
          let err = err "description" y in
          if String.length y > 60 then raise_bad (err (too_long 60));
          if not (quote_enclosed y) then raise_bad (err quotes_reqd)
            
  let validate_visibility t =     
    match get t "visibility" with
      | None -> ()
      | Some y ->
          let msg = "must be \"hide\", \"dense\", \"full\", \"pack\", \"squish\", or integer from integer from 0 - 4" in
          let msg_wig = "must be \"hide\", \"dense\", \"full\", or integer from 0 - 2 for wiggle track" in
          match get t "type" with
            | None -> (match y with "hide" | "dense" | "full" | "pack" | "squish" | "0" | "1" | "2" | "3" | "4" | "5" -> () | _ -> raise_bad (err "visibility" y msg))
            | Some "wiggle_0" -> (match y with "hide" | "dense" | "full" | "0" | "1" | "2" -> () | _ -> raise_bad (err "visibility" y msg_wig))
            | Some _ -> invalid_arg (Msg.bug "validate_type supposed to be called before validate_visibility")

  let validate_color t =
    match get t "color" with 
      | None -> ()
      | Some y -> if not (is_rgb y) then raise_bad (err "color" y "must be in R,G,B format")

  let validate_altColor t =    
    match get t "altColor" with 
      | None -> ()
      | Some y -> if not (is_rgb y) then raise_bad (err "altColor" y "must be in R,G,B format")

  let validate_itemRgb t =
    match get t "itemRgb" with
      | None | Some "On" -> ()
      | Some y -> raise_bad (err "itemRgb" y "only allowed value is \"On\"")

  let validate_useScore t =
    match get t "useScore" with
      | None | Some "0" | Some "1" -> ()
      | Some y -> raise_bad (err "useScore" y "must be 0 or 1")

  let validate_group t = ()
    
  let validate_priority t =
    match get t "priority" with
      | None -> ()
      | Some y -> if not (is_int y) then raise_bad (err "priority" y "must be an integer")
            
  let validate_autoScale t =    
    match get t "autoScale" with
      | None -> ()
      | Some y ->
          match y with 
            | "on" | "off" -> () 
            | _ -> raise_bad (err "autoScale" y "must be \"on\" or \"off\"")
                
  let validate_gridDefault t =  
    match get t "gridDefault" with
      | None -> ()
      | Some y -> 
          match y with 
            | "on" | "off" -> () 
            | _ -> raise_bad (err "gridDefault" y "must be \"on\" or \"off\"")

  let validate_maxHeightPixels t =          
    match get t "maxHeightPixels" with
      | None -> ()
      | Some y ->
          let sl = String.nsplit y ":" in
          if not ((List.length sl = 3) && (List.for_all is_int sl)) 
          then raise_bad (err "maxHeightPixels" y "must be in form max:default:min")
            
  let validate_graphType t =
    match get t "graphType" with
      | None -> ()
      | Some y -> 
          match y with 
            | "bar" | "points" -> ()
            | _ -> raise_bad (err "graphType" y "must be \"bar\" or \"points\"")

  let validate_viewLimits t =
    match get t "viewLimits" with
      | None -> ()
      | Some y ->
          let sl = String.nsplit y ":" in
          if not ((List.length sl = 2) && (List.for_all is_int sl))
          then raise_bad (err "viewLimits" y "must be in form lower:upper")

  let validate_yLineMark t =
    match get t "yLineMark" with
      | None -> ()
      | Some y -> if not (is_float y) then raise_bad (err "yLineMark" y "must be float value")

  let validate_yLineOnOff t =
    match get t "yLineOnOff" with
      | None -> ()
      | Some y -> 
          match y with 
            | "on" | "off" -> () 
            | _ -> raise_bad (err "yLineOnOff" y "must be \"on\" or \"off\"")

  let validate_windowingFunction t =
    match get t "windowingFunction" with
      | None -> ()
      | Some y -> 
          match y with 
            | "maximum" | "mean" | "minimum" -> ()
            | _ -> raise_bad (err "windowingFunction" y "must be \"on\" or \"off\"")
                
  let validate_smoothingWindow t =
    match get t "smoothingWindow" with
      | None -> ()
      | Some y ->
          let err = err "smoothingWindow" y "must be \"off\" or integer between 2 and 16" in
          if y = "off" then
            ()
          else
            try
              let y = int_of_string y in
              if not (2 <= y && y <= 16) then raise_bad err
            with Failure _ -> raise_bad err

  let validate_db t = ()

  let validate_offset t =
    match get t "offset" with
      | None -> ()
      | Some y -> if not (is_int y) then raise_bad (err "offset" y "must be an integer")

  let validate_url t = ()
  let validate_htmlUrl t = ()

  let validators =
    [
      validate_type;
      validate_name;
      validate_description;
      validate_visibility;
      validate_color;
      validate_altColor;
      validate_itemRgb;
      validate_useScore;
      validate_group;
      validate_priority;
      validate_autoScale;
      validate_gridDefault;
      validate_maxHeightPixels;
      validate_graphType;
      validate_viewLimits;
      validate_yLineMark;
      validate_yLineOnOff;
      validate_windowingFunction;
      validate_smoothingWindow;
      validate_db;
      validate_offset;
      validate_url;
      validate_htmlUrl
    ]
      
  let validate t = List.iter (fun f -> f t) validators
    
  let set t x y =
    let ans = Map.add x y t in
    validate ans;
    ans

  let of_list l = List.fold_left (fun t (a,x) -> set t a x) empty l

  let valid_name s =
    let s = if String.starts_with s "\"" then String.lchop s else s in
    let s = if String.ends_with s "\"" then String.rchop s else s in
    let f c l = if Char.is_alpha_num c then c::l else l in
    let cl = String.fold_right f s [] in
    let cl = List.take 15 cl in
    String.implode cl

  let to_string t =
    (* if there is a type attribute print it first *)
    let t, ans =
      match get t "type" with
        | None -> t, "track"
        | Some y -> unset t "type", "track type=" ^ y
    in
    Map.fold (fun x y ans -> ans ^ " " ^ x ^ "=" ^ y) t ans 
      

  type state = NotInQuote | InQuote | InSpaces
  let split_on_spaces (s:string) : string list =
    let n = String.length s in
    let finish (curr : char list) = String.implode (List.rev curr) in
    let rec loop i ans curr st =
      if i = n then
        ans,curr,st
      else
        let loop = loop (i+1) in
        let c = s.[i] in
        if c = '\n' then raise_bad "newline characters not allowed in track lines";
        match st with
          | NotInQuote -> (
              match c with 
                | ' ' | '\r' | '\t' -> loop ((finish curr)::ans) [] InSpaces
                | '"' -> loop ans (c::curr) InQuote
                | _ -> loop ans (c::curr) NotInQuote
            )
          | InQuote -> (
              match c with
                | '"' -> loop ans (c::curr) NotInQuote
                | _ -> loop ans (c::curr) InQuote
            )
          | InSpaces -> (
              match c with
                | ' ' | '\r' | '\t' -> loop ans [] InSpaces
                | '"' -> loop ans [c] InQuote
                | _ -> loop ans [c] NotInQuote              
            )
    in
    let ans,curr,st = loop 0 [] [] NotInQuote in
    match st with
      | NotInQuote -> List.rev ((finish curr)::ans)
      | InQuote -> raise_bad "unmatched quote"
      | InSpaces -> List.rev (""::ans)
          
  let split_on_equal (s:string) : string * string =
    let sl = String.nsplit s "=" in
    match List.length sl with
      | 2 -> List.nth sl 0, List.nth sl 1
      | _ -> raise_bad "expecting exactly one equal sign"
          
  let of_string s =
    let sl = split_on_spaces s in
    if List.hd sl <> "track" then raise_bad "expecting keyword \"track\"";
    let sl = List.map split_on_equal (List.tl sl) in
    let ans = List.fold_left (fun ans (x,y) -> set ans x y) empty sl in
    validate ans;
    ans
end

module BrowserLines = struct
  type mode = Hide | Dense | Pack | Squish | Full
  type line = Position of string * int * int | Mode of mode * string list
  type t = line list
      
  exception Bad of string
  let raise_bad msg = raise (Bad msg)
    
  let mode_to_string = function
    | Hide -> "hide" | Dense -> "dense" | Pack -> "pack" | Squish -> "squish" | Full -> "full"
        
  let validate t =
    let length pred = List.length (List.filter pred t) in
    if length (function Position _ -> true | Mode _ -> false) > 1 then raise_bad "only one position line allowed";
    let check_mode m =
      let pred m = function Mode (m',_) -> m = m' | Position _ -> false in
      if length (pred m) > 1 then raise_bad (sprintf "only one %s line allowed" (mode_to_string m))
    in
    List.iter check_mode [Hide; Dense; Pack; Squish; Full]
      
  let concat t1 t2 = let ans = t1@t2 in validate ans; ans

  let position chr lo hi =
    if lo >= hi then
      raise_bad (sprintf "start %d not less than end %d" lo hi)
    else
      [Position (chr,lo,hi)]
        
  let mode_helper s = (* split string on spaces *)
    if String.exists' ((=) '\n') s then raise_bad "newline characters not allowed in browser lines";
    let sl = Str.split (Str.regexp "[ \t\r]+") s in
    if List.length sl > 1 && List.exists ((=) "all") sl 
    then raise_bad "cannot include \"all\" keyword within list of track names"
    else sl
      
  let hide s = [Mode (Hide, mode_helper s)]
  let dense s = [Mode (Dense, mode_helper s)]
  let pack s = [Mode (Pack, mode_helper s)]
  let squish s = [Mode (Squish, mode_helper s)]
  let full s = [Mode (Full, mode_helper s)]
    
  let of_string s =
    let parse_line s =
      let sl = Str.bounded_split (Str.regexp "[ \t\r]+") s 3 in
      if List.length sl < 3 then 
        raise_bad "browser line should be in form \"browser attribute_name attribute_value(s)\""
      else if List.hd sl <> "browser" then
        raise_bad "browser line must begin with keyword \"browser\""
      else
        match List.nth sl 1 with
          | "position" ->
              let msg = "browser position must be in form chr:start-end" in (
                try
                  let chr,lo_hi = String.split (List.nth sl 2) ":" in
                  let lo,hi = String.split lo_hi "-" in
                  position chr (int_of_string lo) (int_of_string hi)
                with
                  | ExtString.Invalid_string | Failure _ -> raise_bad msg
              )
          | "hide" -> hide (List.nth sl 2)
          | "dense" -> dense (List.nth sl 2)
          | "pack" -> pack (List.nth sl 2)
          | "squish" -> squish (List.nth sl 2)
          | "full" -> full (List.nth sl 2)
          | _ -> raise_bad ("invalid browser line: " ^ s)
    in
    
    let sl = Str.split_delim (Str.regexp "\n") s in
    let ans = List.concat (List.map parse_line sl) in
    validate ans;
    ans
      
  let to_string t =
    let line_to_string = function
      | Position (chr,lo,hi) -> sprintf "browser position %s:%d-%d" chr lo hi
      | Mode (m,sl) -> sprintf "browser %s %s" (mode_to_string m) (String.concat " " sl)
    in
    String.concat "\n" (List.map line_to_string t)

end

type block =
    | B of BrowserLines.t
    | T of TrackLine.t
    | C of CommentLines.t
    | Wig of Wig.t
    | Bed of Bed.t
        
type t = block list

exception Bad of string
let raise_bad msg = raise (Bad msg)

let to_list t = t

let to_channel t cout =
  let print_string s = output_string cout s; output_char cout '\n' in
  let print_block = function
    | B x -> print_string (BrowserLines.to_string x)
    | T x -> print_string (TrackLine.to_string x)
    | C x -> print_string (CommentLines.to_string x)
    | Wig x -> Wig.to_channel x cout
    | Bed x -> Bed.to_channel x cout
  in
  List.iter print_block t

let to_file t file =
  try_finally (to_channel t) close_out (open_out_safe file)
    
let validate t =
  let t = List.filter (function C _ -> false | _ -> true) t in

  let is_data = function | B _ | T _ | C _ -> false | Wig _ | Bed _ -> true in
  
  let rec browser_lines_first t =
    let rec loop non_b_seen = function
      | [] -> ()
      | (B _)::t ->
          if non_b_seen 
          then raise_bad "browser lines must come first"
          else loop false t
      | _::t -> loop true t
    in loop false t
  in

  let rec track_then_data t =
    let msg = "track line must be followed by data" in
    match t with
      | [] -> ()
      | (T _)::[] -> raise_bad msg
      | (T _)::b::t -> if is_data b then track_then_data t else raise_bad msg
      | _::t -> track_then_data t
  in

  let num_track_data t =
    let tl = List.filter (function T _ -> true | _ -> false) t in
    let dl = List.filter is_data t in
    match List.length tl, List.length dl with
      | _,0 -> raise_bad "no data"
      | (0|1),1 -> ()
      | n,m -> if n <> m then raise_bad "when providing multiple tracks, exactly one track line must precede each data section"
  in

  let rec wig_track_type t =
    match t with
      | [] -> ()
      | (T trk)::(Wig _)::t ->
          if not (TrackLine.mem trk "type" "wiggle_0") then
            raise_bad "track type preceding WIG data must set type=wiggle_0"
          else
            wig_track_type t
      | (Wig _)::_
      | _::(Wig _)::_ -> raise_bad "WIG data requires preceding track line"
      | _::t -> wig_track_type t
  in
  browser_lines_first t;
  track_then_data t;
  num_track_data t;
  wig_track_type t
    
let of_list bl = validate bl; bl

