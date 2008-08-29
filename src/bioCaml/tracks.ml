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
  
exception Bad of string
let raise_bad msg = raise (Bad msg)
  
type block =
    | B of BrowserLines.t
    | T of TrackLine.t
    | C of Comments.t
    | Wig of Wig.t
    | Bed of Bed.t
        
type t = block list
let to_list t = t

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
  browser_lines_first t;
  track_then_data t;
  num_track_data t
    
let of_list bl = validate bl; bl
let map = List.map
let map_wig f = map (function Wig x -> Wig (f x) | b -> b)
let map_bed f = map (function Bed x -> Bed (f x) | b -> b)
  
(** states of the finite state machine parser *)
type state =
    | StartFile             (** need to start parsing file *)
    | InB of BrowserLines.t (** parsing browser lines *)
    | StartTrack1           (** need to start parsing first track *)
    | StartTrack2           (** need to start parsing second or later track *)
    | AfterT of TrackLine.t * Comments.t option
                            (** last block parsed was a track line, possibly followed by comments *)
    | InWigB of Wig.B.s     (** parsing bed formatted wig data *)
    | InWigV of Wig.V.s     (** parsing variable step formatted wig data *)
    | InWigF of Wig.F.s     (** parsing fixed step formatted wig data *)
    | InBed of Bed.s        (** parsing bed data *)
  
exception Error of int * string (* line number and message *)
exception Eof of int * block list * state (* raised at end-of-file with line number, block list in reverse, and state *) 

(** Return unit if can parse string as any kind of WIG line. Raise [Wig.Bad] otherwise. *)
let any_wig_line (l:string) : unit =
  ignore (Wig.B.datum_of_string l);
  ignore (Wig.V.header_of_string l);
  ignore (Wig.V.datum_of_string l);
  ignore (Wig.F.header_of_string l);
  ignore (Wig.F.datum_of_string l)

let end_InB ans b = (B b)::ans
let end_AfterT ans (trk,c) = match c with None -> (T trk)::ans | Some c -> (C c)::(T trk)::ans
let end_InWigB ans dat = (Wig (Wig.B.complete dat))::ans
let end_InWigV ans dat = (Wig (Wig.V.complete dat))::ans
let end_InWigF ans dat = (Wig (Wig.F.complete dat))::ans
let end_InBed ans dat = (Bed (Bed.complete dat))::ans

let parse (cin:in_channel) : t =
  let rec loop line_num (ans : block list) (st:state) : unit =
    let loop = loop (line_num + 1) in
    let raise_error msg = raise (Error(line_num, msg)) in
    let bline_err ()  = raise_error "browser lines must occur at beginning of file without intervening comments" in
    let mix_wig_err () = raise_error "cannot mix WIG format types" in

    let raise_errorl (l : string) (msgs : string list ref) =
      let msgs = List.map (fun s -> "- " ^ s) (List.rev !msgs) in
      let msg = String.concat "\n" (l::msgs) in
      raise_error msg
    in
     
    let do_StartFile l =
      let msgs = ref [] in
      let ans,st =
        try (C (Comments.of_string l))::ans, st with Comments.Bad m ->
        try msgs := m::!msgs; ans, InB (BrowserLines.of_string l) with BrowserLines.Bad m ->
        try msgs := m::!msgs; ans, AfterT (TrackLine.of_string l, None) with TrackLine.Bad m ->
        try msgs := m::!msgs; ans, InWigB (Wig.B.singleton (Wig.B.datum_of_string l)) with Wig.Bad m ->
        try msgs := m::!msgs; ans, InWigV (Wig.V.empty (Wig.V.header_of_string l)) with Wig.Bad m ->
        try msgs := m::!msgs; ans, InWigF (Wig.F.empty (Wig.F.header_of_string l)) with Wig.Bad m ->
        try msgs := m::!msgs; ans, InBed (Bed.singleton (Bed.of_line l)) with Bed.Bad m ->
        msgs := m::!msgs; raise_errorl ("unrecognized line " ^ l) msgs
      in loop ans st
    in

    let do_InB b l =
      let msgs = ref [] in
      let ans,st =
        try ans, InB (BrowserLines.concat b (BrowserLines.of_string l)) with BrowserLines.Bad m ->
        try msgs := m::!msgs; end_InB ans b, AfterT (TrackLine.of_string l, None) with TrackLine.Bad m ->
        try msgs := m::!msgs; (C (Comments.of_string l))::(end_InB ans b), StartTrack1 with Comments.Bad m ->
        try msgs := m::!msgs; end_InB ans b, InWigB (Wig.B.singleton (Wig.B.datum_of_string l)) with Wig.Bad m ->
        try msgs := m::!msgs; end_InB ans b, InWigV (Wig.V.empty (Wig.V.header_of_string l)) with Wig.Bad m ->
        try msgs := m::!msgs; end_InB ans b, InWigF (Wig.F.empty (Wig.F.header_of_string l)) with Wig.Bad m ->
        try msgs := m::!msgs; end_InB ans b, InBed (Bed.singleton (Bed.of_line l)) with Bed.Bad m ->
        msgs := m::!msgs;
        raise_errorl ("unrecognized line " ^ l) msgs
      in loop ans st
    in

    let do_StartTrack1 l =
      let msgs = ref [] in
      let ans,st =
        try ignore (BrowserLines.of_string l); bline_err() with BrowserLines.Bad _ ->
        try ans, InWigB (Wig.B.singleton (Wig.B.datum_of_string l)) with Wig.Bad m ->
        try msgs := m::!msgs; ans, InWigV (Wig.V.empty (Wig.V.header_of_string l)) with Wig.Bad m ->
        try msgs := m::!msgs; ans, InWigF (Wig.F.empty (Wig.F.header_of_string l)) with Wig.Bad m ->
        try msgs := m::!msgs; ans, InBed (Bed.singleton (Bed.of_line l)) with Bed.Bad m ->
        try msgs := m::!msgs; (C (Comments.of_string l))::ans, st with Comments.Bad m ->
        try msgs := m::!msgs; ans, AfterT (TrackLine.of_string l, None) with TrackLine.Bad m ->
        msgs := m::!msgs; raise_errorl ("unrecognized line " ^ l) msgs
      in loop ans st
    in

    let do_StartTrack2 l =
      let msgs = ref [] in
      let ans,st =      
        try ans, AfterT (TrackLine.of_string l, None) with TrackLine.Bad m ->
        try msgs := m::!msgs; (C (Comments.of_string l))::ans, st with Comments.Bad m ->
        try msgs := m::!msgs; ignore (BrowserLines.of_string l); bline_err() with BrowserLines.Bad _ ->
        raise_errorl ("expecting track or comment line: " ^ l) msgs
      in loop ans st
    in

    let do_AfterT (trk,c) l =
      let msgs = ref [] in
      let ans,st =
        try ignore (BrowserLines.of_string l); bline_err() with BrowserLines.Bad _ ->
        try ignore (TrackLine.of_string l); raise_error "multiple track lines not allowed" with TrackLine.Bad _ ->
        try
          let c' = Comments.of_string l in
          let c = match c with None -> Some c' | Some c -> Some (Comments.concat c c') in
          ans, AfterT (trk,c)
        with Comments.Bad m ->
          msgs := m::!msgs;
          let is_wiggle = try TrackLine.find trk "type" = "wiggle_0" with Not_found -> false in
          if is_wiggle then
            try end_AfterT ans (trk,c), InWigB (Wig.B.singleton (Wig.B.datum_of_string l)) with Wig.Bad m ->
            try msgs := m::!msgs; end_AfterT ans (trk,c), InWigV (Wig.V.empty (Wig.V.header_of_string l)) with Wig.Bad m ->
            try msgs := m::!msgs; end_AfterT ans (trk,c), InWigF (Wig.F.empty (Wig.F.header_of_string l)) with Wig.Bad m ->
            try msgs := m::!msgs; ignore (Bed.of_line l); raise_error "BED data not allowed after track line with type=wiggle_0" with Bed.Bad _ ->
            raise_errorl ("expecting comment or WIG line: " ^ l) msgs
          else
            try end_AfterT ans (trk,c), InBed (Bed.singleton (Bed.of_line l)) with Bed.Bad m ->
            try msgs := m::!msgs; ignore (any_wig_line l); raise_error "track line before WIG data must set type=wiggle_0" with Wig.Bad _ ->
            raise_errorl ("expecting comment or non-WIG data line: " ^ l) msgs
      in loop ans st
    in

    let do_InWigB dat l = 
      let msgs = ref [] in
      let ans,st =
        try ans, InWigB (Wig.B.append_datum dat (Wig.B.datum_of_string l)) with Wig.Bad m ->
        try msgs := m::!msgs; end_InWigB ans dat, AfterT (TrackLine.of_string l, None) with TrackLine.Bad m ->
        try msgs := m::!msgs; (C (Comments.of_string l))::(end_InWigB ans dat), StartTrack2 with Comments.Bad m ->
        try msgs := m::!msgs; ignore (BrowserLines.of_string l); bline_err() with BrowserLines.Bad _ ->
        try any_wig_line l; mix_wig_err() with Wig.Bad _ ->
        try ignore (Bed.of_line l); raise_error "found BED data while parsing WIG data" with Bed.Bad _ ->
        raise_errorl ("expecting bed formatted WIG data, track or comment line: " ^ l) msgs
      in loop ans st
    in

    let do_InWigV dat l = 
      let msgs = ref [] in
      let ans,st =
        try ans, InWigV (Wig.V.append_datum dat (Wig.V.datum_of_string l)) with Wig.Bad m ->
        try msgs := m::!msgs; ans, InWigV (Wig.V.set_header dat (Wig.V.header_of_string l)) with Wig.Bad m ->
        try msgs := m::!msgs; (end_InWigV ans dat), AfterT (TrackLine.of_string l, None) with TrackLine.Bad m ->
        try msgs := m::!msgs; (C (Comments.of_string l))::(end_InWigV ans dat), StartTrack2 with Comments.Bad m ->
        try msgs := m::!msgs; ignore (BrowserLines.of_string l); bline_err() with BrowserLines.Bad _ ->
        try any_wig_line l; mix_wig_err() with Wig.Bad m ->
        try ignore (Bed.of_line l); raise_error "found BED data while parsing WIG data" with Bed.Bad _ ->
        raise_errorl ("expecting variable-step formatted WIG data, track or comment line: " ^ l) msgs
      in loop ans st
    in

    let do_InWigF dat l = 
      let msgs = ref [] in
      let ans,st =
        try ans, InWigF (Wig.F.append_datum dat (Wig.F.datum_of_string l)) with Wig.Bad m ->
        try msgs := m::!msgs; ans, InWigF (Wig.F.set_header dat (Wig.F.header_of_string l)) with Wig.Bad m ->
        try msgs := m::!msgs; (end_InWigF ans dat), AfterT (TrackLine.of_string l, None) with TrackLine.Bad m ->
        try msgs := m::!msgs; (C (Comments.of_string l))::(end_InWigF ans dat), StartTrack2 with Comments.Bad m ->
        try msgs := m::!msgs; ignore (BrowserLines.of_string l); bline_err() with BrowserLines.Bad _ ->
        try any_wig_line l; mix_wig_err() with Wig.Bad m ->
        try ignore (Bed.of_line l); raise_error "found BED data while parsing WIG data" with Bed.Bad _ ->
        raise_errorl ("expecting fixed-step formatted WIG data, track or comment line: " ^ l) msgs
      in loop ans st
    in

    let do_InBed dat l =
      let msgs = ref [] in
      let ans,st =
        try ans, InBed (Bed.append dat (Bed.of_line l)) with Bed.Bad m ->
        try msgs := m::!msgs; end_InBed ans dat, AfterT (TrackLine.of_string l, None) with TrackLine.Bad m ->
        try msgs := m::!msgs; (C (Comments.of_string l))::(end_InBed ans dat), StartTrack2 with Comments.Bad m ->
        try msgs := m::!msgs; ignore (BrowserLines.of_string l); bline_err() with BrowserLines.Bad _ ->
        try any_wig_line l; raise_error "found WIG data while parsing BED data" with Wig.Bad _ ->
        raise_errorl ("expecting BED data, track or comment line: " ^ l) msgs
      in loop ans st
    in
    
    let l = try input_line cin with End_of_file -> raise (Eof(line_num,ans,st)) in
    match st with
      | StartFile -> do_StartFile l
      | InB x -> do_InB x l
      | StartTrack1 -> do_StartTrack1 l
      | StartTrack2 -> do_StartTrack2 l
      | AfterT (x,c) -> do_AfterT (x,c) l
      | InWigB x -> do_InWigB x l
      | InWigV x -> do_InWigV x l
      | InWigF x -> do_InWigF x l
      | InBed x -> do_InBed x l
  in (* end loop *)
  
  try loop 1 [] StartFile; assert false with Eof(line_num,ans,st) ->
    let raise_error msg = raise (Error(line_num, msg)) in
    let make ans =
      let rec reverse ans_fwd ans_rev = (* reverse list and concat comments *)
        match ans_rev with
          | [] -> ans_fwd
          | b::[] -> b::ans_fwd
          | (C x)::(C y)::ans_rev -> reverse ans_fwd ((C (Comments.concat y x))::ans_rev)
          | b::ans_rev -> reverse (b::ans_fwd) ans_rev
      in
      let ans = reverse [] ans in
      validate ans;
      ans
    in
    match st with
      | StartFile -> raise_error "no data"
      | InB _ -> raise_error "expecting track line or data after browser lines"
      | StartTrack1 -> raise_error "expecting data after browser lines"
      | StartTrack2 -> make ans
      | AfterT _ -> raise_error "expecting data after track line"
      | InWigB dat -> make (end_InWigB ans dat)
      | InWigV dat -> make (end_InWigV ans dat)
      | InWigF dat -> make (end_InWigF ans dat)
      | InBed dat -> make (end_InBed ans dat)
          
let of_file file =
  let f cin =
    try parse cin
    with Error (k,msg) -> raise_bad (Msg.err ~pos:(Pos.fl file k) msg)
  in
  try_finally f close_in (open_in file)
