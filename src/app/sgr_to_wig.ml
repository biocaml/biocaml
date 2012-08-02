open Batteries
open Printf
open Biocaml
module Lines = Biocaml_std.Lines
module Msg = Biocaml_std.Msg

let prog_name = Sys.argv.(0)

let usage = sprintf
"Usage:
  %s --type=(basepair[,stairstep])|interval [--trackline=\"tag_value_pairs\"] -i file.sgr

Convert given SGR file to a WIG file. Result written to
stdout. Recall that SGR files number the start of a
chromosome as 1 while WIG files use 0-based half-open
intervals. This script does the appropriate conversions.

Options:
  -i file.sgr
  File to convert.

  --type=(basepair[,stairstep])|interval
  basepair interprets data as mapping from individual base
  pairs. If basepair is specified in conjunction with
  stairstep then data is interpreted as if value remains
  constant until changed by next row. Alternatively, interval
  interprets data as a mapping from intervals.

  --trackline=\"tag_value_pairs\"
  Can be used to add tag value pairs in track line. The given
  values will be checked for validity to the extent possible,
  i.e. if the UCSC gives a requirement for a certain tag, it
  is checked, and otherwise it is retained unaltered. It is
  recommended to enclose the whole string in quotes to make
  sure your shell passes it through unaltered. Do not include
  \"track type=wiggle_0\", which is always included in output
  by default.

  --help
   Print this help message."
prog_name

type typ = Basepair | BasepairStairstep | Interval

type params = {
  typ : typ;
  (* trackline : Track.TrackLine.t; *)
  in_file : string
}

type options = {
  mutable option_type : string option;
  mutable option_in_file : string option;
  mutable option_trackline : string option;
  mutable option_help : bool
}

(* Convert command line options to parameters,
   or print help message and exit. *)  
let options_to_params (t:options) : params =
  if t.option_help then (printf "%s\n" usage; exit 0);
  
  let exists x =
    if not (Sys.file_exists x) then
      failwith (sprintf "%s: no such file or directory" x)
  in
 (* 
  let trackline = match t.option_trackline with
    | None -> "track type=wiggle_0"
    | Some x -> sprintf "track type=wiggle_0 %s" x
  in
  let trackline = Track.TrackLine.of_string trackline in
 *)  
  {
    in_file = (match t.option_in_file with
      | None -> failwith "must specify input file"
      | Some x -> (exists x; x)
    );
    
    typ = (
      match t.option_type with
        | None -> failwith "must specify SGR file type"
        | Some x ->
            match x with
              | "basepair" -> Basepair
              | "basepair,stairstep" -> BasepairStairstep
              | "interval" -> Interval
              | _ -> failwith (sprintf "%s: invalid type" x)
    );
    
    (* trackline = trackline *)
  }

let parse_cmdline () : params =
  let t = {option_type=None; option_in_file=None; option_trackline=None; option_help=false} in
 
  let opts = [
    Getopt.noshort, "type", None, Some (fun x -> t.option_type <- Some x);
    'i', "", None, Some (fun x -> t.option_in_file <- Some x);
    (* Getopt.noshort, "trackline", None, Some (fun x -> t.option_trackline <- Some x); *)
    Getopt.noshort, "help", Some (fun () -> t.option_help <- true), None;
  ]
  in
  
  Getopt.parse_cmdline opts (fun s -> failwith (sprintf "invalid option %s\n" s));
  options_to_params t


type sgr_pt = string * int * string
    (* chromosome, position, and value (as a string) *)
    
(* Raise [Failure] if [x] is not a number. *)
let validate_num (x:string) : unit =
  try ignore (int_of_string x)
  with Failure _ ->
    try ignore (float_of_string x)
    with Failure _ -> failwith (sprintf "%s: not a number" x)
        
(* Return chr, pos, and value from given SGR line. The numeric value
   is returned as a string. *)
let parse line : sgr_pt =
  match String.nsplit line "\t" with
    | [chr;pos;x] ->
        validate_num x;
        chr, int_of_string pos, x
    | l -> failwith (sprintf "expecting exactly 3 columns but found %d" (List.length l))
        
let print_wig chr lo hi x =
  printf "%s\t%d\t%d\t%s\n" chr lo hi x
    
;;
try
  let params = parse_cmdline() in
  
  (* printf "%s\n" (Track.TrackLine.to_string params.trackline); *)

  match params.typ with
    | Basepair ->
        let f line =
          let chr,pos,x = parse line in
          print_wig chr (pos-1) pos x
        in
        Lines.iter_file f params.in_file

    | BasepairStairstep ->
        (* Print values from [prev]ious point up to but 'not'
           including [curr]ent point. *)
        let from_to
            ((chr_prev, pos_prev, x_prev) : sgr_pt) 
            ((chr_curr, pos_curr, x_curr) : sgr_pt)
            : unit =
          if chr_curr <> chr_prev then
            print_wig chr_prev (pos_prev-1) pos_prev x_prev
          else if not (pos_curr > pos_prev) then
            failwith "stairstep type data must be strictly ascending"
          else
            for i = pos_prev to pos_curr-1 do
              print_wig chr_prev (i-1) i x_prev
            done
        in
        
        (* Print values from [prev]ious point up to but 'not'
           including [curr]ent line. Return current parsed line. *)
        let f (prev : sgr_pt option) (line:string) : sgr_pt option =
          let curr = parse line in
          match prev with
            | None -> Some curr
            | Some prev -> (from_to prev curr; Some curr)
        in
        
        (
          match Lines.fold_file f None params.in_file with
            | None -> (* the file was empty *)
                () 
            | Some (chr,pos,x) -> (* print the last line *)
                print_wig chr (pos-1) pos x 
        )
          
    | Interval ->
        (* pt2, pt1, and pt0 are a window of three points, where
           - pt0 is the current point
           - pt1 is previous point
           - pt2 is the point two rows ago
           Print interval corresponding to range from pt2 to pt1.
           Shift the window one point, i.e. return pt1 and pt0. *)
        let f (pt2,pt1) line =
          let (chr0,pos0,x0) as pt0 = parse line in
          let pt0 = Some pt0 in
          match pt2,pt1 with
            | None, _ -> pt1, pt0
            | Some _, None -> invalid_arg "loop invariant violated"
            | Some (chr2,pos2,x2), Some (chr1,pos1,x1) -> (
                match chr2=chr1, chr2=chr0, chr1=chr0 with
                  | true,true,true ->
                      if pos1 > pos2 then
                        (print_wig chr2 (pos2-1) (pos1-1) x2;
                         pt1,pt0)
                      else
                        failwith "interval type data must be strictly ascending"
                  | true,false,false -> (* pt1 is last point on previous chromosome *)
                      if x2 = x1 then
                        (print_wig chr2 (pos2-1) pos1 x2;
                         None,pt0)
                      else
                        (print_wig chr2 (pos2-1) (pos1-1) x2;
                         print_wig chr1 (pos1-1) pos1 x1;
                         None,pt0)
                  | true,true,false | true,false,true | false,true,true ->
                      invalid_arg "logical impossibility"
                  | false,_,false ->
                      failwith (sprintf "only one data line for chromosome %s" chr1)
                  | false,false,true ->
                      invalid_arg "loop invariant violated, pt2 should have been None in this case"
              )
        in
        
        match Lines.fold_file f (None,None) params.in_file with
          | None,None -> (* file was empty *)
              ()
          | None, Some (chr,pos,x) -> (* file had just one line *)
              failwith (sprintf "only one data line for chromosome %s" chr)
          | Some _, None -> invalid_arg "loop invariant violated"
          |  Some (chr2,pos2,x2), Some (chr1,pos1,x1) ->
               if chr2 <> chr1 then
                 failwith (sprintf "only one data line for chromosomes %s and %s" chr2 chr1)
               else if x2 = x1 then
                 print_wig chr2 (pos2-1) pos1 x2
               else
                 (print_wig chr2 (pos2-1) (pos1-1) x2;
                  print_wig chr1 (pos1-1) pos1 x1)
                   
with
  | Failure msg | Getopt.Error msg (* | Track.TrackLine.Bad msg *) ->
      eprintf "%s: %s\n" prog_name msg
  | Lines.Error(pos,msg) ->
      eprintf "%s: %s\n" prog_name (Msg.err ~pos msg)
