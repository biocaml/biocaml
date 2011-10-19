open Batteries
open Printf
open Biocaml

module StringMap = Biocaml_std.StringMap

let prog_name = Sys.argv.(0)

let usage = sprintf
"Usage:
  %s file.gff[3]

Convert given GFF file to a table with tab separated
columns. Comment lines are ignored. First line in output
gives the column names. Result written to stdout. Only
version 3 GFF files supported currently.

Options:
  --strict=[true|false]
   If set to false, then erroneous lines are silently
   skipped. When true, the default, erroneous lines will
   terminate the program with an error message.

  --help
   Print this help message."
prog_name

type params = {
  strict : bool;
  in_file : string
}

type options = {
  mutable option_strict : string option;
  mutable option_in_file : string option;
  mutable option_help : bool
}

let no_options_defined t =
  t.option_strict = None &&
  t.option_in_file = None

(* Convert command line options to parameters,
   or print help message and exit. *)  
let options_to_params (t:options) : params =
  if t.option_help || no_options_defined t then
    (printf "%s\n" usage; exit 0)
  ;
  
  let exists x =
    if not (Sys.file_exists x) then
      failwith (sprintf "%s: no such file or directory" x)
  in
  
  let in_file = match t.option_in_file with
    | None -> failwith "must specify input file"
    | Some x -> (exists x; x)
  in
  
  let strict = match t.option_strict with
    | None -> true
    | Some x -> (match x with
        | "true" -> true
        | "false" -> false
        | _ -> failwith (sprintf "invalid strict value: %s" x)
      )
  in
  {
    in_file = in_file;
    strict = strict
  }
    

let parse_cmdline () : params =
  let t = {option_strict=None; option_in_file=None; option_help=false} in
 
  let opts = [
    Getopt.noshort, "strict", None, Some (fun x -> t.option_strict <- Some x);
    Getopt.noshort, "help", Some (fun () -> t.option_help <- true), None;
  ]
  in
  
  let anon_handler x = match t.option_in_file with
    | None -> t.option_in_file <- Some x
    | Some _ -> failwith "multiple input files not allowed"
  in
  
  Getopt.parse_cmdline opts anon_handler;
  options_to_params t


type columns = int * int StringMap.t
    (* first item is number of columns,
       second item is mapping from column names to column location,
       first column is numbered 1,
       number of columns guaranteed to equal size of map *)


(* Get the number of columns and the column map for a given gff file *)
let get_columns gff : columns =
  let columns = [
    "CHR",1; "SOURCE",2; "FEATURE",3; "START",4;
    "END",5; "SCORE",6; "STRAND",7; "PHASE",8
  ]
  in
  let num_columns = List.length columns in
  let columns = StringMap.of_enum (List.enum columns) in
  
  let add_attribute (num_columns,columns) attribute =
    if StringMap.mem attribute columns then
      (num_columns,columns)
    else
      (num_columns+1, StringMap.add attribute (num_columns+1) columns)
  in
  
  let add_row (num_columns,columns) row =
    List.fold_left add_attribute (num_columns,columns) (Gff.attribute_names row)
  in
  
  Gff.fold add_row (num_columns,columns) gff


(* Convert GFF row to a table row with values in order as dictated by [columns]. *)
let convert_row strict (num_columns,columns) (x:Gff.row) : string list =
  let lo,hi = x.Gff.pos in
  let lo = string_of_int lo in
  let hi = string_of_int hi in
  let score = match x.Gff.score with 
    | None -> "." 
    | Some x -> string_of_float x
  in
  let strand = match x.Gff.strand with 
    | Gff.Sense -> "+"
    | Gff.Antisense -> "-"
    | Gff.Unstranded -> "."
    | Gff.Unknown -> "?"
  in
  let phase = match x.Gff.phase with
    | None -> "."
    | Some x -> string_of_int x
  in
  let mp = [
    "CHR", x.Gff.chr;
    "SOURCE", x.Gff.source;
    "FEATURE", x.Gff.feature;
    "START", lo;
    "END", hi;
    "SCORE", score;
    "STRAND", strand;
    "PHASE", phase
  ]
  in
  let mp = StringMap.of_enum (List.enum mp) in

  let mp =
    let add_attribute mp = function
      | Gff.Something _ -> mp
      | Gff.TagValue (x,y) ->
          if StringMap.mem x mp then
            if strict then
              failwith (sprintf "column %s already assigned a value" x)
            else
              mp
          else
            StringMap.add x y mp
    in
    List.fold_left add_attribute mp x.Gff.attributes
  in

  let ans = Array.make num_columns "" in
  let set_arr ~key:col_name ~data:col_val =
    match StringMap.find col_name columns with
    | Some i -> ans.(i-1) <- col_val
    | None -> failwith "Column not found (convert_row)"
  in
  StringMap.iter ~f:set_arr mp;
  Array.to_list ans
    
      
;;
try
  let params = parse_cmdline() in
  
  let gff = Gff.of_file ~strict:params.strict params.in_file in
  let ((num_columns,columns) as cols) = get_columns gff in

  (* print column headers *)
  let _ =  
    let x = List.of_enum (StringMap.enum columns) in
    let cmp (_,i) (_,j) = compare i j in
    let x = List.sort ~cmp  x in
    let x = List.map fst x in
    print_endline (String.concat "\t" x)
  in
  
  (* print table *)
  let f row =
    print_endline (String.concat "\t" (convert_row params.strict cols row))
  in
  Gff.iter f gff
    
with
    Failure msg | Getopt.Error msg -> eprintf "%s: %s\n" prog_name msg
