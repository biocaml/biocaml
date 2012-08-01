open Batteries
open Printf
open Biocaml

let prog_name = Sys.argv.(0)

let usage = sprintf
"Usage:
  %s -c column file.gff

Prints a count of the values for specified column.

Options:
  -c column
   Specifies which column to count values of.

  --help
   Print this help message."
prog_name

type params = {
  column : string;
  in_file : string
}

type options = {
  mutable option_column : string option;
  mutable option_in_file : string option;
  mutable option_help : bool
}

let no_options_defined t =
  t.option_column = None &&
  t.option_in_file = None

(* Convert command line options to parameters, or print help message
   and exit. *)
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
  
  let column = match t.option_column with
    | None -> failwith "must specify column"
    | Some x ->
        let x' = String.map Char.uppercase x in
        (
          match x' with
            | "CHR" | "SOURCE" | "FEATURE" -> x'
            | _ -> failwith (sprintf "invalid column: %s" x)
        )
  in
  {
    in_file = in_file;
    column = column
  }
    
let parse_cmdline () : params =
  let t = {option_column=None; option_in_file=None; option_help=false} in
 
  let opts = [
    'c', "", None, Some (fun x -> t.option_column <- Some x);
    Getopt.noshort, "help", Some (fun () -> t.option_help <- true), None;
  ]
  in
  
  let anon_handler x = match t.option_in_file with
    | None -> t.option_in_file <- Some x
    | Some _ -> failwith "multiple input files not allowed"
  in
  
  Getopt.parse_cmdline opts anon_handler;
  options_to_params t

;;
(*
try
  let params = parse_cmdline() in

  let get (row : Gff.row) : string = match params.column with
    | "CHR" -> row.Gff.chr
    | "SOURCE" -> row.Gff.source
    | "FEATURE" -> row.Gff.feature
    | _ -> assert false (* if here, options_to_params *)
  in
  
  let module StringMap = Biocaml_std.StringMap in 
  let f counts r : int StringMap.t =
    let increment prev = match prev with None -> 1 | Some k -> k+1 in
    StringMap.add_with (get r) increment counts
  in

  let counts = Gff.fold_file ~version:2 f StringMap.empty params.in_file in

  StringMap.iter ~f:(fun ~key ~data -> printf "%s\t%d\n" key data) counts
with
    Failure msg | Getopt.Error msg -> eprintf "%s: %s\n" prog_name msg
*)
