open Batteries
open Printf

let prog_name = Sys.argv.(0)

let usage = sprintf
"Usage:
  %s file.gff[3]

Convert given GFF file to a BED file. Result written to
stdout. Coordinate conversions are handled correctly to
match GFF and BED format specifications.

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

;;
try
  let params = parse_cmdline() in
  let string_is_whitespace s =
    String.fold_left (fun a b -> a && Char.is_whitespace b) true s in
  let f line =
    if String.starts_with line "##" then ()
    else if string_is_whitespace line then ()
    else
      try
        match String.nsplit line "\t" with
          | chr::_::_::lo::hi::_ ->
              printf "%s\t%d\t%d\n" chr (int_of_string lo - 1) (int_of_string hi)
          | _ -> failwith "expecting at least 5 columns"
      with Failure msg -> (if params.strict then failwith msg else ())
  in

  Biocaml_std.Lines.iter_file f params.in_file

with
    Failure msg | Getopt.Error msg -> eprintf "%s: %s\n" prog_name msg
