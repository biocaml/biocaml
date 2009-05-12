open Sesame;; open Printf

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
  
let options_to_params (t:options) : params =
  assert (not t.option_help);
  
  let exists x = (if not (Sys.file_exists x) then failwith (sprintf "%s: no such file or directory" x)) in
  
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
  if t.option_help || no_options_defined t then
    (printf "%s\n" usage; exit 0)
  else
    options_to_params t

;;
try
  let p = parse_cmdline() in
  
  let f line =
    if String.starts_with line "##" then
      ()
    else if String.for_all Char.is_space line then
      ()
    else
      try
        let sl = String.nsplit line "\t" in
        let nth = List.nth sl in
        let nthi = nth ->> int_of_string in
        let chr = nth 0 in
        let lo = nthi 3 in
        let hi = nthi 4 in
        printf "%s\t%d\t%d\n" chr (lo-1) hi
      with Failure msg -> (if p.strict then failwith msg else ())
  in
  
  Lines.iter_file f p.in_file

with
    Failure msg | Getopt.Error msg -> eprintf "%s: %s\n" prog_name msg
