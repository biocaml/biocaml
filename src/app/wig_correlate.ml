open Biocaml
open Printf
open Biocaml_std

let prog_name = Sys.argv.(0)

let usage = sprintf
"Usage:
  %s [-m Pearson|Spearman] [-l] file1.wig file2.wig ...

Compute correlations between given WIG files, which must all
provide data for exact same regions.

Options:
  -m Pearson|Spearman
   The method to use. Default is Pearson.

  -l
   Print output in long format. This is implied if three or
   more files given as input.

  --help
   Print this help message."
prog_name

type meth = Pearson | Spearman

type params = {
  in_files : string list; (* two or more input files *)
  long_format : bool;
  meth : meth
}
    
type options = {
  mutable option_in_files : string list;
  mutable option_long_format : bool;
  mutable option_meth : string option;
  mutable option_help : bool
}

let no_options_defined t =
  List.length t.option_in_files = 0 &&
  t.option_long_format = false &&
  t.option_meth = None

let options_to_params (t:options) : params =
  if t.option_help || no_options_defined t then
    (printf "%s\n" usage; exit 0)
  ;

  let exists x =
    try
      let stats = Unix.stat x in
      if stats.Unix.st_kind <> Unix.S_REG then
        failwith (sprintf "%s: not a regular file" x)
    with
        Unix.Unix_error(Unix.ENOENT,_,_) ->
          failwith (sprintf "%s: no such file" x)
  in

  let in_files =
    if List.length t.option_in_files < 2 then
      failwith "must provide at least two input files"
    else (
      List.iter exists t.option_in_files;
      t.option_in_files
    )
  in
  
  let long_format = match t.option_long_format with
    | true -> true
    | false -> List.length in_files > 2
  in
  
  let meth = match t.option_meth with
    | None -> Pearson
    | Some x -> (
        match x with
          | "Pearson" -> Pearson
          | "Spearman" -> Spearman
          | _ -> failwith (sprintf "%s: unknown correlation method" x)
      )
  in
  
  {
    in_files = in_files;
    long_format = long_format;
    meth = meth
  }
    
let parse_cmdline () : params =
  let t = {option_in_files=[]; option_long_format=false; option_meth=None; option_help=false} in
  
  let opts = [
    'l', "", Some (fun () -> t.option_long_format <- true), None;
    'm', "", None, Some (fun x -> t.option_meth <- Some x);
    Getopt.noshort, "help", Some (fun () -> t.option_help <- true), None;
  ]
  in

  let anon_handler x = t.option_in_files <- x::t.option_in_files in
  
  Getopt.parse_cmdline opts anon_handler;
  t.option_in_files <- List.rev t.option_in_files;
  options_to_params t

let corr meth wig_file1 wig_file2 : float =
  let wig_of_file file =
    let wig = Wig.of_file ~fmt:Wig.Bed ~header:true file in
    let cmp (chr1,lo1,hi1,_) (chr2,lo2,hi2,_) = compare (chr1,lo1,hi1) (chr2,lo2,hi2) in
    Array.of_list (List.sort ~cmp (Wig.to_list wig))
  in
  let wig1 = wig_of_file wig_file1 in
  let wig2 = wig_of_file wig_file2 in
  let n1 = Array.length wig1 in
  let n2 = Array.length wig2 in

  if n1 <> n2 then
    failwith (sprintf "%s and %s: files do not have same number of data points" wig_file1 wig_file2);
  
  for i = 0 to n1 - 1 do
    let chr1,lo1,hi1,_ = wig1.(i) in
    let chr2,lo2,hi2,_ = wig2.(i) in
    if (chr1,lo1,hi1) <> (chr2,lo2,hi2) then
      failwith (sprintf "%s and %s: files provide data for different ranges %s:%d-%d and %s:%d-%d" wig_file1 wig_file2 chr1 lo1 hi1 chr2 lo2 hi2)
  done;
  
  let wig1,wig2 =
    let f (_,_,_,x) = x in
    Array.map f wig1, Array.map f wig2
  in
  
  let meth = match meth with
    | Pearson -> Math.pearson
    | Spearman -> Math.spearman
  in  
  meth wig1 wig2

;;
try
  let params = parse_cmdline() in
  let in_files = Array.of_list params.in_files in
  let n = Array.length in_files in

  for i = 0 to n-2 do
    for j = i+1 to n-1 do
      let file1 = in_files.(i) in
      let file2 = in_files.(j) in
      let c = corr params.meth file1 file2 in
      if params.long_format then
        printf "%s\t%s\t%f\n%!" file1 file2 c
      else
        printf "%f\n%!" c
    done
  done

with
    Invalid_argument msg | Failure msg | Getopt.Error msg -> 
      eprintf "%s: %s\n" prog_name msg
      
