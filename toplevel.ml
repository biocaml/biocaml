#! /usr/bin/env ocaml

#use "topfind"
#thread
#require "core"
open Core.Std
open Printf

let setup_data_fn = "setup.data"
let biocaml_dir = "_build/src/lib"

let command fmt =
  let f x =
    printf "Running %S\n%!" x;
    ignore (Sys.command x)
  in
  ksprintf f fmt

let start_with s p =
  let len_p = String.length p in
  String.length s >= len_p && String.sub s 0 len_p = p

let camlzip_findlib_name =
  if Sys.file_exists setup_data_fn = `No then (
    printf "%S not found. Run \"ocaml setup.ml -configure\" first.\n"
           setup_data_fn;
    exit 1
  );
  let data = In_channel.read_lines setup_data_fn in
  let is_zip = List.exists data (fun l -> start_with l "pkg_zip") in
  if is_zip then "zip" else "camlzip"


let () =
  let lib = Filename.concat biocaml_dir "biocaml.cma" in
  if Sys.file_exists lib = `No then (
    printf "%S not found.  Please compile the library first.\n" lib;
    exit 1
  );
  let tmp = Filename.temp_file "ocamlinit" ".ml" in
  let o = open_out tmp in
  fprintf o "
#use \"topfind\";;
#thread;;
#require \"core, %s, sqlite3, unix, batteries, xmlm, netstring\"
#directory %S;;
#load \"biocaml.cma\";;
open Core.Std;;
" camlzip_findlib_name biocaml_dir;
  close_out o;
  command "ocaml -init %s" tmp;
  Sys.remove tmp
