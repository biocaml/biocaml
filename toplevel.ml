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
#require \"core, zip, sqlite3, unix, batteries, xmlm, pcre\"
#directory %S;;
#load \"biocaml.cma\";;
open Core.Std;;
" biocaml_dir;
  close_out o;
  command "ocaml -init %s" tmp;
  Sys.remove tmp
