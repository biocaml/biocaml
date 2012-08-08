#! /usr/bin/env ocaml

open Printf

let command fmt =
  let f x =
    printf "Running %S\n" x;
    ignore (Sys.command x)
  in
  ksprintf f fmt

let remove file =
  if Sys.file_exists file then (
    printf "Removing %s\n" file;
    Sys.remove file
  )
    
let usage ch =
  fprintf ch "usage: %s {-help,-h,help,<cmd>}\n" Sys.argv.(0)
    
let help () =
  usage stdout;
  printf "where cmd is:\n";
  printf "  setup : run oasis setup + customizations\n";
  printf "  setup-clean : really clean oasis-generated files\n";
  printf "  clean all :  make clean + setup-clean\n";
  ()
    
let check_cwd () =
  if not (Sys.file_exists "_oasis") then
    failwith "Wrong working directory: There is no _oasis file"
  
let setup_clean () =
  check_cwd ();
  command "oasis setup-clean";
  remove "myocamlbuild.ml";
  remove "setup.data";
  remove "setup.log";
  remove "_tags";
  remove "Makefile";
  remove "configure";
  remove "src/lib/META";
  remove "src/lib/libbiocaml_stubs.clib";
  remove "src/lib/doclib.odocl";
  remove "src/lib/biocaml.mllib";
  remove "setup.ml"

  
let setup () =
  check_cwd ();
  setup_clean ();
  command "oasis setup";
  command "echo 'true: annot' >> _tags"

    
let () =
  match List.tl (Array.to_list Sys.argv) with
  | [] -> usage stdout
  | "-help" :: _ | "--help" :: _ | "-h" :: _ | "help" :: _ -> help ()
  | "setup" :: [] -> setup ()
  | "setup-clean" :: [] -> setup_clean ()
  | "clean" :: "all" :: [] -> command "make clean"; setup_clean ()
  | "configure" :: l ->
    command "ocaml setup.ml -configure %s"
      (String.concat " " (List.map (sprintf "%S") l))
  | "build" :: args
  | "make" :: args ->
    command "make %s" (String.concat " " (List.map (sprintf "%S") args))
  | h :: _ -> eprintf "Cannot understand %s\n" h; usage stderr

