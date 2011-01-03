#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"]
--

open Batteries_uni;; open Printf

let run cmd =
  print_endline cmd;
  let errcode = Sys.command cmd in
  if errcode = 0 then ()
  else exit errcode
    
let cp src dstdir =
  let open Filename in
  let dst = src |> basename |> flip chop_suffix ".native" |> concat dstdir in
  let cmd = sprintf "cp -f %s %s" src dst in
  let open Unix in
  if not (Sys.file_exists dst) || (stat src).st_mtime > (stat dst).st_mtime then
    run cmd

let srcdir = Sys.argv.(1)
let dstdir = Sys.argv.(2)
let apps =
  srcdir |> Sys.readdir |> Array.to_list 
  |> List.filter (flip Filename.check_suffix ".native")
  |> List.map (Filename.concat srcdir)
    
let _ = List.iter (flip cp dstdir) apps
