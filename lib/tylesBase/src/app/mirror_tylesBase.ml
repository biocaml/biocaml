open Printf

let lib = "tylesBase"
let prog = Filename.basename Sys.argv.(0)

let usage =
  "usage: " ^ prog ^ " master [dir ...]\n  \
      - Mirror master library to dir for each dir given.\n  \
      - master must be the root of your master copy.\n  \
      - At least one dir must be given.\n  \
      - If dir previously exists it will be updated.\n  \
      - All directories, master and mirrors, must be named " ^ lib ^ "."

let has_final_slash (s:string) : bool =
  let n = String.length s in
  assert (n > 0);
  s.[n-1] = '/'

let add_final_slash s : string =
  if String.length s = 0 then
    "/"
  else if has_final_slash s then
    s
  else
    s ^ "/"
      
let strip_final_slash s : string =
  let n = String.length s in
  if n = 0 then
    ""
  else if has_final_slash s then
    String.sub s 0 (n-1)
  else
    s
      
let dir_exists dir : unit =
  if not (Sys.file_exists dir) then 
    failwith (dir ^ " does not exist")
  else if not (Sys.is_directory dir) then
    failwith (dir ^ " is not a directory")
  else
    ()

let check_dir_name dir : bool =
  let dir =
    if has_final_slash dir then
      Filename.basename (Filename.dirname dir)
    else
      Filename.basename dir
  in
  dir = lib
      
let check_master dir =
  if not (check_dir_name dir) then failwith (sprintf "invalid master directory %s: must be named %s" dir lib);
  dir_exists dir
  
let check_mirror dir =
  if not (check_dir_name dir) then failwith (sprintf "invalid mirror directory %s: must be named %s" dir lib);
  dir_exists (Filename.dirname dir)  

let rsync src dest =
  let cmd = sprintf "rsync -av --progress --delete --exclude=.svn --exclude=.DS_Store --exclude=_build --exclude=/doc/html --exclude=*.{a,o,so,cm*,byte,native} %s %s" src dest in
  ignore (Sys.command (sprintf "echo \"%s\"" cmd));
  let exit_code = Sys.command cmd in
  if exit_code <> 0 then failwith (sprintf "rsync failed with code %d" exit_code) else ()
    
;;

try
  let n = Array.length Sys.argv in
  if n < 3 then 
    print_endline usage
  else
    let master = add_final_slash (Sys.argv.(1)) in check_master master;
    let mirrors = Array.map strip_final_slash (Array.sub Sys.argv 2 (n-2)) in
    let m = Array.length mirrors in
    for i = 0 to m-1 do check_mirror mirrors.(i) done;
    for i = 0 to m-1 do rsync master mirrors.(i) done;
    exit 0
with
    Failure msg -> print_endline (prog ^ ": " ^ msg); exit 1
      
