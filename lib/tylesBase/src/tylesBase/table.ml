module List = List2
module Array = Array2
module String = String2
open Pervasives2
open Printf

type column_names = int StringMap.t
    (* map from column name to column number,
       first column is zero *)

type row = string array
type getter = row -> string -> string

let parse_column_names s =
  let sl = String.nsplit s "\t" in
  let f (column_num, ans) column_name =
    if StringMap.mem column_name ans then
      failwith (sprintf "duplicate column name: %s" column_name)
    else
      column_num+1, StringMap.add column_name column_num ans
  in
  let _,ans = List.fold_left f (0,StringMap.empty) sl in
  ans
    
let parse_row s = Array.of_list (String.nsplit s "\t")

let get cols r x = 
  try r.(StringMap.find x cols)
  with Not_found -> failwith (sprintf "invalid column name: %s" x)

let make_iter file =
  let columns cin = 
    try parse_column_names (input_line cin)
    with Failure msg -> failwith (Msg.err ~pos:(Pos.fl file 1) msg)
  in
  let iter f cin = (* cin should point to beginning of file *)
    ignore (input_line cin);
    try Lines.iter_channel (f <<- parse_row) cin
    with Lines.Error (pos,msg) ->
      let pos = Pos.incrl pos 1 in (* because skipped first line *)
      let pos = Pos.set_file pos file in
      failwith (Msg.err ~pos msg)
  in
  let columns file = try_finally columns close_in (open_in file) in
  let iter f file = try_finally (iter f) close_in (open_in file) in
  get (columns file), fun f -> iter f file
    
let make_fold file =
  let get,iter = make_iter file in
  let fold f init =
    let ans = ref init in
    let g r = ans := f !ans r in
    iter g;
    !ans
  in
  get,fold
