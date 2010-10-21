module List = List2
module Array = Array2
module String = String2
open Pervasives2
open Printf

type column_names = int * (int StringMap.t)
    (* number of columns, and map from column name to column number,
       first column is zero *)

type row = string array
type getter = row -> string -> string
type columns = string list

let parse_column_names s =
  let sl = String.nsplit s "\t" in
  let f (column_num, ans) column_name =
    if StringMap.mem column_name ans then
      failwith (sprintf "duplicate column name: %s" column_name)
    else
      column_num+1, StringMap.add column_name column_num ans
  in
  List.fold_left f (0,StringMap.empty) sl
      
let parse_row s = Array.of_list (String.nsplit s "\t")
  
let get ((num_cols, cols) : column_names) r x =
  try
    let i = StringMap.find x cols in
    let n = Array.length r in
    if n > num_cols then
      failwith (sprintf "expecting %d columns but this row has %d" num_cols n)
    else if i >= n then
      ""
    else
      r.(i)
  with
      Not_found -> failwith (sprintf "invalid column name: %s" x)

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
  let (_,col_map) as cols = columns file in
  let col_list = (StringMap.to_list ->> (List.sort ~cmp:(fun (_,a) (_,b) -> compare a b)) ->> (List.map fst)) col_map in
  col_list, get cols, fun f -> iter f file
    
let make_fold file =
  let cols,get,iter = make_iter file in
  let fold f init =
    let ans = ref init in
    let g r = ans := f !ans r in
    iter g;
    !ans
  in
  cols,get,fold

let hashtbl_of_file file x y =
  let _,get,iter = make_iter file in
  let ans = Hashtbl.create 10 in
  let f r =
    let x = get r x in
    let y = get r y in
    try
      let prev_y = Hashtbl.find ans x in
      if prev_y <> y then
        failwith (sprintf "%s: previously mapped to %s but now maps to %s" x prev_y y)
    with Not_found ->
      Hashtbl.add ans x y
  in
  iter f;
  ans

let multi_hashtbl_of_file file x y =
  let _,get,iter = make_iter file in
  let ans = Hashtbl.create 10 in
  let f r =
    let x = get r x in
    let y = get r y in
    let prev_y = try Hashtbl.find ans x with Not_found -> StringSet.empty in
    Hashtbl.replace ans x (StringSet.add y prev_y)
  in
  iter f;
  ans
  
let map_of_file file x y =
  let _,get,fold = make_fold file in
  let f ans r =
    let x = get r x in
    let y = get r y in
    try
      let prev_y = StringMap.find x ans in
      if prev_y <> y then
        failwith (sprintf "%s: previously mapped to %s but now maps to %s" x prev_y y)
      else
        ans
    with Not_found ->
      StringMap.add x y ans
  in
  fold f StringMap.empty

let multi_map_of_file file x y =
  let _,get,fold = make_fold file in
  let f ans r =
    let x = get r x in
    let y = get r y in
    let prev_y = try StringMap.find x ans with Not_found -> StringSet.empty in
    StringMap.add x (StringSet.add y prev_y) ans
  in
  fold f StringMap.empty
