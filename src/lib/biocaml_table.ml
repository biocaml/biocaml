open Biocaml_std

module Comments = Biocaml_comments
module Tags = Biocaml_tags

exception Invalid of string
exception No_column of string

type column_names = int * (int StringMap.t)
    (* number of columns, and map from column name to column number,
       first column is zero *)

type row = string array
type getter = row -> string -> string
type columns = string list
type t = Comments.t option * columns * getter * row Enum.t

let make_getter columns =
  let f i elt accum =
    if StringMap.mem elt accum then
      raise (Invalid (sprintf "found duplicate column name %s" elt))
    else
      StringMap.add elt i accum
  in
  let map = columns |> List.enum |> Enum.foldi ~f ~init:StringMap.empty in
  fun row col ->
    match StringMap.find col map with
    | Some i -> row.(i)
    | None -> raise (No_column col)

let of_input ?(itags="table,comment-char=#,header,header_,separator=\\t") cin =
  let itags = Tags.of_string itags in
  let e = IO.lines_of cin in
  if not (Tags.tag_is "table" "true" itags) then raise (Tags.Invalid "table=true tag required");
  let separator = Tags.find "separator" itags in

  (* parse comments if needed and return enumeration starting after comments *)
  let comments, e =
    try
      let comment_char = (Tags.find "comment-char" itags).[0] in
      let e1,e2 = Enum.span (Comments.is_comments ~comment_char) e in 
      let f x l = Comments.concat x (Comments.of_string ~comment_char l) in
      Some (e1 |> Enum.fold ~f ~init:(Comments.empty comment_char)),
      e2
    with Not_found -> None, e
  in

  (* parse header if needed or None otherwise *)
  let columns_opt =
    if Tags.tag_is "header" "true" itags then
      let columns = match Enum.get e with
        | None -> raise (Invalid "expected headers but reached end-of-input")
        | Some s -> String.nsplit s separator
      in
      (if Tags.tag_is "header_" "true" itags then
        match Enum.get e with
          | None -> raise (Invalid "expected header underline but reached end-of-input")
          | Some s ->
              let pred c = Char.is_whitespace c || c = '-' in
              if s |> String.enum |> Enum.for_all ~f:pred then
                ()
              else
                raise (Invalid "header line must consist of only whitespace or \'-\'") )
      ;
      Some columns
    else
      None
  in

  (* return previously parsed column names or generate by peeking ahead to first data row *)
  let columns = match columns_opt, Enum.peek e with
    | None, None -> []
    | None, Some s ->
        (String.nsplit s separator
        |> List.length |> (fun n -> Enum.(0 -- (n-1)))
        |> Enum.map ~f:(fun i -> "column" ^ string_of_int i)
        |> List.of_enum)
    | Some columns, _ -> columns
  in

  comments,
  columns,
  make_getter columns,
  Enum.map (fun s -> Array.of_list(String.nsplit s separator)) e


let of_string_list ?(itags="table,comment-char=#,header") ?comments ?columns rows =
  let itags = Tags.of_string itags in
  if not (Tags.tag_is "table" "true" itags) then raise (Tags.Invalid "table=true tag required");

  let comments =  match Tags.mem "comment-char" itags, comments with
    | true, Some x ->
        let comment_char = (Tags.find "comment-char" itags).[0] in
        Some (Comments.of_string ~comment_char x) 
    | false, None -> None
    | true, None -> Invalid "comment-char specified but comments not provided" |> raise
    | false, Some _ -> Invalid "comments not allowed but provided" |> raise
  in

  let columns = match columns with
    | Some x -> x
    | None ->
        match rows with
          | [] -> []
          | row::_ -> Enum.(0 -- (List.length row - 1)) |> Enum.map ~f:string_of_int |> List.of_enum
  in

  comments, columns, make_getter columns, List.enum rows |> Enum.map ~f:(Array.of_list)


let to_sqlite ?(otags="sqlite,db=:memory:,db_table=table") (_,cols,get,e) =
  let otags = Tags.of_string otags in
  if not (Tags.tag_is "sqlite" "true" otags) then raise (Tags.Invalid "sqlite=true tag required");
  let open Sqlite3 in
  let db = db_open (Tags.find "db" otags) in
  let db_table = Tags.find "db_table" otags in
  
  let cols' = cols |> List.map ~f:(sprintf "'%s' TEXT") |> String.concat ~sep:", " in
  let stmt = sprintf "CREATE TABLE '%s' (%s);" db_table cols' in
  (match exec db stmt with
    | Rc.OK -> ()
    | x -> failwith (sprintf "sqlite error %s when executing command %s" (Rc.to_string x) stmt)
  );

  let insert row =
    let values = cols |> List.map ~f:(fun x -> sprintf "'%s'" (get row x))
                 |> String.concat ~sep:", " in
    let stmt = sprintf "INSERT INTO '%s' values (%s);" db_table values in
    match exec db stmt with
      | Rc.OK -> ()
      | x -> failwith (sprintf "sqlite error %s when executing command %s" (Rc.to_string x) stmt)
  in
  Enum.iter insert e;
  db
