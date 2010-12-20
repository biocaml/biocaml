open Batteries_uni;; open Printf

exception Invalid of string

type column_names = int * (int StringMap.t)
    (* number of columns, and map from column name to column number,
       first column is zero *)

type row = string array
type getter = row -> string -> string
type columns = string list

let make_getter columns =
  let f i elt accum =
    if StringMap.mem elt accum then
      raise (Invalid (sprintf "found duplicate column name %s" elt))
    else
      StringMap.add elt i accum
  in
  let map = columns |> List.enum |> Enum.foldi f StringMap.empty in
  fun row col ->
    try row.(StringMap.find col map)
    with Invalid_argument _ -> raise (Invalid (sprintf "row does not have data for column %s" col))

let enum_input ?(itags="table,comment-char=#,header,header_,separator=\\t") cin =
  let itags = Tags.of_string itags in
  let e = IO.lines_of cin in
  if not **> Tags.tag_is "table" "true" itags then raise (Tags.Invalid "table=true tag required");
  let separator = Tags.find "separator" itags in

  (* parse comments if needed and return enumeration starting after comments *)
  let comments, e =
    try
      let comment_char = (Tags.find "comment-char" itags).[0] in
      let e1,e2 = Enum.span (Comments.is_comments ~comment_char) e in
      Some (e1 |> Enum.map (Comments.of_string ~comment_char)
      |> List.of_enum |> Comments.concat),
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
              if s |> String.enum |> Enum.for_all pred then
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
        (s
        |> flip String.nsplit separator
        |> List.length |> (fun n -> 0 -- (n-1))
        |> Enum.map ((^) "column" -| string_of_int)
        |> List.of_enum)
    | Some columns, _ -> columns
  in

  comments,
  columns,
  make_getter columns,
  Enum.map (flip String.nsplit separator |- Array.of_list) e
