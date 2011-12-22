open Batteries;; open Printf

exception Invalid of string

(* Concrete syntax tree. *)
module Cst = struct
  type tag = string * string
  type t = tag list

  let parse_escaped_string (s:string) : string =
    let s = String.to_list s in
    let rec loop ans = function
      | [] -> ans
      | '\\'::c::rest -> (
          match c with
            | 't' -> loop ('\t'::ans) rest
            | _ -> failwith (sprintf "unsupported escape sequence \\%c" c)
        )
      | c::rest -> loop (c::ans) rest
    in
    loop [] s |> List.rev |> String.of_list

  let parse_tag (s:string) : tag =
    try
      let x,y = String.split s "=" |> (Tuple2.mapn String.strip) in
      x, parse_escaped_string y
    with Not_found | Invalid_argument _ ->
      match s with
        | "" -> failwith "found empty tag"
        | "!" -> failwith "invalid tag \"!\""
        | _ ->
            let n = String.length s in
            if s.[0] = '!' then
              String.slice ~first:1 ~last:n s, "false"
            else
              s, "true"
                
  let of_string s : t =
    s |> flip String.nsplit "," |> List.map parse_tag
end

type tag = string * string
type t = tag list
    
let parse_tag ((x,y) as tag : tag) : (tag, string) result =
  let is_boolean = function "true" | "false" -> true | _ -> false in
  let is_char y = String.length y = 1 in
  let is_string y = String.length y <> 0 in
  match x with
    | "table" | "bed" | "sqlite" | "header" | "header_" ->
        if is_boolean y then Ok tag
        else Bad (sprintf "tag \'%s\' expected Boolean value but assigned \"%s\"" x y)
    | "comment-char" ->
        if is_char y then Ok tag
        else Bad (sprintf "tag \'%s\' expected single character value but assigned \"%s\"" x y)
    | "separator" | "db" | "db_table" ->
        if is_string y then Ok tag
        else Bad (sprintf "tag \'%s\' expected non-empty string value but assigned \"%s\"" x y)
    | _ -> Bad (sprintf "unrecognized tag %s" x)

let of_cst (cst:Cst.t) : t =
  let parse_tag tag = match parse_tag tag with
    | Ok tag -> tag
    | Bad msg -> Invalid (msg) |> raise
  in
  List.map parse_tag cst

let of_string = Cst.of_string |- of_cst
let find = List.assoc
let tag_is x y t = try find x t = y with Not_found -> false
let mem = List.mem_assoc

(* ***** Below is relevant to checking validity of particular
 * ***** combinations of tags. Incomplete, but save for now.

type tag_kind =
    | KTable
    | KBed
    | KComments
    | KCommentChar
    | KHeader
    | KHeaderUnderlined
    | KRectangular
    | KColumnDelimiter

let kind_of_tag = function
  | Table -> KTable
  | Bed -> KBed
  | Comments _ -> KComments
  | CommentChar _ -> KCommentChar
  | Header _ -> KHeader
  | HeaderUnderlined _ -> KHeaderUnderlined
  | Rectangular _ -> KRectangular
  | ColumnDelimiter _ -> KColumnDelimiter

(* True if any of the given [tags] are of kind [k]. *)
let is_any_of_kind (tags : tag list) (k : tag_kind) : bool =
  List.exists (kind_of_tag |- ((=) k)) tags
    
let is_any_of_kinds (tags : tag list) (ks : tag_kind list) : bool =
  List.exists ((=) true) (List.map (is_any_of_kind tags) ks)

(* True if tag represents a top-level data format.
 * IMPORTANT: Update this if you update tags.
 *)
let is_main_kind (tag : tag) : bool = match tag with
  | Table _ | Bed _ -> true
  | _ -> false

let unique_main_kind (tags : t) : bool =
  List.length (List.filter is_main_kind tags) = 1

let remove_tags_of_kind tags k : tag list =
  List.filter (kind_of_tag |- ((=) k) |- not) tags

 ***** *)
