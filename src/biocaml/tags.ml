open Batteries_uni;; open Printf

(* Concrete syntax tree. *)
module Cst = struct
  type value =
      | B of bool
      | S of string

  type tag = string * value
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
      let x,y = String.split s "=" |> (Pair.map String.strip) in
      x, S (parse_escaped_string y)
    with Not_found ->
      match s with
        | "" -> failwith "found empty tag"
        | "!" -> failwith "invalid tag \"!\""
        | _ ->
            let n = String.length s in
            if s.[0] = '!' then
              String.slice ~first:1 ~last:n s, B false
            else
              s, B true

  let of_string s : t =
    s |> flip String.nsplit "," |> List.map parse_tag
end

type tag =
    | Table of bool
    | Bed of bool
    | Comments of bool
    | Header of bool
    | HeaderUnderlined of bool
        (* | Rectangular of bool *)
        
    | CommentChar of char
    | Separator of string
        
type t = tag list

let of_cst (cst : Cst.t) : t =
  let parse_bool_tag (tag_name:string) (x:bool) = match tag_name with
    | "table" -> Table x
    | "bed" -> Bed x
    | "comments" -> Comments x
    | "header" -> Header x
    | "header_" -> HeaderUnderlined x
    | _ -> failwith (sprintf "%s tag is not Boolean" tag_name)
  in        
  let parse_string_tag (tag_name:string) (x:string) =
    let n = String.length x in
    if n = 0 then failwith (sprintf "%s tag's string value must not be empty" tag_name);
    match tag_name with
      | "comment-char" ->
          if n = 1 then CommentChar x.[0]
          else failwith (sprintf "%s tag's value must be single character" tag_name)
      | "separator" -> Separator x
      | _ -> failwith (sprintf "%s tag does not take string value" tag_name)
  in      
  let rec loop ans = function
    | [] -> ans
    | (tag_name, Cst.B x)::rest -> loop ((parse_bool_tag tag_name x)::ans) rest
    | (tag_name, Cst.S x)::rest -> loop ((parse_string_tag tag_name x)::ans) rest
  in
  loop [] cst |> List.rev

let of_string = of_cst -| Cst.of_string


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
