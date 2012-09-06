open Batteries
open Printf

(* ********************************* *)
(* Preliminary stuff for xml parsing *)
(* ********************************* *)
type tree = E of Xmlm.tag * tree list | D of string

let in_tree i = 
  let el tag children = E (tag, children)  in
  let data d = D d in
  Xmlm.input_doc_tree ~el ~data i

let leaf f k = function
    E (_,children) ->
      List.find_map 
	(function 
	  | E ((tag,_), [D d]) when snd tag = k -> Some (f d)
	  | _  -> None)
	children
  | _ -> raise Not_found

let ileaf = leaf int_of_string
let sleaf = leaf identity

let leaves f k t = match t with
    E (_,children) ->
      List.filter_map 
	(function 
	  | E ((tag,_), [D d]) when snd tag = k -> Some (f d)
	  | _  -> None)
	children
  | _ -> []

let ileaves = leaves int_of_string
let sleaves = leaves identity

let child k = function
    E (_,children) ->
      List.find_map 
	(function 
	  | E ((tag,_), _) as r when snd tag = k -> Some r
	  | _  -> None)
	children
  | _ -> raise Not_found



(* 
   exhaustive list of databases:
   http://www.ncbi.nlm.nih.gov/books/NBK25497/table/chapter2.chapter2_table1/?report=objectonly 
*)
type database = [
| `gene
| `genome
| `geodatasets
| `geoprofiles
| `protein
| `pubmed
| `pubmedcentral
| `sra
| `unigene
| `taxonomy
]

let id_of_database = function
  | `pubmed -> "pubmed"
  | `gene -> "gene"
  | `unigene -> "unigene"
  | `genome -> "genome"
  | `geoprofiles -> "geoprofiles"
  | `geodatasets -> "geodatasets"
  | `pubmedcentral -> "pmc"
  | `protein -> "protein"
  | `sra -> "sra"
  | `taxonomy -> "taxonomy"

let search_base_url = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"

let parameters l = 
  List.filter_map identity l
  |> List.map (fun (k,v) -> sprintf "%s=%s" k v)
  |> String.concat "&"
  
let string_of_datetype = function
  | `pdat -> "pdat"
  | `mdat -> "mdat"
  | `edat -> "edat"


let esearch_url ?retstart ?retmax ?rettype ?field ?datetype ?reldate ?mindate ?maxdate database query = 
  search_base_url ^ "?" ^ parameters Option.([
    Some ("db", id_of_database database) ;
    Some ("term", Netencoding.Url.encode query) ;
    map (fun i -> "retstart", string_of_int i) retstart ;
    map (fun i -> "retmax", string_of_int i) retmax ;
    map (function `uilist -> ("rettype", "uilist") | `count -> ("rettype", "count")) rettype ;
    map (fun s -> "field",s) field ;
    map (fun dt -> "datetype", string_of_datetype dt) datetype ;
    map (fun d -> "mindate", d) mindate ;
    map (fun d -> "maxdate", d) maxdate ;
  ])
  
  

type esearch_answer = {
  count : int ;
  retmax : int ;
  retstart : int ;
  ids : string list
}

let esearch_answer_of_tree = function 
  | E (((_,"eSearchResult"),_),_) as t -> {
      count = ileaf "count" t ;
      retmax = ileaf "retmax" t ;
      retstart = ileaf "retstart" t ;
      ids = child "IdList" t |> sleaves "Id"
    }
  | _ -> assert false

let esearch_answer_of_string str = 
  Xmlm.make_input (`String (0,str))
  |> in_tree 
  |> snd
  |> esearch_answer_of_tree

let efetch_url ?rettype ?retmode db ids = assert false




















