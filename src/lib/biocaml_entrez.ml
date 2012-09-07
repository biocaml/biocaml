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

let tree_of_string str = 
  in_tree (Xmlm.make_input (`String (0,str)))

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

let tag_of_tree = function
| E ((tag,_),_) -> Some (snd tag)
| D _ -> None
    
let child k = function
| E (_,children) ->
    begin
      try
        List.find_map 
	  (function 
	  | E ((tag,_), _) as r when snd tag = k -> Some r
	  | _  -> None)
	  children
      with Not_found -> (
        let tags = List.filter_map tag_of_tree children in
        let msg = sprintf "child: looked for %s but only got %s children" k (String.concat "," tags) in
        raise (Invalid_argument msg)
      )
    end
| D _ -> (
  let msg = sprintf "child: looked for %s tag but only got a PCDATA node" k in
  raise (Invalid_argument msg)
)



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
      count = ileaf "Count" t ;
      retmax = ileaf "RetMax" t ;
      retstart = ileaf "RetStart" t ;
      ids = child "IdList" t |> sleaves "Id"
    }
  | _ -> assert false

let esearch_answer_of_string str = 
  tree_of_string str
  |> snd
  |> esearch_answer_of_tree




let fetch_base_url = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"

let string_of_retmode = function
| `xml -> "xml"
| `asn_1 -> "asn.1"
| `text -> "text"

let efetch_url ?rettype ?retmode ?retstart ?retmax ?strand ?seq_start ?seq_stop db ids =
  if List.length ids > 200 
  then raise (Invalid_argument "Biocaml_entrez.efetch_url: cannot fetch more than 200 objects") ;
  fetch_base_url ^ "?" ^ parameters Option.([
    Some ("db", id_of_database db) ;
    Some ("id", String.concat "," ids) ;
    map (fun r -> "retmode", string_of_retmode r) retmode ;
    map (fun i -> "retstart", string_of_int i) retstart ;
    map (fun i -> "retmax", string_of_int i) retmax ;
    map (fun s -> "strand", match s with `plus -> "1" | `minus -> "2") strand ;
    map (fun i -> "seq_start", string_of_int i) seq_start ;
    map (fun i -> "seq_stop", string_of_int i) seq_stop ;
  ])


module type Fetch = sig
  type 'a fetched

  val fetch : string -> (string -> 'a) -> 'a fetched
  val ( >>= ) : 'a fetched -> ('a -> 'b fetched) -> 'b fetched
  val ( >|= ) : 'a fetched -> ('a -> 'b) -> 'b fetched
end

module Make(F : Fetch) = struct
  open F

  module Pubmed = struct
    type t = {
      pmid : int ;
      title : string ;
      abstract : string ;
    }

    let parse_book_document bd =
      { pmid = ileaf "PMID" bd ; 
        title = sleaf "ArticleTitle" bd ;
        abstract = child "Abstract" bd |> sleaf "AbstractText" }
      
    let parse_medline_citation mc = 
      let article = child "Article" mc in
      { pmid = ileaf "PMID" mc ;
        title = sleaf "ArticleTitle" article ;
        abstract = child "Abstract" article |> sleaf "AbstractText" }

    let parse_pubmed_article_set_element x = match tag_of_tree x with
    | Some "PubmedArticle" -> 
        Some (parse_medline_citation (child "MedlineCitation" x))
    | Some "PubmedBookArticle" ->
        Some (parse_book_document (child "BookDocument" x))
    | Some t -> 
        failwith (sprintf "Unexpected %s tag while parsing PubmedArticleSet element" t)
    | None -> None

    let parse_pubmed_article_set = function
    | E (((_,"PubmedArticleSet"),_),children) ->
        List.filter_map parse_pubmed_article_set_element children
    | _ -> assert false
          

    let search query = 
      let query_url = esearch_url `pubmed query in
      fetch query_url esearch_answer_of_string >>= fun answer ->
      let object_url = efetch_url ~retmode:`xml `pubmed answer.ids in
      fetch object_url (tree_of_string |- snd |- parse_pubmed_article_set)
  end
end





















