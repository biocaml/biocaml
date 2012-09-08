open Batteries
open Printf

(* ********************************* *)
(* Preliminary stuff for xml parsing *)
(* ********************************* *)
type tree = E of string * Xmlm.attribute list * tree list | D of string

let in_tree i = 
  let el ((_,tag),attrs) children = E (tag, attrs, children)  in
  let data d = D d in
  Xmlm.input_doc_tree ~el ~data i

let tree_of_string str = 
  in_tree (Xmlm.make_input (`String (0,str)))

let leaf_exn f k = function
    E (_,_,children) ->
      List.find_map 
	(function 
	  | E (tag,_, [D d]) when tag = k -> Some (f d)
	  | _  -> None)
	children
  | _ -> raise Not_found

let ileaf_exn = leaf_exn int_of_string
let sleaf_exn = leaf_exn identity

let ileaf k x = try Some (ileaf_exn k x) with Not_found -> None
let sleaf k x = try Some (sleaf_exn k x) with Not_found -> None


let leaves f k t = match t with
    E (_,_,children) ->
      List.filter_map 
	(function 
	  | E (tag,_, [D d]) when tag = k -> Some (f d)
	  | _  -> None)
	children
  | _ -> []

let ileaves = leaves int_of_string
let sleaves = leaves identity

let tag_of_tree = function
| E (tag,_,_) -> Some tag
| D _ -> None
    
let child k = function
| E (_,_,children) ->
    begin
      try
        List.find_map 
	  (function 
	  | E (tag,_, _) as r when tag = k -> Some r
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

let fold_echildren ?tag f = 
  let pred = Core.Option.value_map tag ~default:(fun _ -> true) ~f:( = ) in
  fun x init -> 
    match x with
    | E (_,_,children) ->
        List.fold_right
          (fun x accu -> 
            match x with 
            | E (tag,_,_) as x when pred tag -> f x accu
            | _ -> accu)
          children
          init
    | D _ -> init

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
  | E ("eSearchResult",_,_) as t -> {
      count = ileaf_exn "Count" t ;
      retmax = ileaf_exn "RetMax" t ;
      retstart = ileaf_exn "RetStart" t ;
      ids = child "IdList" t |> sleaves "Id"
    }
  | _ -> assert false

let esearch_answer_of_string str = 
  tree_of_string str
  |> snd
  |> esearch_answer_of_tree


let summary_base_url = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"

let esummary_url ?retstart ?retmax db ids =
  if List.length ids > 200 
  then raise (Invalid_argument "Biocaml_entrez.esummary_url: cannot fetch more than 200 summaries") ;
  summary_base_url ^ "?" ^ parameters Option.([
    Some ("db", id_of_database db) ;
    Some ("id", String.concat "," ids) ;
    Some ("version", "2.0") ;
    map (fun i -> "retstart", string_of_int i) retstart ;
    map (fun i -> "retmax", string_of_int i) retmax ;
  ])


let fetch_base_url = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"

let string_of_retmode = function
| `xml -> "xml"
| `asn_1 -> "asn.1"
| `text -> "text"

let efetch_url ?rettype ?retmode ?retstart ?retmax ?strand ?seq_start ?seq_stop db ids =
  if List.length ids > 200 
  then raise (Invalid_argument "Biocaml_entrez.efetch_url: cannot fetch more than 200 records") ;
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

  let search_and_fetch database of_xml query =
    let query_url = esearch_url database query in
    fetch query_url esearch_answer_of_string >>= fun answer ->
    let object_url = efetch_url ~retmode:`xml database answer.ids in
    fetch object_url (tree_of_string |- snd |- of_xml)

  let search_and_summary database of_xml query =
    let query_url = esearch_url database query in
    fetch query_url esearch_answer_of_string >>= fun answer ->
    let object_url = esummary_url database answer.ids in
    fetch object_url (tree_of_string |- snd |- of_xml)

  module PubmedSummary = struct
    (* DTD is at http://www.ncbi.nlm.nih.gov/entrez/query/DTD/eSummaryDTD/eSummary_pubmed.dtd *)

    type t = {
      pmid : int ;
      doi : string option ;
      pubdate : string option ;
      source : string option ;
      title : string ;
    }

    let parse_article_ids x =
      fold_echildren
        ~tag:"ArticleId"
        (fun x accu -> (sleaf_exn "IdType" x, sleaf_exn "Value" x) :: accu)
        x []

    let parse_document_summary x = 
      let article_ids = parse_article_ids (child "ArticleIds" x) in
      { pmid = int_of_string (List.assoc "pubmed" article_ids) ;
        doi = (try Some (List.assoc "doi" article_ids) with Not_found -> None) ;
        pubdate = sleaf "PubDate" x ;
        source = sleaf "Source" x ;
        title = sleaf_exn "Title" x }
      
    let parse_eSummaryResult x = 
      fold_echildren 
        ~tag:"DocumentSummary"
        (fun x accu -> (parse_document_summary x) :: (accu : t list))
        (child "DocumentSummarySet" x) []


    let search = search_and_summary `pubmed parse_eSummaryResult
  end

  module Pubmed = struct
    type t = {
      pmid : int ;
      title : string ;
      abstract : string ;
    }

    let parse_book_document bd =
      { pmid = ileaf_exn "PMID" bd ; 
        title = sleaf_exn "ArticleTitle" bd ;
        abstract = child "Abstract" bd |> sleaf_exn "AbstractText" }

    let parse_medline_citation mc = 
      let article = child "Article" mc in
      { pmid = ileaf_exn "PMID" mc ;
        title = sleaf_exn "ArticleTitle" article ;
        abstract = child "Abstract" article |> sleaf_exn "AbstractText" }

    let parse_pubmed_article_set_element x = match tag_of_tree x with
    | Some "PubmedArticle" -> 
        Some (parse_medline_citation (child "MedlineCitation" x))
    | Some "PubmedBookArticle" ->
        Some (parse_book_document (child "BookDocument" x))
    | Some t -> 
        failwith (sprintf "Unexpected %s tag while parsing PubmedArticleSet element" t)
    | None -> None

    let parse_pubmed_article_set = function
    | E ("PubmedArticleSet",_,children) ->
        List.filter_map parse_pubmed_article_set_element children
    | _ -> assert false
        
    let search = search_and_fetch `pubmed parse_pubmed_article_set
  end


end





















