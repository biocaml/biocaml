open Core_kernel.Std

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

let rec string_of_tree = function
  | E (tag, _, children) ->
    sprintf "<%s>%s<%s/>" tag (String.concat ~sep:"" (List.map ~f:string_of_tree children)) tag
  | D s -> s

let attr k = function
  | E (_,attrs,_) ->
    List.find_map attrs ~f:(fun ((_,k'),value) -> if k = k' then Some value else None)
  | _ -> None

let battr k x = Option.map (attr k x) ~f:bool_of_string

let leaf_exn f k x =
  try
    match x with
    | E (_,_,children) ->
      (match List.find_map children ~f:(function
           | E (tag,_, [D d]) when tag = k -> Some (f d)
           | _  -> None)
       with
       | Some x -> x
       | None -> raise Not_found
      )
    | D _ -> raise Not_found
  with Not_found ->
    invalid_arg (sprintf "Entrez.leaf: no %s child" k)

let ileaf_exn = leaf_exn int_of_string
let sleaf_exn = leaf_exn Fn.id

let sleaf k x = try Some (sleaf_exn k x) with Invalid_argument _ -> None

let leaves f k t = match t with
    E (_,_,children) ->
    List.filter_map children ~f:(function
        | E (tag,_, [D d]) when tag = k -> Some (f d)
        | _  -> None)
  | _ -> []

let sleaves = leaves Fn.id

let tag_of_tree = function
  | E (tag,_,_) -> Some tag
  | D _ -> None

let echild_exn k = function
  | E (_, _, children) ->
    begin
      try
        (match List.find_map children ~f:(function
             | E (tag,_, _) as r when tag = k -> Some r
             | _  -> None)
         with
         | Some x -> x
         | None -> raise Not_found
        )
      with Not_found -> (
          let tags = List.filter_map ~f:tag_of_tree children in
          let msg = sprintf "child: looked for %s but only got %s children" k (String.concat ~sep:"," tags) in
          raise (Invalid_argument msg)
        )
    end
  | D _ -> (
      let msg = sprintf "child: looked for %s tag but only got a PCDATA node" k in
      raise (Invalid_argument msg)
    )

let echild k x = try Some (echild_exn k x) with _ -> None

let fold_echildren ?tag f =
  let pred = Option.value_map tag ~default:(fun _ -> true) ~f:( = ) in
  fun x init ->
    match x with
    | E (_,_,children) ->
      List.fold_right children ~init ~f:(fun x accu ->
          match x with
          | E (tag,_,_) as x when pred tag -> f x accu
          | _ -> accu)
    | D _ -> init

let map_echildren ?tag f x = fold_echildren ?tag (fun x accu -> (f x) :: accu) x []

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
  List.filter_map ~f:Fn.id l
  |> List.map ~f:(fun (k,v) -> sprintf "%s=%s" k v)
  |> String.concat ~sep:"&"

let string_of_datetype = function
  | `pdat -> "pdat"
  | `mdat -> "mdat"
  | `edat -> "edat"


let esearch_url ?retstart ?retmax ?rettype ?field ?datetype ?reldate ?mindate ?maxdate database query =
  search_base_url ^ "?" ^ parameters Option.([
      Some ("db", id_of_database database) ;
      Some ("term", Uri.pct_encode query) ;
      map ~f:(fun i -> "retstart", string_of_int i) retstart ;
      map ~f:(fun i -> "reldate", string_of_int i) reldate ;
      map ~f:(fun i -> "retmax", string_of_int i) retmax ;
      map ~f:(function `uilist -> ("rettype", "uilist") | `count -> ("rettype", "count")) rettype ;
      map ~f:(fun s -> "field",s) field ;
      map ~f:(fun dt -> "datetype", string_of_datetype dt) datetype ;
      map ~f:(fun d -> "mindate", d) mindate ;
      map ~f:(fun d -> "maxdate", d) maxdate ;
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
      ids = echild_exn "IdList" t |> sleaves "Id"
    }
  | _ -> assert false

let esearch_answer_of_string str =
  tree_of_string str
  |> snd
  |> esearch_answer_of_tree


let summary_base_url = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"

let esummary_url ?retstart ?retmax db ids =
  if List.length ids > 200
  then raise (Invalid_argument "Entrez.esummary_url: cannot fetch more than 200 summaries") ;
  summary_base_url ^ "?" ^ parameters Option.([
      Some ("db", id_of_database db) ;
      Some ("id", String.concat ~sep:"," ids) ;
      Some ("version", "2.0") ;
      map ~f:(fun i -> "retstart", string_of_int i) retstart ;
      map ~f:(fun i -> "retmax", string_of_int i) retmax ;
    ])


let fetch_base_url = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"

let string_of_retmode = function
  | `xml -> "xml"
  | `asn_1 -> "asn.1"
  | `text -> "text"

let efetch_url ?rettype ?retmode ?retstart ?retmax ?strand ?seq_start ?seq_stop db ids =
  if List.length ids > 200
  then raise (Invalid_argument "Entrez.efetch_url: cannot fetch more than 200 records") ;
  fetch_base_url ^ "?" ^ parameters Option.([
      Some ("db", id_of_database db) ;
      Some ("id", String.concat ~sep:"," ids) ;
      map ~f:(fun r -> "rettype", r) rettype ;
      map ~f:(fun r -> "retmode", string_of_retmode r) retmode ;
      map ~f:(fun i -> "retstart", string_of_int i) retstart ;
      map ~f:(fun i -> "retmax", string_of_int i) retmax ;
      map ~f:(fun s -> "strand", match s with `plus -> "1" | `minus -> "2") strand ;
      map ~f:(fun i -> "seq_start", string_of_int i) seq_start ;
      map ~f:(fun i -> "seq_stop", string_of_int i) seq_stop ;
    ])


module type Fetch = sig
  type 'a fetched

  val fetch : string -> (string -> 'a) -> 'a fetched
  val ( >>= ) : 'a fetched -> ('a -> 'b fetched) -> 'b fetched
  val ( >|= ) : 'a fetched -> ('a -> 'b) -> 'b fetched
end

module Make(F : Fetch) = struct
  open F

  (* DTD for the databases can be found at http://www.ncbi.nlm.nih.gov/data_specs/dtd/NCBI_Entrezgene.dtd *)

  let search_and_fetch database of_xml query =
    let query_url = esearch_url database query in
    fetch query_url esearch_answer_of_string >>= fun answer ->
    let object_url = efetch_url ~retmode:`xml database answer.ids in
    fetch object_url (fun x -> x |> tree_of_string |> snd |> of_xml)

  let search_and_summary database of_xml query =
    let query_url = esearch_url database query in
    fetch query_url esearch_answer_of_string >>= fun answer ->
    let object_url = esummary_url database answer.ids in
    fetch object_url (fun x -> x |> tree_of_string |> snd |> of_xml)

  module Object_id = struct
    type t = [`int of int | `string of string ]

    let to_string = function
      | `int i -> string_of_int i
      | `string s -> s

    let of_xml x =
      try `int (ileaf_exn "Object-id_id" x)
      with _ -> (
          try `string (sleaf_exn "Object-id_str" x)
          with _ ->
            invalid_arg (sprintf "Entrez.Make.Object_id.of_xml: %s" (string_of_tree x))
        )
  end


  module Dbtag = struct
    type t = {
      db : string ;
      tag : Object_id.t ;
    }

    let of_xml x = {
      db = sleaf_exn "Dbtag_db" x ;
      tag = Object_id.of_xml (x |> echild_exn "Dbtag_tag" |> echild_exn "Object-id")
    }

  end


  module Gene_ref = struct
    type t = {
      locus : string option ;
      allele : string option ;
      desc : string option ;
      maploc : string option ;
      pseudo : bool option ;
      db : Dbtag.t list ;
    }

    let of_xml t =
      let t = echild_exn "Gene-ref" t in
      {
        locus = sleaf "Gene-ref_locus" t ;
        allele = sleaf "Gene-ref_allele" t ;
        desc = sleaf "Gene-ref_desc" t ;
        maploc = sleaf "Gene-ref_maploc" t ;
        pseudo = Option.bind (echild "Gene-ref_pseudo" t) (battr "value") ;
        db =
          Option.value_map
            (echild "Gene-ref_db" t)
            ~default:[]
            ~f:(map_echildren ~tag:"Dbtag" Dbtag.of_xml) ;
      }
  end

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
      map_echildren
        ~tag:"ArticleId"
        (fun x -> sleaf_exn "IdType" x, sleaf_exn "Value" x)
        x

    let parse_document_summary x =
      let article_ids = parse_article_ids (echild_exn "ArticleIds" x) in
      { pmid = int_of_string (List.Assoc.find_exn ~equal:String.equal article_ids "pubmed") ;
        doi = List.Assoc.find ~equal:String.equal article_ids "doi" ;
        pubdate = sleaf "PubDate" x ;
        source = sleaf "Source" x ;
        title = sleaf_exn "Title" x }

    let parse_eSummaryResult x =
      map_echildren
        ~tag:"DocumentSummary"
        parse_document_summary
        (echild_exn "DocumentSummarySet" x)


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
        abstract = echild_exn "Abstract" bd |> sleaf_exn "AbstractText" }

    let parse_medline_citation mc =
      let article = echild_exn "Article" mc in
      { pmid = ileaf_exn "PMID" mc ;
        title = sleaf_exn "ArticleTitle" article ;
        abstract = echild_exn "Abstract" article |> sleaf_exn "AbstractText" }

    let parse_pubmed_article_set_element x = match tag_of_tree x with
      | Some "PubmedArticle" ->
        Some (parse_medline_citation (echild_exn "MedlineCitation" x))
      | Some "PubmedBookArticle" ->
        Some (parse_book_document (echild_exn "BookDocument" x))
      | Some t ->
        failwith (sprintf "Unexpected %s tag while parsing PubmedArticleSet element" t)
      | None -> None

    let parse_pubmed_article_set = function
      | E ("PubmedArticleSet",_,children) ->
        List.filter_map ~f:parse_pubmed_article_set_element children
      | _ -> assert false

    let search = search_and_fetch `pubmed parse_pubmed_article_set
  end

  (* http://www.ncbi.nlm.nih.gov/data_specs/dtd/NCBI_Entrezgene.mod.dtd *)
  module Gene = struct
    type t = {
      _type : [ `unknown | `tRNA | `rRNA | `snRNA | `scRNA |
                `snoRNA | `protein_coding | `pseudo | `transposon | `miscRNA |
                `ncRNA | `other ] ;

      summary : string option ;
      gene : Gene_ref.t ;
    }

    let type_of_int = function
      | 0 -> `unknown
      | 1 -> `tRNA
      | 2 -> `rRNA
      | 3 -> `snRNA
      | 4 -> `scRNA
      | 5 -> `snoRNA
      | 6 -> `protein_coding
      | 7 -> `pseudo
      | 8 -> `transposon
      | 9 -> `miscRNA
      | 10 -> `ncRNA
      | 11 -> `ncRNA
      | n -> invalid_arg (sprintf "Entrez.Make.Gene.type_of_int: %d" n)

    let parse_entrez_gene = function
      | E ("Entrezgene",_,_) as x -> Some {
          summary = sleaf "Entrezgene_summary" x ;
          _type = type_of_int (ileaf_exn "Entrezgene_type" x) ;
          gene = Gene_ref.of_xml (echild_exn "Entrezgene_gene" x) ;
        }
      | _ -> None

    let parse_entrez_gene_set = function
      | E ("Entrezgene-Set",_,children) ->
        List.filter_map ~f:parse_entrez_gene children
      | _ -> assert false

    let search query =
      let database = `gene in
      let of_xml = parse_entrez_gene_set in
      let query_url = esearch_url database query in
      (* print_endline query_url ; *)
      fetch query_url esearch_answer_of_string >>= fun answer ->
      let object_url = efetch_url ~retmode:`xml database answer.ids in
      (* print_endline object_url ; *)
      fetch object_url (fun x -> x |> tree_of_string |> snd |> of_xml)
  end
end
