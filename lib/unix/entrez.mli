(** Entrez Utilities API

    This modules provides a partial access to Entrez databases such as
    Pubmed, Gene or Protein. The API proposed by the NCBI is based on HTTP
    requests, and this modules contains a couple of functions to ease the
    construction of appropriate URLs. This module also offers a more
    high-level access, with parsers for the answers from Entrez.

    Databases in Entrez can be seen as collections of records, each
    record representing an object of the database. The basic usage of
    the low-level API is first to search a database with the esearch
    utility.  Given a query string, esearch will return a collection
    of identifiers. These identifiers are then used to fetch the
    actual records with the efetch utility. These two operations are
    done in one call with the high-level API.
*)
open Core_kernel

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
(** Represents available databases *)


(** {4 Low level access} *)

(** For a documentation of the parameters, see
    {{:http://www.ncbi.nlm.nih.gov/books/NBK25499/}this reference} *)


val esearch_url :
  ?retstart:int -> ?retmax:int ->
  ?rettype:[`uilist | `count] ->
  ?field:string ->
  ?datetype:[`pdat | `mdat | `edat] ->
  ?reldate:int ->
  ?mindate:string -> ?maxdate:string ->
  database -> string -> string
(** Construction of esearch URLs. *)

type esearch_answer = {
  count : int ;
  retmax : int ;
  retstart : int ;
  ids : string list
}
(** Represents the result of a request to esearch *)

val esearch_answer_of_string : string -> esearch_answer
(** Parses an answer of esearch under XML format *)

val esummary_url :
  ?retstart:int -> ?retmax:int ->
  database -> string list -> string
(** Construction of esummary URLs *)

val efetch_url :
  ?rettype:string -> ?retmode:[`xml|`text|`asn_1] ->
  ?retstart:int -> ?retmax:int ->
  ?strand:[`plus|`minus] ->
  ?seq_start:int -> ?seq_stop:int ->
  database -> string list -> string
(** Construction of efetch URLs. Note that this access method does not
    support more than 200 ids. For legible values of [rettype] and
    [retmode] please consult
    {{:http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}the
    official specification}. *)

(** {4 High level access} *)

(** A signature for an HTTP request framework *)
module type Fetch = sig
  type 'a fetched

  val fetch : string -> (string -> 'a) -> 'a fetched
  val ( >>= ) : 'a fetched -> ('a -> 'b fetched) -> 'b fetched
  val ( >|= ) : 'a fetched -> ('a -> 'b) -> 'b fetched
end

module Make(F : Fetch) : sig
  open F

  module Object_id : sig
    type t = [`int of int | `string of string ]
    val to_string : t -> string
  end

  module Dbtag : sig
    type t = {
      db : string ;
      tag : Object_id.t ;
    }
  end

  module Gene_ref : sig
    type t = {
      locus : string option ;
      allele : string option ;
      desc : string option ;
      maploc : string option ;
      pseudo : bool option ;
      db : Dbtag.t list ;
    }
  end

  module PubmedSummary : sig
    type t = { pmid : int ;
               doi : string option ;
               pubdate : string option ;
               source : string option ;
               title : string }
    val search : string -> t list fetched
  end

  module Pubmed : sig
    type t =  { pmid : int ;
                title : string ;
                abstract : string }
    val search : string -> t list fetched
  end

  module Gene : sig
    type t = { 
      _type : [ `unknown | `tRNA | `rRNA | `snRNA | `scRNA |
                `snoRNA | `protein_coding | `pseudo | `transposon | `miscRNA |
                `ncRNA | `other ] ;
      summary : string option ;
      gene : Gene_ref.t ;
    }
    val search : string -> t list fetched
  end

end




















