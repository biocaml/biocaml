(** Entrez Utilities API

This modules provides a partial access to Entrez databases
such as Pubmed, Gene or Protein. The API proposed by the
NCBI is based on HTTP requests, and this modules contains
a couple of functions to ease the construction of appropriate
URLs. It is thus of rather low-level; in particular, there is
no support for parsing the answers, which are simple strings.

Databases in Entrez can be seen as collections of records, each
record representing an object of the database. The basic usage
of the API is first to search a database with the esearch utility.
Given a query string, esearch will return a collection of
identifiers. These identifiers are then used to fetch the actual
records with the efetch utility.
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
(** Represents available databases *)


(** {4 Low level access} *)

(** For a documentation of the parameters, see
    http://www.ncbi.nlm.nih.gov/books/NBK25499/ *)


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
    [retmode] please consult:
    http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly *)

module type Fetch = sig
  type 'a fetched

  val fetch : string -> (string -> 'a) -> 'a fetched
  val ( >>= ) : 'a fetched -> ('a -> 'b fetched) -> 'b fetched
  val ( >|= ) : 'a fetched -> ('a -> 'b) -> 'b fetched
end

module Make(F : Fetch) : sig
  open F

  module Pubmed : sig
    type t = {
      pmid : int ;
      title : string ;
      abstract : string ;
    }

    val search : string -> t list fetched
  end

end




















