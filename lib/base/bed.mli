type parser_error = [ `Bed_parser_error of int * string ]
[@@deriving sexp]

type item = string * int * int * string list

val item_of_line : Line.t -> (item, string) Caml.result
val line_of_item : item -> Line.t

module Bed3 : sig
  type item = private {
    chrom : string ;
    chrom_start : int ;
    chrom_end : int ;
    others : string array ;
  }
  [@@deriving sexp]
end

module Bed4 : sig
  type item = private {
    chrom : string ;
    chrom_start : int ;
    chrom_end : int ;
    name : string ;
    others : string array ;
  }
  [@@deriving sexp]

end

module Bed5_raw : sig
  type item = private {
    chrom : string ;
    chrom_start : int ;
    chrom_end : int ;
    name : string ;
    score : int ;
    others : string list ;
  }
  [@@deriving sexp]

  val make :
    chrom:string ->
    chrom_start:int ->
    chrom_end:int ->
    name:string ->
    score:int ->
    ?others:string list ->
    unit -> (item, string) Caml.result

  val set_score : item -> int -> item

  val item_of_line : Line.t -> (item, string) Caml.result
  val line_of_item : item -> Line.t
end

module Bed5 : sig
  type item = private Bed5_raw.item
  [@@deriving sexp]

  val make :
    chrom:string ->
    chrom_start:int ->
    chrom_end:int ->
    name:string ->
    score:int ->
    ?others:string list ->
    unit -> (item, string) Caml.result

  val item_of_line : Line.t -> (item, string) Caml.result
  val line_of_item : item -> Line.t
end
