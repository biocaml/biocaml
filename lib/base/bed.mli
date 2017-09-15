type parser_error = [ `Bed_parser_error of int * string ]
[@@deriving sexp]


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

module Raw_bed5 : sig
  type item = {
    chrom : string ;
    chrom_start : int ;
    chrom_end : int ;
    name : string ;
    score : int ;
    others : string array ;
  }
  [@@deriving sexp]

  val item_of_line : Line.t -> (item, string) result
end

module Bed5 : sig
  type item = private {
    chrom : string ;
    chrom_start : int ;
    chrom_end : int ;
    name : string ;
    score : int ;
    others : string array ;
  }
  [@@deriving sexp]

  val item_of_line : Line.t -> (item, string) result
  val line_of_item : item -> Line.t
end

