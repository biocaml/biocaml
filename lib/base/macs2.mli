(** Interaction with MACS2 peak caller *)

(** XLS output *)
module Xls : sig
  type item = [
    | `Comment of string
    | `Record of record
    | `Header
  ]
  and record = {
    chr : string ;
    start : int ;
    end_ : int ;
    length : int ;
    abs_summit : int ;
    pileup : int ;
    log10pvalue : float ;
    fold_enrichment : float ;
    log10qvalue : float ;
    name : string ;
  }

  val parse : Line.t -> (item,  [> `Msg of string]) result
end
