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
    pileup : float ;
    log10pvalue : float ;
    fold_enrichment : float ;
    log10qvalue : float ;
    name : string ;
  }

  val parse : Line.t -> (item,  [> `Msg of string]) Result.t
end

module Broad_peaks : sig
  type item = {
    chr : string ;
    chr_start : int ;
    chr_end : int ;
    name : string ;
    score : int ;
    strand : string ;
    fold_change : float ;
    log10pvalue : float ;
    log10qvalue : float ;
  }

  val parse : Line.t -> (item,  [> `Msg of string]) Result.t
end
