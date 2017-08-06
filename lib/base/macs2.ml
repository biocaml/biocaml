open Base
open Rresult

module Xls = struct
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

  let header =
    "chr\tstart\tend\tlength\tabs_summit\tpileup\t-log10(pvalue)\tfold_enrichment\t-log10(qvalue)\tname"

  let parse_field f field x =
    try R.ok (f x)
    with _ -> R.error_msgf "Failed to parse field %s" field

  let parse_int = parse_field Int.of_string
  let parse_float = parse_field Float.of_string

  let parse line =
    match (line : Line.t :> string) with
    | "" -> R.ok (`Comment "")
    | line when String.(line = header) -> R.ok `Header
    | line ->
      if Char.(line.[0] = '#') then R.ok (`Comment (String.slice line 1 0))
      else
        match String.split ~on:'\t' line with
        | [ chr ; start ; end_ ; length ; abs_summit ;
            pileup ; log10pvalue ; fold_enrichment ;
            log10qvalue ; name ] ->
          parse_int "start" start >>= fun start ->
          parse_int "end" end_ >>= fun end_ ->
          parse_int "length" length >>= fun length ->
          parse_int "abs_summit" abs_summit >>= fun abs_summit ->
          parse_float "pileup" pileup >>= fun pileup ->
          parse_float "log10pvalue" log10pvalue >>= fun log10pvalue ->
          parse_float "log10qvalue" log10qvalue >>= fun log10qvalue ->
          parse_float "fold_enrichment" fold_enrichment >>| fun fold_enrichment ->
          `Record {
            chr ; start ; end_ ; length ; abs_summit ;
            pileup ; log10pvalue ; fold_enrichment ;
            log10qvalue ; name ;
          }
        | _ -> R.error_msg "Wrong number of fields"
end
