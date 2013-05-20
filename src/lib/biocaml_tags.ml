open Biocaml_internal_pervasives
open Result
module Gff = Biocaml_gff
module Wig = Biocaml_wig

type file_format = [
| `gzip of file_format
| `raw_zip of file_format
| `gff of Gff.Tags.t
| `wig of Wig.Tags.t
| `table of Biocaml_table.Row.Tags.t
| `bam
| `sam
| `bed
| `fastq
| `fasta of Biocaml_fasta.Tags.t
] with sexp

type t = [
| file_format
| `list of t list
] with sexp

let rec default_extension: file_format -> string = function
  | `gzip t -> sprintf "%s.gz" (default_extension t)
  | `raw_zip t -> sprintf "%s.rawzip" (default_extension t)
  | `gff _ -> "gff"
  | `wig _ -> "wig"
  | `bam     -> "bam"
  | `sam     -> "sam"
  | `bed     -> "bed"
  | `fastq   -> "fastq"
  | `fasta _ -> "fasta"
  | `table tags -> Biocaml_table.Row.Tags.default_extension tags

let to_tag (f : file_format) = (f :> t)

let rec guess_from_filename filename =
  match Filename.split_extension filename with
  | (f, Some "gz") ->
    guess_from_filename f
    >>= fun t ->
    return (`gzip t)
  | (_, Some term) ->
    begin match term with
    | "gff" -> return (`gff Gff.Tags.default)
    | "wig" -> return (`wig Wig.Tags.default)
    | "bam" -> return `bam
    | "sam" -> return `sam
    | "bed" -> return `bed
    | "fastq" -> return `fastq
    | "fasta" -> return (`fasta Biocaml_fasta.Tags.default)
    | "tsv" -> return (`table [`separator '\t'])
    | "csv" -> return (`table [`separator ','])
    | u -> fail (`extension_unknown u)
    end
  | (_, None) -> fail (`extension_absent)


let of_string s =
  let open Sexplib in
  try return (t_of_sexp (Sexp.of_string s))
  with e -> fail (`parse_tags e)

let to_string t =
  let open Sexplib in
  sexp_of_t t |! Sexp.to_string_hum
