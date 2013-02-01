open Biocaml_internal_pervasives
open Result
module Gff = Biocaml_gff
module Wig = Biocaml_wig

type t = [
| `gzip of t
| `raw_zip of t
| `gff of Gff.tag list
| `wig of Wig.tag list
| `bam
| `sam
| `bed
| `fastq
| `fasta of [`int | `char | `unknown ]
] with sexp

let rec default_extension = function
| `gzip t -> sprintf "%s.gz" (default_extension t)
| `raw_zip t -> sprintf "%s.rawzip" (default_extension t)
| `gff _ -> "gff"
| `wig _ -> "wig"
| `bam     -> "bam"  
| `sam     -> "sam"
| `bed     -> "bed"
| `fastq   -> "fastq"
| `fasta _ -> "fasta" 

  
let rec guess_from_filename filename =
  match Filename.split_extension filename with
  | (f, Some "gz") ->
    guess_from_filename f
    >>= fun t ->
    return (`gzip t)
  | (_, Some term) ->
    begin match term with
    | "gff" -> return (`gff Gff.default_tags)
    | "wig" -> return (`wig Wig.default_tags)
    | "bam" -> return `bam
    | "sam" -> return `sam
    | "bed" -> return `bed
    | "fastq" -> return `fastq
    | "fasta" -> return (`fasta `unknown)
    | u -> fail (`extension_unknown u)
    end
  | (_, None) -> fail (`extension_absent)
