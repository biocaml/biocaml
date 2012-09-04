open Biocaml_internal_pervasives
open With_result

type t = [
| `gzip of t
| `raw_zip of t
| `gff of Biocaml_gff.tag list
| `wig of Biocaml_wig.tag list
| `bam
| `sam
| `bed
] with sexp

  
let rec guess_from_filename filename =
  match Filename.split_extension filename with
  | (f, Some "gz") ->
    guess_from_filename f
    >>= fun t ->
    return (`gzip t)
  | (_, Some term) ->
    begin match term with
    | "gff" -> return (`gff Biocaml_gff.default_tags)
    | "wig" -> return (`wig Biocaml_wig.default_tags)
    | "bam" -> return `bam
    | "sam" -> return `sam
    | "bed" -> return `bed
    | u -> fail (`extension_unknown u)
    end
  | (_, None) -> fail (`extension_absent)
