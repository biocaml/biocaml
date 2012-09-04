
type t = [
| `gzip of t
| `raw_zip of t
| `gff of Biocaml_gff.tag list
| `wig of Biocaml_wig.tag list
| `bam
| `sam
| `bed
] with sexp

val guess_from_filename: string ->
  (t, [> `extension_absent | `extension_unknown of string ]) Core.Result.t

  
