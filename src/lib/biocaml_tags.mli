
type t = [
| `gzip of t
| `raw_zip of t
| `gff of Biocaml_gff.tag list
| `wig of Biocaml_wig.tag list
| `bam
| `sam
| `bed
| `fastq
| `fasta of [`int | `char | `unknown ]
] with sexp

val default_extension: t -> string
  
val guess_from_filename: string ->
  (t, [> `extension_absent | `extension_unknown of string ]) Core.Result.t

  
