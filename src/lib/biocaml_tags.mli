
type t = [
| `gzip of t
| `raw_zip of t
| `gff of Biocaml_gff.tag list
| `wig of Biocaml_wig.tag list
| `bam
| `sam
| `bed
| `fastq
| `table of char
| `fasta of [`int | `char | `unknown ]
] with sexp

val default_extension: t -> string

val guess_from_filename: string ->
  (t, [> `extension_absent | `extension_unknown of string ]) Core.Result.t


val of_string: string -> (t, [> `parse_tags of exn]) Core.Result.t
val to_string: t -> string

