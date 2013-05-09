(** Meta-information about files and their formats. *)


type t = [
| `gzip of t
| `raw_zip of t
| `gff of Biocaml_gff.tag list
| `wig of Biocaml_wig.Tags.t
| `bam
| `sam
| `bed
| `fastq
| `table of Biocaml_table.Row.Tags.t
| `fasta of Biocaml_fasta.Tags.t
]
(** Description of file formats. *)

val default_extension: t -> string
(** Get a filename extension for a given
    format. E.g. [default_extension (`gzip (`fasta `char))] will be
    ["fasta.gz"]. *)

val guess_from_filename: string ->
  (t, [> `extension_absent | `extension_unknown of string ]) Core.Result.t
(** Get a tag as precise as possible for a given filename. *)

val of_string: string -> (t, [> `parse_tags of exn]) Core.Result.t
(** Parse a tag specification (the format is, for now, based on
    S-Expressions, but this will change). *)

val to_string: t -> string
(** Convert a [t]ag to its string specification. *)

include Core.Sexpable.S with type t := t

