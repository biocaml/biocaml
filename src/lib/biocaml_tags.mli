(** Meta-information about files and their formats. *)


type file_format = [
| `gzip of file_format
| `raw_zip of file_format
| `gff of Biocaml_gff.Tags.t
| `wig of Biocaml_wig.Tags.t
| `table of Biocaml_table.Row.Tags.t
| `bam
| `sam
| `bed
| `fastq
| `fasta of Biocaml_fasta.Tags.t
]
(** Description of file formats. *)

type t = [
| file_format
| `list of t list
]
(** Description of multiple file formats. *)

val to_tag: file_format -> t
(** Do a practical coercion. *)

val default_extension: file_format -> string
(** Get a filename extension for a given
    format. E.g. [default_extension (`gzip (`fasta `char))] will be
    ["fasta.gz"]. *)

val guess_from_filename: string ->
  (file_format, [> `extension_absent | `extension_unknown of string ]) Core.Result.t
(** Get a tag as precise as possible for a given filename. *)

val of_string: string -> (t, [> `parse_tags of exn]) Core.Result.t
(** Parse a tag specification (the format is, for now, based on
    S-Expressions, but this will change). *)

val to_string: t -> string
(** Convert a [t]ag to its string specification. *)

(** {2 S-Expression Conversions} *)

val file_format_of_sexp: Sexplib.Sexp.t -> file_format
val sexp_of_file_format: file_format -> Sexplib.Sexp.t
include Core.Sexpable.S with type t := t

