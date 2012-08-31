open Biocaml_internal_pervasives


type t = [
| `gzip of t
| `raw_zip of t
| `gff of Biocaml_gff.tag list
] with sexp

  
