include module type of Biocaml_unix.Lines

val file_mapper : string -> string -> f:(item -> item list) -> unit
