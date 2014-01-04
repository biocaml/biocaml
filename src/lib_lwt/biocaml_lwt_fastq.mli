open Biocaml_internal_pervasives
open Biocaml_fastq

val get_lwt_stream : Lwt_io.input_channel -> item Lwt_stream.t
