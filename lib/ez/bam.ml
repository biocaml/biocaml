open Core_kernel
open CFStream

include Biocaml_unix.Bam

let with_file fn ~f =
  with_file fn ~f:(fun h xs ->
      Ok (f h (Stream.map xs ~f:ok_exn))
    )
  |> ok_exn
