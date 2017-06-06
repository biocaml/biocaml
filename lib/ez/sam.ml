open Core_kernel.Std
open CFStream

include Biocaml_unix.Sam

let read ?start ic =
  (read ?start ic |> ok_exn) |> fun (hdr, pipe_r) ->
  hdr, Stream.map pipe_r ~f:ok_exn
