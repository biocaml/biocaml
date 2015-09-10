open Core.Std
open Future_unix.Std
open Future

include Biocaml_unix.Std.Sam

let read ?start ic =
  (read ?start ic >>| ok_exn) >>= fun (hdr, pipe_r) ->
  hdr, Pipe.map pipe_r ~f:ok_exn
