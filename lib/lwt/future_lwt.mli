open Core_kernel
open Lwt

include Biocaml_unix.Future.S
  with type 'a Deferred.t = 'a Lwt.t
  and type 'a Pipe.Reader.t = 'a Lwt_stream.t
  and type Reader.t = Lwt_io.input_channel
  and type Writer.t = Lwt_io.output_channel
