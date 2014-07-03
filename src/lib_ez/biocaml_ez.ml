open Core.Std
open Future_std

module Fastq = struct
  include Biocaml_fastq

  let read ic =
    read ic
    |> Pipe.map ~f:ok_exn

  let read_file ?buf_len file =
    read_file ?buf_len file >>| fun pipe_r ->
    Pipe.map pipe_r ~f:ok_exn

end

module Range = struct
  include Biocaml_range

  let make lo hi = ok_exn (make lo hi)

end
