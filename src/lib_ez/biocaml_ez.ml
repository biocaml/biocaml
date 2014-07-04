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

module Phred_score = struct
  include Biocaml_phred_score

  let of_ascii ?offset x = ok_exn (of_ascii ?offset x)
  let to_ascii ?offset t = ok_exn (to_ascii ?offset t)
  let of_int x = ok_exn (of_int x)
  let of_probability ?f x = ok_exn (of_probability ?f x)

end

module Range = struct
  include Biocaml_range

  let make lo hi = ok_exn (make lo hi)

end
