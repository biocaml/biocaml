open Core.Std
open Future_std

module Fastq = struct
  include Biocaml_fastq

  let read ic =
    read ic
    |> Pipe.map ~f:ok_exn

end

module Phred_score = struct
  include Biocaml_phred_score

  let of_char ?offset x = ok_exn (of_char ?offset x)
  let to_char ?offset t = ok_exn (to_char ?offset t)
  let of_int x = ok_exn (of_int x)
  let of_probability ?f x = ok_exn (of_probability ?f x)

end

module Range = struct
  include Biocaml_range

  let make lo hi = ok_exn (make lo hi)

end

module Sam = struct
  include Biocaml_sam

  let read ?start ic =
    (read ?start ic >>| ok_exn) >>= fun (hdr, pipe_r) ->
    hdr, Pipe.map pipe_r ~f:ok_exn

end

module Strand = struct
  include Biocaml_strand

  let of_string s = ok_exn (of_string s)

end
