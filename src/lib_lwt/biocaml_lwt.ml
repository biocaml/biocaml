open Biocaml_lwt_internal_pervasives

module Lines = struct
  include Biocaml_lines
  include MakeIO(Future_lwt)
end

module Fastq = struct
  include Biocaml_fastq
  include MakeIO(Future_lwt)
end
