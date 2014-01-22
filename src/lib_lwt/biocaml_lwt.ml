open Biocaml_lwt_internal_pervasives

module Fastq = struct
  include Biocaml_fastq
  include MakeIO(Future_lwt)
end
