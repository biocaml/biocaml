open Biocaml_async_internal_pervasives

module Fastq = struct
  include Biocaml_fastq
  include MakeIO(Future_async)
end
