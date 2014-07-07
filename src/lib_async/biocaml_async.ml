module Lines = struct
  include Biocaml_lines
  include MakeIO(Future_async)
end

module Fastq = struct
  include Biocaml_fastq
  include MakeIO(Future_async)
end

module Sam = struct
  include Biocaml_sam
  include MakeIO(Future_async)
end
