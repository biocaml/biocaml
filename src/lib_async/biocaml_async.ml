open Future_async_unix.Std

module Lines = struct
  include Biocaml_lines
  include MakeIO(Future)
end

module Fastq = struct
  include Biocaml_fastq
  include MakeIO(Future)
end

module Sam = struct
  include Biocaml_sam
  include MakeIO(Future)
end
