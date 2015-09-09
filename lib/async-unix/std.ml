open Future_async_unix.Std
open Biocaml_unix.Std

module Lines = struct
  include Lines
  include MakeIO(Future)
end

module Fastq = struct
  include Fastq
  include MakeIO(Future)
end

module Sam = struct
  include Sam
  include MakeIO(Future)
end
