
include Core.Std

module Xmlm = Biocaml_internal_xmlm
  
module Stream = struct
  include Stream

  let next s = try Some (next s) with Stream.Failure -> None
end
