
include Core.Std

module Xmlm = Biocaml_internal_xmlm
  
module Stream = struct
  include Stream

  let next_exn = next
  let next s = try Some (next_exn s) with Stream.Failure -> None
end
