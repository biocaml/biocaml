open Core_kernel

include Biocaml_unix.Range

let make lo hi = ok_exn (make lo hi)
