open Core_kernel

include Biocaml_unix.Seq_range

let make seq lo hi = ok_exn (make seq lo hi)
