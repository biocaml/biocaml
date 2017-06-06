open Core_kernel.Std

include Biocaml_unix.Seq_range

let make seq lo hi = ok_exn (make seq lo hi)
