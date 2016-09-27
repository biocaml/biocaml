open Core_kernel.Std

include Biocaml_unix.Std.Range

let make lo hi = ok_exn (make lo hi)
