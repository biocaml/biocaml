open Core_kernel

include Biocaml_unix.Strand

let of_string s = ok_exn (of_string s)
