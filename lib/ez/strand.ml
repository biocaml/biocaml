open Core_kernel.Std

include Biocaml_unix.Std.Strand

let of_string s = ok_exn (of_string s)
