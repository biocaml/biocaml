open Core.Std

include Biocaml_unix.Std.Strand

let of_string s = ok_exn (of_string s)
