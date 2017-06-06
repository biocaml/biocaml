open Core_kernel.Std

include Biocaml_unix.Phred_score

let of_char ?offset x = ok_exn (of_char ?offset x)
let to_char ?offset t = ok_exn (to_char ?offset t)
let of_int x = ok_exn (of_int x)
let of_probability ?f x = ok_exn (of_probability ?f x)
