open Core_kernel

include Biocaml_unix.Roman_num

let of_roman x = ok_exn (of_roman x)
let of_arabic x = ok_exn (of_arabic x)
