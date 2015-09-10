open Core.Std

include Biocaml_unix.Std.Roman_num

let of_roman x = ok_exn (of_roman x)
let of_arabic x = ok_exn (of_arabic x)
