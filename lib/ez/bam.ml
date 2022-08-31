open CFStream
include Biocaml_unix.Bam

let with_file fn ~f =
  with_file fn ~f:(fun h xs -> Ok (f h (Stream.map xs ~f:ok_exn))) |> ok_exn
;;

let with_file0 fn ~f =
  with_file0 fn ~f:(fun h xs -> Ok (f h (Stream.map xs ~f:ok_exn))) |> ok_exn
;;
