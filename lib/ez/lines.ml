include Biocaml_unix.Lines

let file_mapper inbed outbed ~f =
  In_channel.with_file inbed ~f:(fun ic ->
    Out_channel.with_file outbed ~f:(fun oc ->
      read ic
      |> Biocaml_unix.CFStream.concat_map ~f:(fun x -> Stream.of_list (f x))
      |> write oc))
;;
