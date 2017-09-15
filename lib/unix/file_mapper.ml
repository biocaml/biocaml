open CFStream
open Core_kernel

module B = Biocaml_base

type 'a t = string -> string -> f:('a -> 'a list) -> unit

exception Parse_error of string

let lines inbed outbed ~f =
  In_channel.with_file inbed ~f:(fun ic ->
      Out_channel.with_file outbed ~f:(fun oc ->
          Lines.read ic
          |> Stream.concat_map ~f:(fun x -> Stream.of_list (f x))
          |> Lines.write oc
        )
    )

let bed5 infile outfile ~f =
  lines infile outfile ~f:(fun line ->
      match B.Bed.Bed5.item_of_line line with
      | Ok item ->
        List.map (f item) ~f:B.Bed.Bed5.line_of_item
      | Error msg -> raise (Parse_error msg)
    )

