module B = Biocaml

type 'a t = string -> string -> f:('a -> 'a list) -> unit

exception Parse_error of string

let lines inbed outbed ~f =
  In_channel.with_file inbed ~f:(fun ic ->
    Out_channel.with_file outbed ~f:(fun oc ->
      Lines.read ic
      |> CFStream.Stream.concat_map ~f:(fun x -> CFStream.Stream.of_list (f x))
      |> Lines.write oc))
;;

let line_mapper item_parser item_unparser infile outfile ~f =
  lines infile outfile ~f:(fun line ->
    match item_parser line with
    | Ok item -> List.map (f item) ~f:item_unparser
    | Error msg -> raise (Parse_error msg))
;;

let bed5 = B.Bed.Bed5.(line_mapper item_of_line line_of_item)
let bed5_raw = B.Bed.Bed5_raw.(line_mapper item_of_line line_of_item)
