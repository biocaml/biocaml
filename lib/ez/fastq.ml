open Core_kernel.Std
open CFStream
open Biocaml_unix.Fastq

type nonrec item = item

let split_name = split_name

let read ic =
  read ic
  |> Stream.map ~f:ok_exn

let write = write
let write_file = write_file

module Illumina = struct
  include Illumina
  let tile_of_string x = ok_exn (tile_of_string x)
  let sequence_id_of_string x = ok_exn (sequence_id_of_string x)
end

let item_to_string = item_to_string

let name_of_line ?pos x = ok_exn (name_of_line ?pos x)

let sequence_of_line = sequence_of_line

let comment_of_line ?pos x = ok_exn (comment_of_line ?pos x)

let qualities_of_line ?pos ?sequence x =
  ok_exn (qualities_of_line ?pos ?sequence x)
