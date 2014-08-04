open Core.Std
open CFStream
open Or_error

module Bgzf = Biocaml_bgzf
module Sam = Biocaml_sam

type header = Biocaml_sam.header

type alignment = string

exception Parse_error of string

let wrap f x =
  try f x
  with
  | Bgzf.Parse_error err -> error_string err
  | End_of_file -> error_string "Bam: premature end of file reading the header"

let input_int32_as_int iz =
  Int32.to_int_exn (Bgzf.input_int32 iz)

let read_header = wrap (fun iz ->
    let magic = Bgzf.input_string iz 4 in
    if magic <> "BAM\001" then raise (Parse_error "Incorrect magic string, not a BAM file") ;
    let l_text = input_int32_as_int iz in
    let text = Bgzf.input_string iz l_text in
    Biocaml_sam.parse_header text
  )

let read_one_reference_information iz =
  let parse iz =
    let l_name = input_int32_as_int iz in
    let name = Bgzf.input_string iz (l_name - 1) in
    let _ = Bgzf.input_char iz in (* name is a NULL terminated string *)
    let length = input_int32_as_int iz in
    return (name, length)
  in
  wrap parse iz >>= fun (name, length) ->
  Sam.ref_seq ~name ~length ()

let read_reference_information = wrap (fun iz ->
    let rec loop accu n =
      if n = 0 then Ok (List.rev accu)
      else
        match read_one_reference_information iz with
        | Ok refseq -> loop (refseq :: accu) (n - 1)
        | Error _ as e -> e
    in
    let n_ref = input_int32_as_int iz in
    loop [] n_ref
  )

let read_alignment_stream iz =
  Stream.from (fun _ ->
      try
        let block_size = input_int32_as_int iz in
        let block = Bgzf.input_string iz block_size in
        Some (return block)
      with
      | Bgzf.Parse_error err -> print_endline err ; Some (error_string err)
      | End_of_file -> None
    )


let read ic =
  let iz = Bgzf.of_in_channel ic in
  read_header iz >>= fun header ->
  read_reference_information iz >>= fun refinfo ->
  return (header, read_alignment_stream iz)


let with_file fn ~f =
  In_channel.with_file ~binary:true fn ~f:(fun ic ->
      read ic >>= fun (header, alignments) ->
      return (f header alignments)
    )

let write header alignments oc = assert false
