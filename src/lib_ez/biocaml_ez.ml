open Core.Std
open Future_unix.Std
open Future

module Fasta = struct
  open Biocaml_fasta

  let sequence_to_int_list s =
    ok_exn (sequence_to_int_list s)

  let read0
      ?start
      ?allow_sharp_comments
      ?allow_semicolon_comments
      ?allow_empty_lines
      ?max_line_length
      ?alphabet
      r
      =
    read0
      ?start
      ?allow_sharp_comments
      ?allow_semicolon_comments
      ?allow_empty_lines
      ?max_line_length
      ?alphabet
      r
    |> Pipe.map ~f:ok_exn

  let read ?start ?fmt r =
    let header,strm = ok_exn (read ?start ?fmt r) in
    header, Pipe.map strm ~f:ok_exn

  let with_file ?fmt file ~f =
    with_file ?fmt file ~f:(fun header strm ->
      Ok (f header (Pipe.map strm ~f:ok_exn))
    )
    |> ok_exn

end

module Fastq = struct
  open Biocaml_fastq

  type nonrec item = item
  let split_name = split_name

  let read ic =
    read ic
    |> Pipe.map ~f:ok_exn

  let write = write
  let write_file = write_file

  module Illumina = struct
    open Illumina
    type nonrec surface = surface
    type nonrec tile = tile
    let tile_of_string x = ok_exn (tile_of_string x)
    let tile_to_string = tile_to_string
    type nonrec sequence_id = sequence_id
    let sequence_id_of_string x = ok_exn (sequence_id_of_string x)
  end

  let item_to_string = item_to_string

  let name_of_line ?pos x = ok_exn (name_of_line ?pos x)
  let sequence_of_line = sequence_of_line
  let comment_of_line ?pos x = ok_exn (comment_of_line ?pos x)
  let qualities_of_line ?pos ?sequence x =
    ok_exn (qualities_of_line ?pos ?sequence x)

end

module Line = struct
  include Biocaml_line
end

module Lines = struct
  include Biocaml_lines
end

module Phred_score = struct
  include Biocaml_phred_score

  let of_char ?offset x = ok_exn (of_char ?offset x)
  let to_char ?offset t = ok_exn (to_char ?offset t)
  let of_int x = ok_exn (of_int x)
  let of_probability ?f x = ok_exn (of_probability ?f x)

end

module Range = struct
  include Biocaml_range

  let make lo hi = ok_exn (make lo hi)

end

module Roman_num = struct
  include Biocaml_roman_num

  let of_roman x = ok_exn (of_roman x)
  let of_arabic x = ok_exn (of_arabic x)

end

module Sam = struct
  include Biocaml_sam

  let read ?start ic =
    (read ?start ic >>| ok_exn) >>= fun (hdr, pipe_r) ->
    hdr, Pipe.map pipe_r ~f:ok_exn

end

module Seq_range = struct
  include Biocaml_seq_range
  let make seq lo hi = ok_exn (make seq lo hi)
end

module Strand = struct
  include Biocaml_strand

  let of_string s = ok_exn (of_string s)

end
