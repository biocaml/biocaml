open Biocaml_internal_pervasives
open Result
module Gff = Biocaml_gff
module Wig = Biocaml_wig

type file_format = [
| `gzip of file_format
| `raw_zip of file_format
| `gff of Gff.Tags.t
| `wig of Wig.Tags.t
| `table of Biocaml_table.Row.Tags.t
| `bam
| `sam
| `bed
| `fastq
| `fasta of Biocaml_fasta.Tags.t
] with sexp

type t = [
| file_format
| `list of t list
] with sexp

let rec default_extension: file_format -> string = function
  | `gzip t -> sprintf "%s.gz" (default_extension t)
  | `raw_zip t -> sprintf "%s.rawzip" (default_extension t)
  | `gff _ -> "gff"
  | `wig _ -> "wig"
  | `bam     -> "bam"
  | `sam     -> "sam"
  | `bed     -> "bed"
  | `fastq   -> "fastq"
  | `fasta _ -> "fasta"
  | `table tags -> Biocaml_table.Row.Tags.default_extension tags

let to_tag (f : file_format) = (f :> t)

let rec guess_from_filename filename =
  match Filename.split_extension filename with
  | (f, Some "gz") ->
    guess_from_filename f
    >>= fun t ->
    return (`gzip t)
  | (_, Some term) ->
    begin match term with
    | "gff" -> return (`gff Gff.Tags.default)
    | "wig" -> return (`wig Wig.Tags.default)
    | "bam" -> return `bam
    | "sam" -> return `sam
    | "bed" -> return `bed
    | "fastq" -> return `fastq
    | "fasta" -> return (`fasta Biocaml_fasta.Tags.default)
    | "tsv" -> return (`table [`separator '\t'])
    | "csv" -> return (`table [`separator ','])
    | u -> fail (`extension_unknown u)
    end
  | (_, None) -> fail (`extension_absent)


let of_string s =
  let open Sexplib in
  try return (t_of_sexp (Sexp.of_string s))
  with e -> fail (`parse_tags e)

let to_string t =
  let open Sexplib in
  sexp_of_t t |! Sexp.to_string_hum

module Output_transform = struct

  type sam_output_error = [
    | `bam of Biocaml_bam.Error.item_to_raw
    | `sam of Biocaml_sam.Error.item_to_raw
  ] with sexp_of

  type tags = t

  (** Generic union of possible output transforms. *)
  type t = [
    | `sam_item_to_file of
        (Biocaml_sam.item, (string, sam_output_error) Result.t)
          Biocaml_transform.t
    | `gff_to_file of(Biocaml_gff.item, string) Biocaml_transform.t
    | `wig_to_file of (Biocaml_wig.item, string) Biocaml_transform.t
    | `bed_to_file of (Biocaml_bed.item, string) Biocaml_transform.t
    | `fastq_to_file of (Biocaml_fastq.item, string) Biocaml_transform.t
    | `char_fasta_to_file of
        (Biocaml_fasta.char_seq Biocaml_fasta.raw_item, string)
          Biocaml_transform.t
    | `int_fasta_to_file of
        (Biocaml_fasta.int_seq Biocaml_fasta.raw_item, string)
          Biocaml_transform.t
    | `table_to_file of (Biocaml_table.Row.t, string) Biocaml_transform.t
  ]

  let name = function
  | `sam_item_to_file _ -> "sam_item_to_file"
  | `gff_to_file _ -> "gff_to_file"
  | `wig_to_file _ -> "wig_to_file"
  | `bed_to_file _ -> "bed_to_file"
  | `fastq_to_file _ -> "fastq_to_file"
  | `char_fasta_to_file _ -> "char_fasta_to_file"
  | `int_fasta_to_file _ -> "int_fasta_to_file"
  | `table_to_file _ -> "table_to_file"

  let from_tags ?zip_level ?zlib_buffer_size
      (output_tags: tags) : (t, _) Result.t =
    let rec output_transform ?with_zip (output_tags : file_format) =
      let with_zip_result t =
        match with_zip with
        | Some z -> Biocaml_transform.compose_result_left t z
        | None -> t
      in
      let with_zip_no_error t =
        match with_zip with
        | Some z -> Biocaml_transform.compose t z
        | None -> t
      in
      let sam_item_to_file t = return (`sam_item_to_file (with_zip_result t) : t) in
      match output_tags with
      | `raw_zip (tags: file_format) ->
        output_transform
          ~with_zip:(Biocaml_zip.Transform.zip ?zlib_buffer_size
                       ~format:`raw ?level:zip_level ()) tags
      | `gzip (tags: file_format) ->
        output_transform
          ~with_zip:(Biocaml_zip.Transform.zip ?zlib_buffer_size
                       ~format:`gzip ?level:zip_level ()) tags
      | `bam ->
        sam_item_to_file (
          Biocaml_transform.compose_result_left
            (Biocaml_transform.on_output
               (Biocaml_bam.Transform.item_to_raw ())
               (function Ok o -> Ok o | Error e -> Error (`bam e)))
            (Biocaml_bam.Transform.raw_to_string ?gzip_level:zip_level
               ?zlib_buffer_size ()))
      | `sam ->
        sam_item_to_file (
          Biocaml_transform.compose_result_left
            (Biocaml_transform.on_output
               (Biocaml_sam.Transform.item_to_raw ())
               (function Ok o -> Ok o | Error e -> Error (`sam e)))
            (Biocaml_sam.Transform.raw_to_string ()))
      | `gff tag_list ->
        let t = Biocaml_gff.Transform.item_to_string ~tags:tag_list () in
        return (`gff_to_file (with_zip_no_error t) : t)
      | `wig tag_list ->
        let t = Biocaml_wig.Transform.item_to_string  ~tags:tag_list () in
        return (`wig_to_file (with_zip_no_error t) : t)
      | `bed ->
        let t = Biocaml_bed.Transform.item_to_string  () in
        return (`bed_to_file (with_zip_no_error t) : t)
      | `fastq ->
        let t = Biocaml_fastq.Transform.item_to_string () in
        return (`fastq_to_file (with_zip_no_error t) : t)
      | `fasta (`char_sequence _ as tags) ->
        (* TODO output warning? if `unknown *)
        let t = Biocaml_fasta.Transform.char_seq_raw_item_to_string ~tags () in
        return (`char_fasta_to_file (with_zip_no_error t) : t)
      | `fasta (`int_sequence _ as tags) ->
        let t = Biocaml_fasta.Transform.int_seq_raw_item_to_string ~tags () in
        return (`int_fasta_to_file (with_zip_no_error t) : t)
      | `table tags ->
        let t =
          Biocaml_transform.compose
            (Biocaml_table.Row.Transform.item_to_line ~tags ())
            (Biocaml_lines.Transform.item_to_string ()) in
        return (`table_to_file (with_zip_no_error t) : t)
    in
    match output_tags with
    | `list (tags : tags list) -> fail (`not_implemented "list output_tags")
    | #file_format as file_output_tags -> output_transform file_output_tags


end

module Input_transform = struct


  type input_error = [
    | `bam of Biocaml_bam.Error.raw_bam
    | `bam_to_item of [ Biocaml_bam.Error.raw_to_item ]
    | `sam of [ Biocaml_sam.Error.string_to_raw ]
    | `sam_to_item of [ Biocaml_sam.Error.raw_to_item ]
    | `unzip of Biocaml_zip.Error.unzip
    | `gff of Biocaml_gff.Error.parsing
    | `wig of Biocaml_wig.Error.parsing
    | `bed of Biocaml_bed.Error.parsing
    | `fastq of Biocaml_fastq.Error.t
    | `fasta of Biocaml_fasta.Error.t
    | `table_row of Biocaml_table.Row.Error.t
  ]
  with sexp_of

  type tags = t

  type t = [
    | `from_sam_item of
        (string, (Biocaml_sam.item, input_error) Result.t) Biocaml_transform.t
    | `from_gff of
        (string, (Biocaml_gff.item, input_error) Result.t) Biocaml_transform.t
    | `from_wig of
        (string, (Biocaml_wig.item, input_error) Result.t) Biocaml_transform.t
    | `from_bed of
        (string, (Biocaml_bed.item, input_error) Result.t) Biocaml_transform.t
    | `from_fastq
      of (string, (Biocaml_fastq.item, input_error) Result.t) Biocaml_transform.t
    | `from_char_fasta
      of (string, (Biocaml_fasta.char_seq Biocaml_fasta.raw_item,
                   input_error) Result.t) Biocaml_transform.t
    | `from_int_fasta of
        (string, (Biocaml_fasta.int_seq Biocaml_fasta.raw_item,
                  input_error) Result.t) Biocaml_transform.t
    | `from_table of
        (string, (Biocaml_table.Row.t, input_error) Result.t) Biocaml_transform.t
  ]

  let name = function
  | `from_sam_item _ -> "from_sam_item"
  | `from_gff _ -> "from_gff"
  | `from_wig _ -> "from_wig"
  | `from_bed _ -> "from_bed"
  | `from_fastq _ -> "from_fastq"
  | `from_char_fasta _ -> "from_char_fasta"
  | `from_int_fasta _ -> "from_int_fasta"
  | `from_table _ -> "from_table"


  let from_tags ?zlib_buffer_size (input_tags: tags) =
    let rec input_transform ?with_unzip input_tags =
      let with_unzip t =
        match with_unzip with
        | Some z ->
          Biocaml_transform.compose_results
            ~on_error:(function `left l -> `unzip l | `right r -> r)
            z t
        | None -> t
      in
      let from_sam_item t = return (`from_sam_item (with_unzip t) : t) in
      match (input_tags : file_format) with
      | `raw_zip tags ->
        input_transform
          ~with_unzip:(Biocaml_zip.Transform.unzip
                         ?zlib_buffer_size ~format:`raw ()) tags
      | `gzip tags ->
        input_transform
          ~with_unzip:(Biocaml_zip.Transform.unzip
                         ?zlib_buffer_size ~format:`gzip ()) tags
      | `bam ->
        from_sam_item (
          Biocaml_transform.compose_results
            ~on_error:(function `left l -> l | `right r -> `bam_to_item r)
            (Biocaml_bam.Transform.string_to_raw ?zlib_buffer_size ())
            (Biocaml_bam.Transform.raw_to_item ()))
      | `sam ->
        from_sam_item (
          Biocaml_transform.compose_results
            ~on_error:(function `left l -> `sam l | `right r -> `sam_to_item r)
            (Biocaml_sam.Transform.string_to_raw ())
            (Biocaml_sam.Transform.raw_to_item ()))
      | `gff gff_tag_list ->
        let t =
          Biocaml_transform.on_output
            (Biocaml_gff.Transform.string_to_item ~tags:gff_tag_list ())
            (function Ok o -> Ok o | Error e -> Error (`gff e))
        in
        return (`from_gff (with_unzip t) : t)
      | `wig wig_tag_list ->
        let t =
          Biocaml_transform.on_output
            (Biocaml_wig.Transform.string_to_item ~tags:wig_tag_list ())
            (function Ok o -> Ok o | Error e -> Error (`wig e))
        in
        return (`from_wig (with_unzip t) : t)
      | `bed ->
        let t =
          Biocaml_transform.on_output
            (Biocaml_bed.Transform.string_to_item ())
            (function Ok o -> Ok o | Error e -> Error (`bed e))
        in
        return (`from_bed (with_unzip t) : t)
      | `fastq ->
        let t =
          Biocaml_transform.on_output
            (Biocaml_fastq.Transform.string_to_item ())
            (function Ok o -> Ok o | Error e -> Error (`fastq e))
        in
        return (`from_fastq (with_unzip t) : t)
      | `fasta (`char_sequence  tags) ->
        let t =
          Biocaml_transform.on_output
            (Biocaml_fasta.Transform.string_to_char_seq_raw_item ~tags ())
            (function Ok o -> Ok o | Error e -> Error (`fasta e))
        in
        return (`from_char_fasta (with_unzip t) : t)
      | `fasta (`int_sequence tags) ->
        let t =
          Biocaml_transform.on_output
            (Biocaml_fasta.Transform.string_to_int_seq_raw_item ~tags ())
            (function Ok o -> Ok o | Error e -> Error (`fasta e))
        in
        return (`from_int_fasta (with_unzip t) : t)
      | `table tags ->
        let t =
          Biocaml_transform.compose
            (Biocaml_lines.Transform.string_to_item ())
            (Biocaml_table.Row.Transform.line_to_item ~tags ())
        in
        return (`from_table (with_unzip t) : t)
    in
    match input_tags with
    | `list (tags : tags list) ->
      fail (`not_implemented "list input_tags")
    | #file_format as file_input_tags ->
      input_transform file_input_tags



end
