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
    | `to_sam_item of
        (Biocaml_sam.item, (string, sam_output_error) Result.t)
          Biocaml_transform.t
    | `to_gff of(Biocaml_gff.item, string) Biocaml_transform.t
    | `to_wig of (Biocaml_wig.item, string) Biocaml_transform.t
    | `to_bed of (Biocaml_bed.item, string) Biocaml_transform.t
    | `to_fastq of (Biocaml_fastq.item, string) Biocaml_transform.t
    | `to_char_fasta of
        (Biocaml_fasta.char_seq Biocaml_fasta.raw_item, string)
          Biocaml_transform.t
    | `to_int_fasta of
        (Biocaml_fasta.int_seq Biocaml_fasta.raw_item, string)
          Biocaml_transform.t
    | `to_table of (Biocaml_table.Row.t, string) Biocaml_transform.t
  ]

  let name = function
  | `to_sam_item _ -> "to_sam_item"
  | `to_gff _ -> "to_gff"
  | `to_wig _ -> "to_wig"
  | `to_bed _ -> "to_bed"
  | `to_fastq _ -> "to_fastq"
  | `to_char_fasta _ -> "to_char_fasta"
  | `to_int_fasta _ -> "to_int_fasta"
  | `to_table _ -> "to_table"

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
      let to_sam_item t = return (`to_sam_item (with_zip_result t) : t) in
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
        to_sam_item (
          Biocaml_transform.compose_result_left
            (Biocaml_transform.on_output
               (Biocaml_bam.Transform.item_to_raw ())
               (function Ok o -> Ok o | Error e -> Error (`bam e)))
            (Biocaml_bam.Transform.raw_to_string ?gzip_level:zip_level
               ?zlib_buffer_size ()))
      | `sam ->
        to_sam_item (
          Biocaml_transform.compose_result_left
            (Biocaml_transform.on_output
               (Biocaml_sam.Transform.item_to_raw ())
               (function Ok o -> Ok o | Error e -> Error (`sam e)))
            (Biocaml_sam.Transform.raw_to_string ()))
      | `gff tag_list ->
        let t = Biocaml_gff.Transform.item_to_string ~tags:tag_list () in
        return (`to_gff (with_zip_no_error t) : t)
      | `wig tag_list ->
        let t = Biocaml_wig.Transform.item_to_string  ~tags:tag_list () in
        return (`to_wig (with_zip_no_error t) : t)
      | `bed ->
        let t = Biocaml_bed.Transform.item_to_string  () in
        return (`to_bed (with_zip_no_error t) : t)
      | `fastq ->
        let t = Biocaml_fastq.Transform.item_to_string () in
        return (`to_fastq (with_zip_no_error t) : t)
      | `fasta (`char_sequence _ as tags) ->
        (* TODO output warning? if `unknown *)
        let t = Biocaml_fasta.Transform.char_seq_raw_item_to_string ~tags () in
        return (`to_char_fasta (with_zip_no_error t) : t)
      | `fasta (`int_sequence _ as tags) ->
        let t = Biocaml_fasta.Transform.int_seq_raw_item_to_string ~tags () in
        return (`to_int_fasta (with_zip_no_error t) : t)
      | `table tags ->
        let t =
          Biocaml_transform.compose
            (Biocaml_table.Row.Transform.item_to_line ~tags ())
            (Biocaml_lines.Transform.item_to_string ()) in
        return (`to_table (with_zip_no_error t) : t)
    in
    match output_tags with
    | `list (tags : tags list) -> fail (`not_implemented "list output_tags")
    | #file_format as file_output_tags -> output_transform file_output_tags


end
