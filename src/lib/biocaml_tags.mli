(** Meta-information about files and their formats. *)


type file_format = [
| `gzip of file_format
| `raw_zip of file_format
| `gff of Biocaml_gff.Tags.t
| `wig of Biocaml_wig.Tags.t
| `table of Biocaml_table.Row.Tags.t
| `bam
| `sam
| `bed
| `fastq
| `fasta of Biocaml_fasta.Tags.t
]
(** Description of file formats. *)

type t = [
| file_format
| `list of t list
]
(** Description of multiple file formats. *)

val to_tag: file_format -> t
(** Do a practical coercion. *)

val default_extension: file_format -> string
(** Get a filename extension for a given
    format. E.g. [default_extension (`gzip (`fasta `char))] will be
    ["fasta.gz"]. *)

val default_extensions: t -> string list
(** Do like [default_extension] but for potentially multiple file-formats. *)

val add_extensions: t -> string list ->
  (string list * string list, [> `tags of [> `not_enough_filenames ] ]) Core.Result.t
(** For a given [tags] value and a list of {i file-base-names} add
    extensions to the files, and return also the base-names not touched.
    If [t] involves {i n} file-formats, [add_extensions t fl] will add
    extensions to the first {i n} files of [fl] and return also the
    untouched base-names:
    [(files_with_extensions, remaingin_base_names)].
    If there are not enough base-names, the result is the error
    [`tags `not_enough_filenames]. *)


val guess_from_filename: string ->
  (file_format, [> `extension_absent | `extension_unknown of string ]) Core.Result.t
(** Get a tag as precise as possible for a given filename. *)

val of_string: string -> (t, [> `parse_tags of exn]) Core.Result.t
(** Parse a tag specification (the format is, for now, based on
    S-Expressions, but this will change). *)

val to_string: t -> string
(** Convert a [t]ag to its string specification. *)

(** {2 Transforms From Tags } *)


module Output_transform: sig


  type output_error = [
    | `bam of Biocaml_bam.Error.item_to_raw
    | `sam of Biocaml_sam.Error.item_to_raw
    | `fastq of [ `cannot_convert_ascii_phred_score of string ]
  ]
  (** Union possible output errors of the transforms
      leading to {!Biocaml_sam.item} values.
      (the other output transforms are error-free).  *)

  type tags = t
  (** Alias to [Tags.t] for this module. *)

  type t = [
    | `sam_item_to_file of
        (Biocaml_sam.item, (string, output_error) Core.Result.t)
          Biocaml_transform.t
    | `gff_to_file of(Biocaml_gff.item, string) Biocaml_transform.t
    | `wig_to_file of (Biocaml_wig.item, string) Biocaml_transform.t
    | `bed_to_file of (Biocaml_bed.item, string) Biocaml_transform.t
    | `fastq_to_file of (Biocaml_fastq.item, string) Biocaml_transform.t
    | `fastq_to_two_files of
        (Biocaml_fastq.item, (string * string, output_error) Core.Result.t)
          Biocaml_transform.t
    | `char_fasta_to_file of
        (Biocaml_fasta.char_seq Biocaml_fasta.raw_item, string)
          Biocaml_transform.t
    | `int_fasta_to_file of
        (Biocaml_fasta.int_seq Biocaml_fasta.raw_item, string)
          Biocaml_transform.t
    | `table_to_file of (Biocaml_table.Row.t, string) Biocaml_transform.t
  ]
  (** Generic union of possible output transforms. *)

  val name: t -> string
  (** Get a string naming the transform (for debug/error messages). *)

  val from_tags : ?zip_level:int -> ?zlib_buffer_size:int -> tags ->
    (t, [> `not_implemented of string ]) Biocaml_internal_pervasives.Result.t
  (** Guess the [Output_transform.t] from file tags. *)

  val sexp_of_output_error: output_error -> Sexplib.Sexp.t

end


module Input_transform: sig


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
    | `fasta_lengths_mismatch
  ]
  (** An union of all possible input errors. *)

  type tags = t
  (** An alias of the type [Tags.t]. *)


  type t = [
    | `file_to_sam_item of
        (string, (Biocaml_sam.item, input_error) Core.Result.t) Biocaml_transform.t
    | `file_to_gff of
        (string, (Biocaml_gff.item, input_error) Core.Result.t) Biocaml_transform.t
    | `file_to_wig of
        (string, (Biocaml_wig.item, input_error) Core.Result.t) Biocaml_transform.t
    | `file_to_bed of
        (string, (Biocaml_bed.item, input_error) Core.Result.t) Biocaml_transform.t
    | `file_to_fastq
      of (string, (Biocaml_fastq.item, input_error) Core.Result.t) Biocaml_transform.t
    | `file_to_char_fasta
      of (string, (Biocaml_fasta.char_seq Biocaml_fasta.raw_item,
                   input_error) Core.Result.t) Biocaml_transform.t
    | `file_to_int_fasta of
        (string, (Biocaml_fasta.int_seq Biocaml_fasta.raw_item,
                  input_error) Core.Result.t) Biocaml_transform.t
    | `file_to_table of
        (string, (Biocaml_table.Row.t, input_error) Core.Result.t) Biocaml_transform.t
    | `two_files_to_fastq of
        (string * string,
         (Biocaml_fastq.item, input_error) Core.Result.t) Biocaml_transform.t
  ]
  (** The general input transformation. *)

  val name: t -> string
  (** Get a string describing and input transform (for debug/display
      purposes). *)

  val from_tags :
    ?zlib_buffer_size:int ->
    tags ->
    (t, [> `not_implemented of string ]) Core.Result.t
  (** Create an [Input_transform.t] from [tags] describing the format. *)

  val sexp_of_input_error: input_error -> Sexplib.Sexp.t

end



(** {2 S-Expression Conversions} *)

val file_format_of_sexp: Sexplib.Sexp.t -> file_format
val sexp_of_file_format: file_format -> Sexplib.Sexp.t
include Core.Sexpable.S with type t := t
