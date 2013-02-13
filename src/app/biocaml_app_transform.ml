
open Core.Std
open Flow
open Biocaml_app_common
open Biocaml


type input_error = [
  | `bam of Biocaml_bam.Transform.raw_bam_error
  | `bam_to_item of [ Biocaml_bam.Transform.raw_to_item_error ]
  | `sam of [ Biocaml_sam.Error.string_to_raw ]
  | `sam_to_item of [ Biocaml_sam.Error.raw_to_item ]
  | `unzip of Biocaml_zip.Transform.unzip_error
  | `gff of Gff.parse_error
  | `wig of Wig.parse_error
  | `bed of Bed.parse_error
  | `fastq of Fastq.Error.t
  | `fasta of Fasta.Error.t
  | `table of [ `wrong_format of
                [ `column_number
                | `float_of_string of string
                | `int_of_string of string ] *
                  Biocaml.Table.Row.t_type * string ]

  ]
with sexp_of

type input_transform = [
  | `from_sam_item of (string, (Sam.item, input_error) Result.t) Transform.t
  | `from_gff of(string, (Gff.stream_item, input_error) Result.t) Transform.t
  | `from_wig of (string, (Wig.t, input_error) Result.t) Transform.t
  | `from_bed of (string, (Bed.t, input_error) Result.t) Transform.t
  | `from_fastq of (string, (Fastq.item, input_error) Result.t) Transform.t
  | `from_char_fasta of
    (string, (Fasta.char_seq Fasta.Transform.raw_item, input_error) Result.t) Transform.t
  | `from_int_fasta of
    (string, (Fasta.int_seq Fasta.Transform.raw_item, input_error) Result.t) Transform.t
  | `from_table of
    (string, (Biocaml.Table.Row.t, input_error) Result.t) Transform.t
]
let input_transform_name = function
  | `from_sam_item _ -> "from_sam_item"
  | `from_gff _ -> "from_gff"
  | `from_wig _ -> "from_wig"
  | `from_bed _ -> "from_bed"
  | `from_fastq _ -> "from_fastq"
  | `from_char_fasta _ -> "from_char_fasta"
  | `from_int_fasta _ -> "from_int_fasta"
  | `from_table _ -> "from_table"


let rec input_transform ?with_unzip ~zlib_buffer_size input_tags =
  let with_unzip t =
    match with_unzip with
    | Some z ->
      Transform.compose_results
        ~on_error:(function `left l -> `unzip l | `right r -> r)
        z t
    | None -> t
  in
  let from_sam_item t = return (`from_sam_item (with_unzip t) : input_transform) in
  match input_tags with
  | `raw_zip tags ->
    input_transform
      ~with_unzip:(Zip.Transform.unzip ~zlib_buffer_size ~format:`raw ())
      ~zlib_buffer_size tags
  | `gzip tags ->
    input_transform
      ~with_unzip:(Zip.Transform.unzip ~zlib_buffer_size ~format:`gzip ())
      ~zlib_buffer_size tags
  | `bam ->
    from_sam_item (
      Transform.compose_results
        ~on_error:(function `left l -> l | `right r -> `bam_to_item r)
        (Bam.Transform.string_to_raw ~zlib_buffer_size ())
        (Bam.Transform.raw_to_item ()))
  | `sam ->
    from_sam_item (
      Transform.compose_results
        ~on_error:(function `left l -> `sam l | `right r -> `sam_to_item r)
        (Sam.Transform.string_to_raw ())
        (Sam.Transform.raw_to_item ()))
  | `gff gff_tag_list ->
    let t =
      Transform.on_output
        (Gff.Transform.string_to_item ~tags:gff_tag_list ())
        (function Ok o -> Ok o | Error e -> Error (`gff e))
    in
    return (`from_gff (with_unzip t) : input_transform)
  | `wig wig_tag_list ->
    let t =
      Transform.on_output
        (Wig.Transform.string_to_t ~tags:wig_tag_list ())
        (function Ok o -> Ok o | Error e -> Error (`wig e))
    in
    return (`from_wig (with_unzip t) : input_transform)
  | `bed ->
    let t =
      Transform.on_output
        (Bed.Transform.string_to_t ())
        (function Ok o -> Ok o | Error e -> Error (`bed e))
    in
    return (`from_bed (with_unzip t) : input_transform)
  | `fastq ->
    let t =
      Transform.on_output
        (Fastq.Transform.string_to_item ())
        (function Ok o -> Ok o | Error e -> Error (`fastq e))
    in
    return (`from_fastq (with_unzip t) : input_transform)
  | `fasta `unknown
  | `fasta `char ->
    (* TODO output warning? if `unknown *)
    let t =
      Transform.on_output
        (Fasta.Transform.string_to_char_seq_raw_item ())
        (function Ok o -> Ok o | Error e -> Error (`fasta e))
    in
    return (`from_char_fasta (with_unzip t) : input_transform)
  | `fasta `int ->
    let t =
      Transform.on_output
        (Fasta.Transform.string_to_int_seq_raw_item ())
        (function Ok o -> Ok o | Error e -> Error (`fasta e))
    in
    return (`from_int_fasta (with_unzip t) : input_transform)
  | `table sep ->
    let t =
      Transform.on_output
        ~f:begin fun s ->
          Table.Row.of_line ~separators:[sep] (s : Lines.item :> Line.t)
          |! begin function
          | Ok o -> Ok o
          | Error e -> Error (`table e)
          end
        end
        (Lines.Transform.string_to_item ())
    in
    return (`from_table (with_unzip t) : input_transform)

let filename_chop_all_extensions filename =
  let rec f filename =
    match Filename.split_extension filename with
    | (n, None) -> n
    | (n, Some s) -> f n
  in
  f filename

let filename_make_new base extension =
  sprintf "%s-%s.%s" base Time.(now () |! to_filename_string) extension

let transform_stringify_errors t =
  Transform.on_error t
    ~f:(function
    | `input e -> `string (Sexp.to_string_hum (sexp_of_input_error e))
    | `output e -> `string (Sexp.to_string_hum (sexp_of_output_error e))
    )

let fastq_item_to_sam_item () =
  let queue =
    Queue.of_list [
      `header_line ("1.0", `unsorted, []);
      `reference_sequence_dictionary [| |];
    ] in
  Transform.make () ~name:"fastq_item_to_sam_item"
    ~next:(fun stopped ->
      match Queue.dequeue queue with
      | Some s -> `output s
      | None -> if stopped then `end_of_stream else `not_ready)
    ~feed:(fun { Fastq.name; sequence; comment; qualities } ->
      let quality =
        String.to_array qualities
        |! Array.map ~f:(fun c ->
          Phred_score.(of_ascii c |! Option.value ~default:(of_probability_exn 0.1)))
      in
      Queue.enqueue queue
        (`alignment {
          Sam.
          query_template_name = name;
          flags = Sam.Flags.of_int 0;
          reference_sequence = `none;
          position = None;
          mapping_quality = None;
          cigar_operations = [| |];
          next_reference_sequence = `none;
          next_position = None;
          template_length = None;
          sequence = `string sequence;
          quality;
          optional_content = []; }))

let transforms_to_do
    input_files_tags_and_transforms meta_output_transform output_tags =
  let rec loop acc l =
    match l, (meta_output_transform : output_transform) with
    | [], _ -> return acc
    | (filename, tags, `from_fastq tri) :: t, `to_fastq tro ->
      let m =
        let transfo =
          Transform.(
            compose_result_left
              (on_error tri (fun e -> `input e)) tro)
          |! transform_stringify_errors in
        let out_extension = Tags.default_extension output_tags in
        let base = filename_chop_all_extensions filename in
        `file_to_file (filename, transfo, filename_make_new base out_extension)
      in
      loop (m :: acc) t
    | (filename, tags, `from_sam_item tri) :: t, `to_sam_item tro ->
      let m =
        let transfo =
          Transform.compose_results
            ~on_error:(function `left e -> `input e | `right e -> `output e)
            tri tro
          |! transform_stringify_errors in
        let out_extension = Tags.default_extension output_tags in
        let base = filename_chop_all_extensions filename in
        `file_to_file (filename, transfo, filename_make_new base out_extension)
      in
      loop (m :: acc) t
    | (filename, tags, `from_fastq tri) :: t, `to_sam_item tro ->
      let m =
        let transfo =
          Transform.(compose_results
            ~on_error:(function `left e -> `input e | `right e -> `output e)
            tri (compose (fastq_item_to_sam_item ()) tro))
          |! transform_stringify_errors in
        let out_extension = Tags.default_extension output_tags in
        let base = filename_chop_all_extensions filename in
        `file_to_file (filename, transfo, filename_make_new base out_extension)
      in
      loop (m :: acc) t
    | _ ->
      error (`not_implemented)
  in
  loop [] input_files_tags_and_transforms


let run_transform  ~input_buffer_size ~output_buffer_size ~output_tags files =
  begin
    let zlib_buffer_size = 2 * input_buffer_size in
    while_sequential files (fun file_optionally_tagged ->
      match String.lsplit2 ~on:':' file_optionally_tagged with
      | None ->
        of_result (Tags.guess_from_filename file_optionally_tagged)
        >>= fun tags ->
        input_transform ~zlib_buffer_size tags
        >>= fun meta_transform ->
        return (file_optionally_tagged, tags, meta_transform)
      | Some (one, two) ->
        of_result (Tags.of_string two)
        >>= fun tags ->
        input_transform ~zlib_buffer_size tags
        >>= fun meta_transform ->
        return (one, tags, meta_transform))
    >>= fun input_files_tags_and_transforms ->
    of_result (Tags.of_string output_tags)
    >>= fun tags ->
    output_transform_of_tags ~zlib_buffer_size:(output_buffer_size) tags
    >>= fun meta_output_transform ->
    while_sequential input_files_tags_and_transforms (fun (filename, tags, tr) ->
      dbg "Convert %s (%s) %s %s"
        filename (Tags.to_string tags) (input_transform_name tr)
        (output_transform_name meta_output_transform)
    )
    >>= fun _ ->
    transforms_to_do input_files_tags_and_transforms meta_output_transform tags
    >>= fun transforms ->

    for_concurrent transforms begin function
    | `file_to_file (filein, tr, fileout) ->
      dbg "Starting Transform: %s → %s" filein fileout >>= fun () ->
      IO.Transform.file_to_file (Transform.to_object tr)
        ~input_buffer_size filein ~output_buffer_size fileout
    end
    >>= fun (results, errors) ->
    begin match errors with
    | [] -> return ()
    | some -> error (`errors some)
    end
  end
  >>< begin function
  | Ok () -> return ()
  | Error e ->
    error (
      <:sexp_of< [ `extension_absent
                 | `errors of [ `transform of
                     [ `io_exn of exn
                     | `stopped_before_end_of_stream
                     | `transform_error of [ `string of string ] ] ] list
                 | `not_implemented
                 | `extension_unknown of string
                 | `parse_tags of exn ] >> e
      |! Sexp.to_string_hum)
  end


let command =
  let open Command_line in
  let spec =
    let open Spec in
    file_to_file_flags ()
    ++ step (fun k v -> k ~output_tags:v)
    +> flag "output-tags" ~aliases:["to"] (required string)
      ~doc:"<string> give the specification of the output"
    +> anon (sequence ("INPUT-FILE:tag-definition" %: string))
    ++ uses_lwt ()
  in
  basic ~summary:"Transform files" spec run_transform
