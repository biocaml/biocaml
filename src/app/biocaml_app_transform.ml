
open Core.Std
open Flow
open Biocaml_app_common
open Biocaml


type input_error = [
  | `bam of Biocaml_bam.Error.raw_bam
  | `bam_to_item of [ Biocaml_bam.Error.raw_to_item ]
  | `sam of [ Biocaml_sam.Error.string_to_raw ]
  | `sam_to_item of [ Biocaml_sam.Error.raw_to_item ]
  | `unzip of Biocaml_zip.Error.unzip
  | `gff of Gff.Error.parsing
  | `wig of Wig.Error.parsing
  | `bed of Bed.Error.parsing
  | `fastq of Fastq.Error.t
  | `fasta of Fasta.Error.t
  | `table_row of [ `wrong_format of
                      [ `column_number
                      | `float_of_string of string
                      | `int_of_string of string ] *
                        Biocaml.Table.Row.t_type * string ]

  ]
with sexp_of

type input_transform = [
  | `from_sam_item of (string, (Sam.item, input_error) Result.t) Transform.t
  | `from_gff of(string, (Gff.item, input_error) Result.t) Transform.t
  | `from_wig of (string, (Wig.item, input_error) Result.t) Transform.t
  | `from_bed of (string, (Bed.item, input_error) Result.t) Transform.t
  | `from_fastq of (string, (Fastq.item, input_error) Result.t) Transform.t
  | `from_char_fasta of
    (string, (Fasta.char_seq Fasta.raw_item, input_error) Result.t) Transform.t
  | `from_int_fasta of
    (string, (Fasta.int_seq Fasta.raw_item, input_error) Result.t) Transform.t
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


let rec input_transform ?with_unzip input_tags =
  let zlib_buffer_size = Global_configuration.zlib_buffer_size () in
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
      ~with_unzip:(Zip.Transform.unzip ~zlib_buffer_size ~format:`raw ()) tags
  | `gzip tags ->
    input_transform
      ~with_unzip:(Zip.Transform.unzip ~zlib_buffer_size ~format:`gzip ()) tags
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
        (Wig.Transform.string_to_item ~tags:wig_tag_list ())
        (function Ok o -> Ok o | Error e -> Error (`wig e))
    in
    return (`from_wig (with_unzip t) : input_transform)
  | `bed ->
    let t =
      Transform.on_output
        (Bed.Transform.string_to_item ())
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
  | `fasta (`char_sequence  tags) ->
    let t =
      Transform.on_output
        (Fasta.Transform.string_to_char_seq_raw_item ~tags ())
        (function Ok o -> Ok o | Error e -> Error (`fasta e))
    in
    return (`from_char_fasta (with_unzip t) : input_transform)
  | `fasta (`int_sequence tags) ->
    let t =
      Transform.on_output
        (Fasta.Transform.string_to_int_seq_raw_item ~tags ())
        (function Ok o -> Ok o | Error e -> Error (`fasta e))
    in
    return (`from_int_fasta (with_unzip t) : input_transform)
  | `table tags ->
    let t =
      Transform.compose
        (Lines.Transform.string_to_item ())
        (Table.Row.Transform.line_to_item ~tags ())
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

let sam_item_to_fastq_item () : (Sam.item, Fastq.item) Transform.t =
  let queue = Queue.create () in
  Transform.make () ~name:"sam_item_to_fastq_item"
    ~next:(fun stopped ->
      match Queue.dequeue queue with
      | Some s -> `output s
      | None -> if stopped then `end_of_stream else `not_ready)
    ~feed:begin function
    | `header_line _
    | `header _
    | `comment _
    | `reference_sequence_dictionary _ -> ()
    | `alignment {Sam. query_template_name; sequence = `string seq; quality; _} ->
      let qualities =
        Array.map quality ~f:(fun c ->
          Phred_score.(to_ascii c |> Option.value ~default:'{' |> Char.to_string))
        |> String.concat_array ~sep:"" in
      Queue.enqueue queue
        { Fastq.name = query_template_name; sequence = seq;
          comment = ""; qualities }
    | `alignment {Sam. query_template_name; sequence ; quality; _} ->
      ()
    end

let transforms_to_do
    input_files_tags_and_transforms meta_output_transform output_tags =
  let any_fasta_transform
      filename itags tr_in raw_to_item otags item_to_raw tr_out =
    let transfo =
      Transform.(
        on_error ~f:(fun e -> `input e)
          (compose_result_left
             (compose_results
                ~on_error:(function `left e ->  e | `right e -> e)
                    tr_in (on_error ~f:(fun e -> `fasta e) raw_to_item))
             (compose item_to_raw tr_out))
      ) |> transform_stringify_errors in
    let out_extension = Tags.default_extension output_tags in
    let base = filename_chop_all_extensions filename in
    `file_to_file (filename, transfo, filename_make_new base out_extension)
  in
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
    | (filename, tags, `from_sam_item tri) :: t, `to_fastq tro ->
      let m =
        let transfo =
          Transform.(
            on_error ~f:(fun e -> `input e)
              (compose_result_left
                 tri
                 (compose (sam_item_to_fastq_item ()) tro))
          ) |> transform_stringify_errors in
        let out_extension = Tags.default_extension output_tags in
        let base = filename_chop_all_extensions filename in
        `file_to_file (filename, transfo, filename_make_new base out_extension)
      in
      loop (m :: acc) t
    | (filename, itags, `from_char_fasta tri) :: t, `to_char_fasta (tro, otags) ->
      let m =
        any_fasta_transform
          filename
          itags tri (Fasta.Transform.char_seq_raw_item_to_item  ())
          otags (Fasta.Transform.char_seq_item_to_raw_item ~tags:otags ()) tro
      in
      loop (m :: acc) t
    | (filename, itags, `from_int_fasta tri) :: t, `to_int_fasta (tro, otags) ->
      let m =
        any_fasta_transform
          filename
          itags tri (Fasta.Transform.int_seq_raw_item_to_item  ())
          otags (Fasta.Transform.int_seq_item_to_raw_item ~tags:otags ()) tro
      in
      loop (m :: acc) t
    | (   a_filename, a_itags, `from_char_fasta a_tri)
      :: (b_filename, b_itags, `from_int_fasta  b_tri) :: t,
      `to_fastq tro ->
      let a_transfo =
        Transform.(
          on_error ~f:(fun e -> `input e)
            (compose_results
               ~on_error:(function `left e ->  e | `right e -> e)
               a_tri (on_error ~f:(fun e -> `fasta e)
                        (Fasta.Transform.char_seq_raw_item_to_item  ()))))
      in
      let b_transfo =
        Transform.(
          on_error ~f:(fun e -> `input e)
            (compose_results
               ~on_error:(function `left e ->  e | `right e -> e)
               b_tri (on_error ~f:(fun e -> `fasta e)
                        (Fasta.Transform.int_seq_raw_item_to_item  ()))))
      in
      let two_fastas_to_fastq = Fastq.Transform.fasta_pair_to_fastq () in
      let out_extension = Tags.default_extension output_tags in
      let base = filename_chop_all_extensions a_filename in
      let m =
      `two_files_to_file (a_filename, a_transfo,
                          b_filename, b_transfo,
                          filename_make_new base out_extension,
                          Transform.compose_result_left
                            two_fastas_to_fastq tro) in
      loop (m :: acc) t

    | _ ->
      error (`not_implemented "transform")
  in
  loop [] input_files_tags_and_transforms


let run_transform ~output_tags files =
  begin
    while_sequential files (fun file_optionally_tagged ->
      match String.lsplit2 ~on:':' file_optionally_tagged with
      | None ->
        of_result (Tags.guess_from_filename file_optionally_tagged)
        >>= fun tags ->
        input_transform tags
        >>= fun meta_transform ->
        return (file_optionally_tagged, tags, meta_transform)
      | Some (one, two) ->
        of_result (Tags.of_string two)
        >>= fun tags ->
        input_transform tags
        >>= fun meta_transform ->
        return (one, tags, meta_transform))
    >>= fun input_files_tags_and_transforms ->
    of_result (Tags.of_string output_tags)
    >>= fun tags ->
    output_transform_of_tags tags
    >>= fun meta_output_transform ->
    while_sequential input_files_tags_and_transforms (fun (filename, tags, tr) ->
      Say.dbg "Convert %s (%s) %s %s"
        filename (Tags.to_string tags) (input_transform_name tr)
        (output_transform_name meta_output_transform)
    )
    >>= fun _ ->
    transforms_to_do input_files_tags_and_transforms meta_output_transform tags
    >>= fun transforms ->

    for_concurrent transforms begin function
    | `file_to_file (filein, tr, fileout) ->
      Say.dbg "Starting Transform: %s → %s" filein fileout >>= fun () ->
      IO.Transform.file_to_file (Transform.to_object tr) filein  fileout
    | `two_files_to_file (a_filename, a_transfo,
                          b_filename, b_transfo,
                          out_file, out_transfo) ->
      two_files_to_file
        ~left:(a_filename, a_transfo) ~right:(b_filename, b_transfo)
        (out_file, out_transfo)
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
      <:sexp_of<
       [> `errors of
              [> `cannot_convert_to_phred_score of int Core.Std.List.t
               | `lwt_exn of exn
               | `sequence_names_mismatch of string * string
               | `input of input_error
               | `fastas_of_incompatible_length of
                   Core.Std.String.t * Core.Std.String.t
               | `transform of
                   [> `io_exn of exn
                    | `stopped_before_end_of_stream
                    | `transform_error of [> `string of string ] ] ]
              list
          | `extension_absent
          | `extension_unknown of string
          | `not_implemented of string
          | `parse_tags of Core.Exn.t ]
      >> e
      |! Sexp.to_string_hum)
  end


(** Return a string containing a manual in markdown-ish syntax. *)
let more_help original_help : string =
  Text.with_buffer begin fun buf ->
    let open Text.Markdown in
    title buf "Biocaml's File Conversion Tool";
    section buf "Usage";
    par buf "This command is used to convert a set of input files to \
        another format.";
    code buf "biocaml transform <input_1> <input_2> ... -to <output-format>";
    par buf "If `input_n` is simply a filename, Biocaml will try to
        guess how to parse it (for now using the file extension). But
        if more flexibility is needed one can describe the file-format using
        the `Biocaml.Tags.t` format after a colon character,
        e.g. `my_file:(gzip fastq)`.";
    par buf "The output format also uses the `Biocaml.Tags.t` format
        (c.f. [Biocaml_tags]).";
    par buf "The transformations are based on the idea of parsing
        *semantically similar* formats to a common representation and
        printing that representation to compatible formats. The
        *FASTQ*, *FASTA*, and *SAM-Item* representations are partially
        implemented so far. Here are the *half-transformations*
        available:";
    ul buf [
      "Parse `(gzip ANY)` -> *ANY*";
      "Print *ANY* -> `(gzip ANY)`";
      "Parse `fastq` → *FASTQ*";
      "Print *FASTQ* -> `fastq`";
      "Print *FASTQ* -> `sam` (so-called non-aligned SAM/BAM files)";
      "Print *FASTQ* -> `bam`";
      "Parse `sam` → *SAM-Item*";
      "Parse `bam` → *SAM-Item*";
      "Print *SAM-Item* -> `sam`";
      "Print *SAM-Item* -> `bam`";
      "Print *SAM-Item* -> `fastq` (gets the name, sequences and qualities \
                                from alignments and discards any other info)";
      "Parse/Print *{int, char} FASTA* <-> `{int, char} fasta` \
       (can do some clean-up of FASTA files, more to come …)";
      "Parse (char fasta × int fasta) -> *FASTQ* (take the sequences \
       from the first file and the qualities from the second and merge \
       them into a FASTQ record)";
    ];
    par buf "Note that other meaningless file-formats are *not forbidden* like
       `(gzip (gzip fastq))` or `(gzip bam)`.";
    par buf "`biocaml transform` outputs files named like their corresponding
        inputs but tagged with a time-stamp and the correct file-extension.";
    lines buf [
      "[Biocaml_tags]: http://biocaml.org/doc/dev/api/Biocaml_tags.html";
    ];
    section buf "Examples";
    par buf "A simple SAM to BAM conversion:";
    code buf "biocaml transform  my_file.sam -to bam";
    par buf "A Fastq file with a non-recognizable name converted to a
        BAM file with maximal compression to gain a few bytes:";
    code buf "biocaml transform my_file:fastq -gzip-level 9 -to bam";
    let command = "transform" in
    command_line_section buf ~command ~original_help;
    about_the_manual_section buf ~command;
  end

let command =
  let open Command_line in
  let spec =
    let open Spec in
    file_to_file_flags ()
    ++ gzip_output_flags ~activation:false
    ++ step (fun k v -> k ~output_tags:v)
    +> flag "output-tags" ~aliases:["to"] (optional string)
        ~doc:"<string> give the specification of the output"
    +> anon (sequence ("INPUT-FILE:tag-definition" %: string))
    ++ display_manual_flag ()
    +> help
    ++ uses_lwt ()
  in
  basic ~summary:"Transform files" spec
    (fun ~output_tags tagged_files ~manual help ->
      if manual then
        Say.raw "%s%!" (more_help (Lazy.force help))
        >>< fun _ -> return ()
      else
        begin match output_tags with
        | Some output_tags ->
          run_transform ~output_tags tagged_files
        | None ->
          Say.problem "Expecting either -help, -manual, or -output-tags"
          >>< fun _ -> return ()
        end)
