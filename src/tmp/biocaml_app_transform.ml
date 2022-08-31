open Core.Std
open Flow
open Biocaml_app_common
open Biocaml
module Sam = Biocaml_sam_deprecated

let filename_chop_all_extensions filename =
  let rec f filename =
    match Filename.split_extension filename with
    | n, None -> n
    | n, Some s -> f n
  in
  f filename
;;

let filename_make_new base extension =
  sprintf "%s-%s.%s" base Time.(now () |> to_filename_string) extension
;;

let transform_stringify_errors t =
  Transform.on_error t ~f:(function
    | `input e ->
      `string (Sexp.to_string_hum (Tags.Input_transform.sexp_of_input_error e))
    | `output e ->
      `string (Sexp.to_string_hum (Tags.Output_transform.sexp_of_output_error e)))
;;

let fastq_item_to_sam_item () =
  let queue =
    Queue.of_list
      [ `header_line ("1.0", `unsorted, []); `reference_sequence_dictionary [||] ]
  in
  Transform.make
    ()
    ~name:"fastq_item_to_sam_item"
    ~next:(fun stopped ->
      match Queue.dequeue queue with
      | Some s -> `output s
      | None -> if stopped then `end_of_stream else `not_ready)
    ~feed:(fun { Fastq.name; sequence; comment; qualities } ->
      let quality =
        String.to_array qualities
        |> Array.map ~f:(fun c ->
             Phred_score.(
               of_char c
               |> function
               | Ok x -> x
               | Error _ -> ok_exn (of_probability 0.1)))
      in
      Queue.enqueue
        queue
        (`alignment
          { Sam.query_template_name = name
          ; flags = Sam.Flags.of_int 0
          ; reference_sequence = `none
          ; position = None
          ; mapping_quality = None
          ; cigar_operations = [||]
          ; next_reference_sequence = `none
          ; next_position = None
          ; template_length = None
          ; sequence = `string sequence
          ; quality
          ; optional_content = []
          }))
;;

let sam_item_to_fastq_item () : (Sam.item, Fastq.item) Transform.t =
  let queue = Queue.create () in
  Transform.make
    ()
    ~name:"sam_item_to_fastq_item"
    ~next:(fun stopped ->
      match Queue.dequeue queue with
      | Some s -> `output s
      | None -> if stopped then `end_of_stream else `not_ready)
    ~feed:
      (function
       | `header_line _ | `header _ | `comment _ | `reference_sequence_dictionary _ -> ()
       | `alignment { Sam.query_template_name; sequence = `string seq; quality; _ } ->
         let qualities =
           Array.map quality ~f:(fun c ->
             Phred_score.(
               to_char c
               |> (function
                    | Ok x -> x
                    | Error _ -> '{')
               |> Char.to_string))
           |> String.concat_array ~sep:""
         in
         Queue.enqueue
           queue
           { Fastq.name = query_template_name; sequence = seq; comment = ""; qualities }
       | `alignment { Sam.query_template_name; sequence; quality; _ } -> ())
;;

let transforms_to_do input_files_tags_and_transforms meta_output_transform output_tags =
  (* begin match general_output_tags with *)
  (* | `list _ -> error (`not_implemented "transforms_to_do: `list _") *)
  (* | #Tags.file_format as f -> return f *)
  (* end *)
  (* >>= fun output_tags -> *)
  let any_fasta_transform filename itags tr_in raw_to_item otags item_to_raw tr_out =
    let transfo =
      Transform.(
        on_error
          ~f:(fun e -> `input e)
          (compose_result_left
             (compose_results
                ~on_error:
                  (function
                   | `left e -> e
                   | `right e -> e)
                tr_in
                (on_error ~f:(fun e -> `fasta e) raw_to_item))
             (compose item_to_raw tr_out)))
      |> transform_stringify_errors
    in
    let out_extension =
      match Tags.default_extensions output_tags with
      | [ one ] -> one (* we know the output_tags correspond to a FASTA file *)
      | other -> assert false
    in
    let base = filename_chop_all_extensions filename in
    `file_to_file (filename, transfo, filename_make_new base out_extension)
  in
  let rec loop acc l =
    match l, (meta_output_transform : Tags.Output_transform.t) with
    | [], _ -> return acc
    | (filename, tags, `file_to_fastq tri) :: t, `fastq_to_file tro ->
      let m =
        let transfo =
          Transform.(compose_result_left (on_error tri (fun e -> `input e)) tro)
          |> transform_stringify_errors
        in
        let out_extension = Tags.default_extensions output_tags |> List.hd_exn in
        let base = filename_chop_all_extensions filename in
        `file_to_file (filename, transfo, filename_make_new base out_extension)
      in
      loop (m :: acc) t
    | (filename, tags, `file_to_sam_item tri) :: t, `sam_item_to_file tro ->
      let m =
        let transfo =
          Transform.compose_results
            ~on_error:
              (function
               | `left e -> `input e
               | `right e -> `output e)
            tri
            tro
          |> transform_stringify_errors
        in
        let out_extension = Tags.default_extensions output_tags |> List.hd_exn in
        let base = filename_chop_all_extensions filename in
        `file_to_file (filename, transfo, filename_make_new base out_extension)
      in
      loop (m :: acc) t
    | (filename, tags, `file_to_fastq tri) :: t, `sam_item_to_file tro ->
      let m =
        let transfo =
          Transform.(
            compose_results
              ~on_error:
                (function
                 | `left e -> `input e
                 | `right e -> `output e)
              tri
              (compose (fastq_item_to_sam_item ()) tro))
          |> transform_stringify_errors
        in
        let out_extension = Tags.default_extensions output_tags |> List.hd_exn in
        let base = filename_chop_all_extensions filename in
        `file_to_file (filename, transfo, filename_make_new base out_extension)
      in
      loop (m :: acc) t
    | (filename, tags, `file_to_sam_item tri) :: t, `fastq_to_file tro ->
      let m =
        let transfo =
          Transform.(
            on_error
              ~f:(fun e -> `input e)
              (compose_result_left tri (compose (sam_item_to_fastq_item ()) tro)))
          |> transform_stringify_errors
        in
        let out_extension = Tags.default_extensions output_tags |> List.hd_exn in
        let base = filename_chop_all_extensions filename in
        `file_to_file (filename, transfo, filename_make_new base out_extension)
      in
      loop (m :: acc) t
    | (filename, itags, `file_to_char_fasta tri) :: t, `char_fasta_to_file tro ->
      let m =
        let otags =
          match output_tags with
          | `fasta t -> t
          | _ -> assert false
        in
        any_fasta_transform
          filename
          itags
          tri
          (Fasta.Transform.char_seq_raw_item_to_item ())
          otags
          (Fasta.Transform.char_seq_item_to_raw_item ~tags:otags ())
          tro
      in
      loop (m :: acc) t
    | (filename, itags, `file_to_int_fasta tri) :: t, `int_fasta_to_file tro ->
      let m =
        let otags =
          match output_tags with
          | `fasta t -> t
          | _ -> assert false
        in
        any_fasta_transform
          filename
          itags
          tri
          (Fasta.Transform.int_seq_raw_item_to_item ())
          otags
          (Fasta.Transform.int_seq_item_to_raw_item ~tags:otags ())
          tro
      in
      loop (m :: acc) t
    | ( (a_filename, a_itags, `file_to_char_fasta a_tri)
        :: (b_filename, b_itags, `file_to_int_fasta b_tri)
        :: t
      , `fastq_to_file tro ) -> (
      let double_file_tags = `list [ a_itags; b_itags ] in
      of_result (Tags.Input_transform.from_tags double_file_tags)
      >>= function
      | `two_files_to_fastq two_fastas_to_fastq ->
        (* transform: string * string → (Fastq.item, _) result *)
        let out_extension = Tags.default_extensions output_tags |> List.hd_exn in
        let base = filename_chop_all_extensions a_filename in
        let m =
          `two_files_to_file
            ( a_filename
            , b_filename
            , filename_make_new base out_extension
            , Transform.compose_result_left two_fastas_to_fastq tro )
        in
        loop (m :: acc) t
      | _ -> error (`not_implemented "expecting two_fastas_to_fastq"))
    | (filename, tags, `file_to_fastq tri) :: t, `fastq_to_two_files tro ->
      (* tri: (string, (Fastq.item, _) result)
           tro: (Fastq.item, (string * string, output_error) result) *)
      Say.dbg "file %s should go to two FASTAs…" filename
      >>= fun () ->
      let tr =
        Transform.compose_results tri tro ~on_error:(function
          | `left e -> `input e
          | `right e -> `output e)
      in
      let base = filename_chop_all_extensions filename in
      let ext_left, ext_right =
        match Tags.default_extensions output_tags with
        | [ one; two ] -> one, two
        | _ -> assert false
      in
      let char_seq = filename_make_new (base ^ "-seq") ext_left in
      let int_seq = filename_make_new (base ^ "-qual") ext_right in
      let m = `file_to_two_files (filename, tr, char_seq, int_seq) in
      loop (m :: acc) t
    | (filename, tags, input_t) :: t, output_t ->
      ksprintf
        (fun s -> error (`not_implemented s))
        "transform: %s → %s"
        (Tags.Input_transform.name input_t)
        (Tags.Output_transform.name output_t)
  in
  loop [] input_files_tags_and_transforms
;;

let run_transform ~output_tags files =
  while_sequential files (fun file_optionally_tagged ->
    match String.lsplit2 ~on:':' file_optionally_tagged with
    | None ->
      of_result (Tags.guess_from_filename file_optionally_tagged)
      >>| Tags.to_tag
      >>= fun tags ->
      of_result (Tags.Input_transform.from_tags tags)
      >>= fun meta_transform -> return (file_optionally_tagged, tags, meta_transform)
    | Some (one, two) ->
      of_result (Tags.of_string two)
      >>= fun tags ->
      of_result (Tags.Input_transform.from_tags tags)
      >>= fun meta_transform -> return (one, tags, meta_transform))
  >>= (fun input_files_tags_and_transforms ->
        of_result (Tags.of_string output_tags)
        >>= fun tags ->
        of_result
          (Tags.Output_transform.from_tags
             tags
             ~zip_level:(Global_configuration.gzip_level ())
             ~zlib_buffer_size:(Global_configuration.zlib_buffer_size ()))
        >>= fun meta_output_transform ->
        while_sequential input_files_tags_and_transforms (fun (filename, tags, tr) ->
          Say.dbg
            "Convert %s (%s) %s %s"
            filename
            (Tags.to_string tags)
            (Tags.Input_transform.name tr)
            (Tags.Output_transform.name meta_output_transform))
        >>= fun _ ->
        transforms_to_do input_files_tags_and_transforms meta_output_transform tags
        >>= fun transforms ->
        for_concurrent transforms (function
          | `file_to_file (filein, tr, fileout) ->
            Say.dbg "Starting Transform: %s → %s" filein fileout
            >>= fun () ->
            IO.Transform.file_to_file (Transform.to_object tr) filein fileout
          | `two_files_to_file (a_filename, b_filename, out_file, out_transfo) ->
            two_files_to_file ~left:a_filename ~right:b_filename (out_file, out_transfo)
          | `file_to_two_files (filename, t, char_seq, int_seq) ->
            file_to_two_files
              ~input:filename
              t
              ~output_left:char_seq
              ~output_right:int_seq)
        >>= fun (results, errors) ->
        match errors with
        | [] -> return ()
        | some -> error (`errors some))
  >>< function
  | Ok () -> return ()
  | Error e ->
    error
      (let module M = struct
         type s =
           [ `cannot_convert_to_phred_score of int list
           | `input of Tags.Input_transform.input_error
           | `input_stream_length_mismatch
           | `output of Biocaml.Tags.Output_transform.output_error
           | `io_exn of exn
           | `lwt_exn of exn
           | `multi_files_errors of s list
           | `result_out_for_bounds of int * int
           | `sequence_names_mismatch of string * string
           | `transform of
             [ `io_exn of exn
             | `stopped_before_end_of_stream
             | `transform_error of [ `string of string ]
             ]
           ]
         [@@deriving sexp_of]

         type t =
           [ `errors of s list
           | `extension_absent
           | `extension_unknown of string
           | `not_implemented of string
           | `parse_tags of exn
           ]
         [@@deriving sexp_of]
       end
       in
      M.sexp_of_t e |> Sexp.to_string_hum)
;;

(** Return a string containing a manual in markdown-ish syntax. *)
let more_help original_help : string =
  Text.with_buffer (fun buf ->
    let open Text.Markdown in
    title buf "Biocaml's File Conversion Tool";
    section buf "Usage";
    par buf "This command is used to convert a set of input files to another format.";
    code buf "biocaml transform <input_1> <input_2> ... -to <output-format>";
    par
      buf
      "If `input_n` is simply a filename, Biocaml will try to\n\
      \        guess how to parse it (for now using the file extension). But\n\
      \        if more flexibility is needed one can describe the file-format using\n\
      \        the `Biocaml.Tags.t` format after a colon character,\n\
      \        e.g. `my_file:(gzip fastq)`.";
    par
      buf
      "The output format also uses the `Biocaml.Tags.t` format\n\
      \        (c.f. [Biocaml_tags]).";
    par
      buf
      "The transformations are based on the idea of parsing\n\
      \        *semantically similar* formats to a common representation and\n\
      \        printing that representation to compatible formats. The\n\
      \        *FASTQ*, *FASTA*, and *SAM-Item* representations are partially\n\
      \        implemented so far. Here are the *half-transformations*\n\
      \        available:";
    ul
      buf
      [ "Parse `(gzip ANY)` -> *ANY*"
      ; "Print *ANY* -> `(gzip ANY)`"
      ; "Parse `fastq` → *FASTQ*"
      ; "Print *FASTQ* -> `fastq`"
      ; "Print *FASTQ* -> `sam` (so-called non-aligned SAM/BAM files)"
      ; "Print *FASTQ* -> `bam`"
      ; "Parse `sam` → *SAM-Item*"
      ; "Parse `bam` → *SAM-Item*"
      ; "Print *SAM-Item* -> `sam`"
      ; "Print *SAM-Item* -> `bam`"
      ; "Print *SAM-Item* -> `fastq` (gets the name, sequences and qualities from \
         alignments and discards any other info)"
      ; "Parse/Print *{int, char} FASTA* <-> `{int, char} fasta` (can do some clean-up \
         of FASTA files, more to come …)"
      ; "Parse/Print (char fasta × int fasta) <-> *FASTQ* (take the sequences from the \
         first file and the qualities from the second and merge them into a FASTQ \
         record, and vice-versa)"
      ];
    par
      buf
      "Note that other meaningless file-formats are *not forbidden* like\n\
      \       `(gzip (gzip fastq))` or `(gzip bam)`.";
    par
      buf
      "`biocaml transform` outputs files named like their corresponding\n\
      \        inputs but tagged with a time-stamp and the correct file-extension.";
    lines buf [ "[Biocaml_tags]: http://biocaml.org/doc/dev/api/Biocaml_tags.html" ];
    section buf "Examples";
    par buf "A simple SAM to BAM conversion:";
    code buf "biocaml transform  my_file.sam -to bam";
    par
      buf
      "A Fastq file with a non-recognizable name converted to a\n\
      \        BAM file with maximal compression to gain a few bytes:";
    code buf "biocaml transform my_file:fastq -gzip-level 9 -to bam";
    let command = "transform" in
    command_line_section buf ~command ~original_help;
    about_the_manual_section buf ~command)
;;

let command =
  let open Command_line in
  let spec =
    let open Spec in
    file_to_file_flags ()
    ++ gzip_output_flags ~activation:false
    ++ step (fun k v -> k ~output_tags:v)
    +> flag
         "output-tags"
         ~aliases:[ "to" ]
         (optional string)
         ~doc:"<string> give the specification of the output"
    +> anon (sequence ("INPUT-FILE:tag-definition" %: string))
    ++ display_manual_flag ()
    +> help
    ++ uses_lwt ()
  in
  basic ~summary:"Transform files" spec (fun ~output_tags tagged_files ~manual help ->
    if manual
    then Say.raw "%s%!" (more_help (Lazy.force help)) >>< fun _ -> return ()
    else (
      match output_tags with
      | Some output_tags -> run_transform ~output_tags tagged_files
      | None ->
        Say.problem "Expecting either -help, -manual, or -output-tags"
        >>< fun _ -> return ()))
;;
