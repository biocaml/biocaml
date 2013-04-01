(** This module is a “standard library” for all the components of the
[biocaml] command line application *)

open Core.Std
open Flow
open Biocaml


let failf fmt =
  ksprintf (fun s -> error (`failure s)) fmt

let common_error_to_string = begin function
  | Ok () -> return ()
  | Error (`failure c) -> error (c)
  | Error (`lwt_exn e) -> error (sprintf "Lwt-exn: %s" (Exn.to_string e))
end

let wrap_deferred_lwt f =
  wrap_deferred (fun () -> f ()) ~on_exn:(fun e -> `lwt_exn e)


module Text = struct

  let word_wrap ?(prefix="") ?start ?(text_width=72) text =
    (* In Core_extended there were also: String.{squeeze,word_wrap} *)
    let outbuf = Buffer.create 42 in
    let out fmt = ksprintf (Buffer.add_string outbuf) fmt in
    let words =
      String.(split ~on:' '
          (map text (function '\n' | '\t' | '\r' -> ' ' | c -> c))) in
    let prefix_lgth = String.length prefix in
    let before_string = Option.value start ~default:prefix in
    let rec loop count = function
    | [] -> ()
    | "" :: more
    | "\n" :: more -> loop count more
    | one :: more ->
      let lgth = String.length one + count + 1 in
      if lgth > text_width
      then (out "\n%s%s" prefix one; loop (prefix_lgth + String.length one + 1) more)
      else
        (out "%s%s" (if count > prefix_lgth then " " else before_string) one;
         loop lgth more)
    in
    loop prefix_lgth words;
    Buffer.contents outbuf

end

module Say = struct
  open Text

  (** Verbosity of the biocaml application. *)
  let verbose = ref false

  let with_color = ref true

  let if_color f s = if !with_color then f s else s
  let red s      = if_color (sprintf "\x1b[31;1m%s\x1b[0m") s
  let green s    = if_color (sprintf "\x1b[32;1m%s\x1b[0m") s
  let blue s     = if_color (sprintf "\x1b[34;1m%s\x1b[0m") s
  let yellow s   = if_color (sprintf "\x1b[33;1m%s\x1b[0m") s

  let dbgi fmt =
    ksprintf (fun s ->
      if !verbose then
        eprintf "%s:\n%s\n%!"
          (yellow "Biocaml-debug") (word_wrap ~prefix:"  | " s)
    ) fmt

  let dbg fmt =
    ksprintf (fun s -> dbgi "%s" s; return ()) fmt

  let info fmt =
    ksprintf (fun s ->
      wrap_deferred_lwt (fun () ->
        Lwt_io.printf "%s:\n%s\n%!"
          (green "Biocaml") (word_wrap ~prefix:"  | " s))
    ) fmt

  let problem fmt =
    ksprintf (fun s ->
      wrap_deferred_lwt (fun () ->
        Lwt_io.eprintf "%s:\n%s\n%!" (red "Biocaml-ERROR") (word_wrap ~prefix:"  | " s))
    ) fmt

  let problemi fmt =
    ksprintf (fun s ->
      eprintf "%s:\n%s\n%!" (red "Biocaml-ERROR") (word_wrap ~prefix:"  | " s);
    ) fmt

  let raw fmt =
    ksprintf (fun s ->
      wrap_deferred_lwt (fun () -> Lwt_io.print s)) fmt

end

module Global_configuration = struct
  (** This module contains global configuration variables and their
     usage/access functions. *)

  (** [input_buffer_size] and [output_buffer_size] are the
     buffer-sizes used for [Lwt] channels.

     In the future we may make more distinctions:
     - possible different buffer-sizes for different files
     - use these numbers for the Lwt-channels creations but have a
       different configuration variable for the calls to
       [Lwt_io.read].
  *)
  let default_output_buffer_size = 64_000
  let default_input_buffer_size = 64_000
  let output_buffer_size = ref default_output_buffer_size
  let input_buffer_size = ref default_input_buffer_size


  (** Configuration of the [Zip.Transform.zip] transform. *)
  type gzip_output_configuration = [ `size of int | `factor of float ] * int

  let gzip_output_configuration = ref (None : gzip_output_configuration option)

  (** Get the [~zlib_buffer_size] parameter to give to the transformations. *)
  let zlib_buffer_size () =
    Option.value_map ~default:Zip.Default.zlib_buffer_size
      !gzip_output_configuration ~f:(function
        | `size s, _ -> s
        | `factor f, _ ->
          f *. (float !output_buffer_size) |> Float.iround_nearest_exn)

  (** Actually create the [Zip.Transform.zip] transform with current
     values of the configuration.  *)
  let gzip_output_transform () : (string, string) Transform.t option =
    Option.map !gzip_output_configuration (fun (size_config, level) ->
      let zlib_buffer_size = zlib_buffer_size () in
      (Biocaml_zip.Transform.zip ~format:`gzip ~level ~zlib_buffer_size ()))

  (** If the configuration is not set, then set it. This is useful
     when the information can come from a configuration file and from the
     command-line (in this case the CL takes precedence as is it parsed
     before the potential configuration file). *)
  let gzip_set_if_not_set ?(level=Zip.Default.level)
      ?(zlib_buffer_size=Zip.Default.zlib_buffer_size) () =
    match !gzip_output_configuration with
    | Some s -> ()
    | None ->
      gzip_output_configuration := Some (`size zlib_buffer_size, level)

  (** Tell if the configuration has been set (for example to add
     [".gz"] to file-names). *)
  let gzip_output_activated () : bool = (!gzip_output_configuration <> None)


  (* TODO:
        - same for Zip.unzip
  *)

end

module Command_line = struct
  (** This module is Core's [Command] module plus a few predefined
     command-line flags. *)

  include  Command

  let lwts_to_run = ref ([]: (unit, string) Flow.t list)

  (** [uses_lwt ()] means: “register this command in
     [lwts_to_run]”. It must be given last. *)
  let uses_lwt () =
    Spec.step (fun lwt () -> lwts_to_run := lwt :: !lwts_to_run)

  (** Add the [-input-buffer] size flag to the CL. It configures
     [Global_configuration.input_buffer_size]. *)
  let input_buffer_size_flag () =
    Spec.(
      step (fun k v ->
        Option.iter v (fun v ->
          Global_configuration.input_buffer_size := v);
        k)
      +> flag "input-buffer" ~aliases:["ib"] (optional int)
        ~doc:(sprintf
            "<int> set the input buffer size\n(default: %d)"
            Global_configuration.default_input_buffer_size)
    )

  (** Like [input_buffer_size_flag] but for the output-buffers. *)
  let output_buffer_size_flag () =
    Spec.(
      step (fun k v ->
        Option.iter v (fun v ->
          Global_configuration.output_buffer_size := v);
        k)
      +> flag "output-buffer" ~aliases:["ob"] (optional int)
        ~doc:(sprintf
            "<int> set the output buffer size\n(default: %d)"
            Global_configuration.default_output_buffer_size)
    )

  (** Append all the know verbosity flags (even if not relevant …). *)
  let verbosity_flags () =
    let set_verbosity v =
      if v then Biocaml_internal_pervasives.Debug.enable "BAM";
      if v then Biocaml_internal_pervasives.Debug.enable "SAM";
      if v then Biocaml_internal_pervasives.Debug.enable "ZIP";
      Say.verbose := v;
    in
    Spec.(
      step (fun k v -> set_verbosity v; k)
      +> flag "verbose-all" ~aliases:["V"] no_arg ~doc:" make everything over-verbose"
      ++ step (fun k v -> if v then Biocaml_internal_pervasives.Debug.enable "BAM"; k)
      +> flag "verbose-bam"  no_arg ~doc:" make Biocaml_bam verbose"
      ++ step (fun k v -> if v then Biocaml_internal_pervasives.Debug.enable "SAM"; k)
      +> flag "verbose-sam"  no_arg ~doc:" make Biocaml_sam verbose"
      ++ step (fun k v -> if v then Biocaml_internal_pervasives.Debug.enable "ZIP"; k)
      +> flag "verbose-zip"  no_arg ~doc:" make Biocaml_zip verbose"
      ++ step (fun k v ->  if v then Say.verbose := true; k)
      +> flag "verbose-app" ~aliases:["v"] no_arg ~doc:" make 'biocaml' itself verbose"
      ++ step (fun k v -> if v then Say.with_color := false; k)
      +> flag "no-color" no_arg ~doc:" disable colors in the output"
    )


  (** Add 3 flags that configure GZip-output transforms (also in the
     [Global_configuration] module). *)
  let gzip_output_flags ~activation =
    let level = ref None in
    let buf_size = ref None in
    let make_gzip_params factor : unit =
      let activated =
        match activation, !level with
        | false, _ -> true
        | true, None -> false
        | true, Some _ -> true in
      let lvl = Option.value !level ~default:Zip.Default.level in
      if activated
      then
        Global_configuration.gzip_output_configuration :=
          Some (match !buf_size, factor with
          | Some bf, _ -> (`size bf, lvl)
          | None, Some f -> (`factor f, lvl)
          | None, None -> (`size Zip.Default.zlib_buffer_size, lvl))
      else ()
    in
    Spec.(
      step (fun k v -> level := v; k)
      +> flag "gzip-level" ~aliases:["gz"] (optional int)
          ~doc:(sprintf "<level> %s <level> (included in [1, 9])\n\
                         (default: %s)"
              (if activation
               then "compress output files with GZip-level "
               else "set the GZip compression level to ")
              (if activation
               then "no compression"
               else (Int.to_string Zip.Default.level)))
      ++ step (fun k v -> buf_size := v; k)
      +> flag "gzip-buffer-size" ~aliases:["gzbuf"]
          (optional int)
          ~doc:(sprintf "<size> \
                         setup ZLib's internal buffer size (default: %d)"
              Zip.Default.zlib_buffer_size)
      ++ step (fun k v -> make_gzip_params v; k)
      +> flag "gzip-buffer-size-factor" ~aliases:["gzbfs"]
          (optional float)
          ~doc:"<factor> \
                multiply the output-buffer-size by <factor> to setup ZLib's\
                internal buffer (if -gzip-buffer-size is given, it takes \
                priority)"
    )

  (** Add flags relevant for file-to-file conversions. *)
  let file_to_file_flags () =
    Spec.(
      verbosity_flags ()
      ++ input_buffer_size_flag ()
      ++ output_buffer_size_flag ()
    )

end

(** Run a transform between two files. *)
let file_to_file transfo bamfile samfile =
  let input_buffer_size = !Global_configuration.input_buffer_size in
  let output_buffer_size = !Global_configuration.output_buffer_size in
  Lwt_io.(
    with_file ~mode:input ~buffer_size:input_buffer_size bamfile (fun i ->
      with_file ~mode:output ~buffer_size:output_buffer_size samfile (fun o ->
        let rec print_all stopped =
          match Biocaml_transform.next transfo with
          | `output (Ok s) ->
            wrap_deferred_lwt  (fun () -> write o s)
            >>= fun () ->
            print_all stopped
          | `end_of_stream ->
            if stopped then
              Say.dbg "=====  WELL TERMINATED \n%!"
            else begin
              Say.dbg "=====  PREMATURE TERMINATION \n%!"
              >>= fun () ->
              failf "file_to_file: premature termination"
            end
          | `not_ready ->
            Say.dbg "NOT READY" >>= fun () ->
            if stopped then print_all stopped else return ()
          | `output (Error (`string s)) ->
            Say.dbg "=====  ERROR: %s\n%!" s
        in
        let rec loop () =
          wrap_deferred_lwt (fun () -> read ~count:input_buffer_size i)
          >>= fun read_string ->
          if read_string = "" then (
            Biocaml_transform.stop transfo;
            print_all true
          ) else (
            Biocaml_transform.feed transfo read_string;
            print_all false
            >>= fun () ->
            loop ()
          )
        in
        loop ()
      )
    )
  )

(** Run a transform on an input-file and discard it's output. *)
let go_through_input ~transform ~max_read_bytes filename =
  let input_buffer_size = !Global_configuration.input_buffer_size in
  Lwt_io.(with_file ~mode:input ~buffer_size:input_buffer_size filename (fun i ->
      let rec count_all stopped =
        match Biocaml_transform.next transform with
        | `output (Ok _) -> count_all stopped
        | `end_of_stream ->
          if stopped then
            Say.dbg "=====  WELL TERMINATED \n%!"
          else begin
            Say.dbg "=====  PREMATURE TERMINATION \n%!"
            >>= fun () ->
            failf "go_through_input (%s): premature termination" filename
          end
        | `not_ready ->
          Say.dbg "NOT READY" >>= fun () ->
          if stopped then count_all stopped else return ()
        | `output (Error (`bed s)) ->
          failf "go_throught_input:   ERROR: %s\n%!"
            (Biocaml_bed.Error.sexp_of_parsing s |! Sexp.to_string_hum)
        | `output (Error (`bam s)) ->
          failf "go_throught_input:   ERROR: %s\n%!"
            (Biocaml_bam.Transform.sexp_of_raw_bam_error s |! Sexp.to_string_hum)
        | `output (Error (`sam s)) ->
          failf "go_throught_input:   ERROR: %s\n%!"
            (Biocaml_sam.Error.sexp_of_string_to_raw s |! Sexp.to_string_hum)
        | `output (Error (`unzip s)) ->
          failf "go_throught_input:   ERROR: %s\n%!"
            (Biocaml_zip.Transform.sexp_of_unzip_error s |! Sexp.to_string_hum)
        | `output (Error (`bam_to_item s)) ->
          failf "go_throught_input:   ERROR: %s\n%!"
            (Biocaml_bam.Transform.sexp_of_raw_to_item_error s |! Sexp.to_string_hum)
        | `output (Error (`sam_to_item s)) ->
          failf "go_throught_input:   ERROR: %s\n%!"
            (Biocaml_sam.Error.sexp_of_raw_to_item s |! Sexp.to_string_hum)
      in
      let rec loop c =
        wrap_deferred_lwt (fun () -> read ~count:input_buffer_size i)
        >>= fun read_string ->
        let read_bytes = (String.length read_string) + c in
        Say.dbg "read_string: %d, c: %d" (String.length read_string) c
        >>= fun () ->
        if read_bytes >= max_read_bytes then count_all false
        else if read_string = "" then (
          Biocaml_transform.stop transform;
          count_all true
        ) else (
          Biocaml_transform.feed transform read_string;
          count_all false
          >>= fun () ->
          loop read_bytes
        )
      in
      loop 0
    ))

(** Run a transform on an in_channel until it returns something or
   consumes all the input. *)
let pull_next
    ~in_channel ~(transform: (string, 'b) Transform.t) : ('b option, _) Flow.t =
  let rec loop () =
    match Biocaml_transform.next transform with
    | `output o -> return (Some o)
    | `end_of_stream -> return None
    | `not_ready ->
      wrap_deferred_lwt (fun () ->
        Lwt_io.(read ~count:(buffer_size in_channel) in_channel))
      >>= fun read_string ->
      if read_string = ""
      then begin
        Biocaml_transform.stop transform;
        loop ()
      end
      else begin
        Biocaml_transform.feed transform read_string;
        loop ()
      end
  in
  loop ()

(** Output the outputs of transform until its finished or not-ready. *)
let flush_transform
    ~out_channel ~(transform : ('a, string) Transform.t) : (unit, _) Flow.t =
  let rec loop () =
    match Biocaml_transform.next transform with
    | `output o ->
      Lwt_io.(wrap_deferred_lwt (fun () -> write out_channel o))
      >>= fun () ->
      loop ()
    | `end_of_stream -> return ()
    | `not_ready -> return ()
  in
  loop ()

(** Feed a transform and then flush it (with [flush_transform]). *)
let push_to_the_max ~out_channel ~transform input =
  Biocaml_transform.feed transform input;
  flush_transform ~out_channel ~transform

(** Merge of the possible output errors of transforms of type
   [output_transform] (most output transforms are error-free).  *)
type output_error =
[ `bam of Biocaml_bam.Transform.item_to_raw_error
| `sam of Biocaml_sam.Error.item_to_raw
]
with sexp_of

(** Generic union of possible output transforms. *)
type output_transform = [
| `to_sam_item of (Sam.item, (string, output_error) Result.t) Transform.t
| `to_gff of(Gff.stream_item, string) Transform.t
| `to_wig of (Wig.t, string) Transform.t
| `to_bed of (Bed.item, string) Transform.t
| `to_fastq of (Fastq.item, string) Transform.t
| `to_char_fasta of (Fasta.char_seq Fasta.Transform.raw_item, string) Transform.t
| `to_int_fasta of (Fasta.int_seq Fasta.Transform.raw_item, string) Transform.t
| `to_table of (Table.Row.t, string) Biocaml.Transform.t
]
let output_transform_name = function
  | `to_sam_item _ -> "to_sam_item"
  | `to_gff _ -> "to_gff"
  | `to_wig _ -> "to_wig"
  | `to_bed _ -> "to_bed"
  | `to_fastq _ -> "to_fastq"
  | `to_char_fasta _ -> "to_char_fasta"
  | `to_int_fasta _ -> "to_int_fasta"
  | `to_table _ -> "to_table"

(** Guess the [output_transform] from file tags. *)
let output_transform_of_tags
    (output_tags: Tags.t) : (output_transform, _) Flow.t =
  let rec output_transform ?with_zip output_tags =
    let with_zip_result t =
      match with_zip with
      | Some z -> Transform.compose_result_left t z
      | None -> t
    in
    let with_zip_no_error t =
      match with_zip with
      | Some z -> Transform.compose t z
      | None -> t
    in
    let to_sam_item t = return (`to_sam_item (with_zip_result t) : output_transform) in
    let zlib_buffer_size = Global_configuration.zlib_buffer_size () in
    match output_tags with
    | `raw_zip tags ->
      output_transform
        ~with_zip:(Zip.Transform.zip ~zlib_buffer_size ~format:`raw ()) tags
    | `gzip tags ->
      output_transform
        ?with_zip:(Global_configuration.gzip_output_transform ()) tags
    | `bam ->
      to_sam_item (
        Transform.compose_result_left
          (Transform.on_output
             (Bam.Transform.item_to_raw ())
             (function Ok o -> Ok o | Error e -> Error (`bam e)))
          (Bam.Transform.raw_to_string ~zlib_buffer_size ()))
    | `sam ->
      to_sam_item (
        Transform.compose_result_left
          (Transform.on_output
             (Sam.Transform.item_to_raw ())
             (function Ok o -> Ok o | Error e -> Error (`sam e)))
          (Sam.Transform.raw_to_string ()))
    | `gff tag_list ->
      let t = Gff.Transform.item_to_string ~tags:tag_list () in
      return (`to_gff (with_zip_no_error t) : output_transform)
    | `wig tag_list ->
      let t = Wig.Transform.t_to_string  ~tags:tag_list () in
      return (`to_wig (with_zip_no_error t) : output_transform)
    | `bed ->
      let t = Bed.Transform.item_to_string  () in
      return (`to_bed (with_zip_no_error t) : output_transform)
    | `fastq ->
      let t = Fastq.Transform.item_to_string () in
      return (`to_fastq (with_zip_no_error t) : output_transform)
    | `fasta `unknown
    | `fasta `char ->
    (* TODO output warning? if `unknown *)
      let t = Fasta.Transform.char_seq_raw_item_to_string () in
      return (`to_char_fasta (with_zip_no_error t) : output_transform)
    | `fasta `int ->
      let t = Fasta.Transform.int_seq_raw_item_to_string () in
      return (`to_int_fasta (with_zip_no_error t) : output_transform)
    | `table sep ->
      let t =
        Transform.on_input
          ~f:(fun row ->
            sprintf "%s\n"
              (Table.Row.to_line ~sep:(Char.to_string sep) row : Line.t :> string))
          (Transform.identity ())
      in
      return (`to_table (with_zip_no_error t) : output_transform)
  in
  output_transform output_tags

