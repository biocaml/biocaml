
open Core.Std
open Lwt
  
let verbose = ref false
let dbg fmt =
  ksprintf (fun s ->
    if !verbose
    then (eprintf "bamt: %s\n%!" s; return ())
    else return ()) fmt

let file_to_file transfo ?(input_buffer_size=42_000) bamfile
    ?(output_buffer_size=42_000) samfile =
  Lwt_io.(
    with_file ~mode:input ~buffer_size:input_buffer_size bamfile (fun i ->
      with_file ~mode:output ~buffer_size:output_buffer_size samfile (fun o ->
        let rec print_all stopped =
          match Biocaml_transform.next transfo with
          | `output (Ok s) ->
            write o s >>= fun () -> print_all stopped
          | `end_of_stream ->
            Lwt_io.printf "=====  WELL TERMINATED \n%!"
          | `not_ready ->
            dbg "NOT READY" >>= fun () ->
            if stopped then print_all stopped else return ()
          | `output (Error (`string s)) -> 
            Lwt_io.eprintf "=====  ERROR: %s\n%!" s
        in
        let rec loop () =
          read ~count:input_buffer_size i
          >>= fun read_string ->
          (* dbg verbose "read_string: %d" (String.length read_string) *)
          (* >>= fun () -> *)
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
let err_to_string sexp e = Error (`string (Sexp.to_string_hum (sexp e)))

let bam_to_sam ?input_buffer_size =
  file_to_file ?input_buffer_size
    Biocaml_transform.(
      on_output
        (bind_result_merge_error
           (bind_result_merge_error
              (Biocaml_bam.Transform.string_to_raw
                 ?zlib_buffer_size:input_buffer_size ())
              (Biocaml_bam.Transform.raw_to_item ()))
           (map_result
              (Biocaml_sam.Transform.item_to_raw ())
              (Biocaml_sam.Transform.raw_to_string ())))
        ~f:(function
        | Ok o -> Ok o
        | Error (`left (`left (`bam e))) ->
          err_to_string Biocaml_bam.Transform.sexp_of_raw_bam_error e
        | Error (`left (`left (`unzip e))) ->
          err_to_string Biocaml_zip.Transform.sexp_of_unzip_error e
        | Error (`left (`right e)) ->
          err_to_string Biocaml_bam.Transform.sexp_of_raw_to_item_error e
        | Error (`right  e) ->
          err_to_string Biocaml_sam.Transform.sexp_of_item_to_raw_error e
        )
    )

let bam_to_bam ~input_buffer_size ?output_buffer_size =
  file_to_file ~input_buffer_size ?output_buffer_size
    Biocaml_transform.(
      on_output
        (bind_result_merge_error
           (bind_result_merge_error
              (Biocaml_bam.Transform.string_to_raw
                 ~zlib_buffer_size:(10 * input_buffer_size) ())
              (Biocaml_bam.Transform.raw_to_item ()))
           (map_result 
              (Biocaml_bam.Transform.item_to_raw ())
              (Biocaml_bam.Transform.raw_to_string
                 ?zlib_buffer_size:output_buffer_size ())))
        ~f:(function
        | Ok o -> Ok o
        | Error (`left (`left (`bam e))) ->
          err_to_string Biocaml_bam.Transform.sexp_of_raw_bam_error e
        | Error (`left (`left (`unzip e))) ->
          err_to_string Biocaml_zip.Transform.sexp_of_unzip_error e
        | Error (`left (`right e)) ->
          err_to_string Biocaml_bam.Transform.sexp_of_raw_to_item_error e
        | Error (`right  e) ->
          err_to_string Biocaml_bam.Transform.sexp_of_item_to_raw_error e
        )
    )
    
module Command = Core_extended.Std.Core_command

let file_to_file_flags =
  Command.Spec.(
    step (fun k v ->
      if v then Biocaml_internal_pervasives.Debug.enable "BAM";
      if v then Biocaml_internal_pervasives.Debug.enable "SAM";
      if v then Biocaml_internal_pervasives.Debug.enable "ZIP";
      verbose := v;
      k)
    ++ flag "verbose-all" ~aliases:["V"] no_arg ~doc:" make everything over-verbose"
    ++ step (fun k v -> if v then Biocaml_internal_pervasives.Debug.enable "BAM"; k)
    ++ flag "verbose-bam"  no_arg ~doc:" make Biocaml_bam verbose"
    ++ step (fun k v -> if v then Biocaml_internal_pervasives.Debug.enable "SAM"; k)
    ++ flag "verbose-sam"  no_arg ~doc:" make Biocaml_sam verbose"
    ++ step (fun k v -> if v then Biocaml_internal_pervasives.Debug.enable "ZIP"; k)
    ++ flag "verbose-zip"  no_arg ~doc:" make Biocaml_zip verbose"
    ++ step (fun k v ->  verbose := v; k)
    ++ flag "verbose-bamt"  no_arg ~doc:" make 'bamt' itself verbose"

    ++ step (fun k v -> k ~input_buffer_size:v)
    ++ flag "input-buffer" ~aliases:["ib"] (optional_with_default 42_000 int)
      ~doc:"<int> input buffer size (Default: 42_000)"
    ++ step (fun k v -> k ~output_buffer_size:v)
    ++ flag "output-buffer" ~aliases:["ob"] (optional_with_default 42_000 int)
      ~doc:"<int> output buffer size (Default: 42_000)"
  )

let verbosity verbose_all vbam vsam vzip =
  List.filter_opt [
    if verbose_all || vbam then Some `bam else None;
    if verbose_all || vsam then Some `sam else None;
    if verbose_all || vzip then Some `zip else None;
  ]
    
let cmd_bam_to_sam =
  Command.basic ~summary:"convert from BAM to SAM"
    Command.Spec.(
      file_to_file_flags
      ++ anon ("BAM-FILE" %: string)
      ++ anon ("SAM-FILE" %: string)
    )
    (fun ~input_buffer_size ~output_buffer_size bam sam ->
      bam_to_sam ~input_buffer_size bam ~output_buffer_size sam
      |! Lwt_main.run)
    
let cmd_bam_to_bam =
  Command.basic ~summary:"convert from BAM to BAM again (after parsing everything)"
    Command.Spec.(
      file_to_file_flags
      ++ anon ("BAM-FILE" %: string)
      ++ anon ("BAM-FILE" %: string)
    )
    (fun ~input_buffer_size ~output_buffer_size bam bam2 ->
      bam_to_bam ~input_buffer_size bam ~output_buffer_size bam2
      |! Lwt_main.run)
let () =
  Command.(
    group ~summary:"fcommand examples"
      [ ("b2s", cmd_bam_to_sam);
        ("b2b", cmd_bam_to_bam)]
    |! run)

