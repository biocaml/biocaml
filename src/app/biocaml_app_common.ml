
open Core.Std
open Flow


let verbose = ref false
let dbg fmt =
  ksprintf (fun s ->
    if !verbose
    then (eprintf "biocaml: %s\n%!" s; return ())
    else return ()) fmt

let failf fmt =
  ksprintf (fun s -> error (`failure s)) fmt

let common_error_to_string = begin function
  | Ok () -> return ()
  | Error (`failure c) -> error (c)
  | Error (`io_exn e) -> error (sprintf "IO: %s" (Exn.to_string e))
end

module Command_line = struct
  include  Command


  let lwts_to_run = ref ([]: (unit, string) Flow.t list)
  let uses_lwt () =
    Spec.step (fun lwt () -> lwts_to_run := lwt :: !lwts_to_run)


  let input_buffer_size_flag () =
    Spec.(
      step (fun k v -> k ~input_buffer_size:v)
      +> flag "input-buffer" ~aliases:["ib"] (optional_with_default 42_000 int)
        ~doc:"<int> input buffer size (Default: 42_000)")

  let verbosity_flags () =
    let set_verbosity v =
      if v then Biocaml_internal_pervasives.Debug.enable "BAM";
      if v then Biocaml_internal_pervasives.Debug.enable "SAM";
      if v then Biocaml_internal_pervasives.Debug.enable "ZIP";
      verbose := v;
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
      ++ step (fun k v ->  if v then verbose := true; k)
      +> flag "verbose-app"  no_arg ~doc:" make 'biocaml' itself verbose"
    )

  let file_to_file_flags () =
    Spec.(
      verbosity_flags ()
      ++ input_buffer_size_flag ()
      ++ step (fun k v -> k ~output_buffer_size:v)
      +> flag "output-buffer" ~aliases:["ob"] (optional_with_default 42_000 int)
        ~doc:"<int> output buffer size (Default: 42_000)"
    )

end

let file_to_file transfo ?(input_buffer_size=42_000) bamfile
    ?(output_buffer_size=42_000) samfile =
  Lwt_io.(
    with_file ~mode:input ~buffer_size:input_buffer_size bamfile (fun i ->
      with_file ~mode:output ~buffer_size:output_buffer_size samfile (fun o ->
        let rec print_all stopped =
          match Biocaml_transform.next transfo with
          | `output (Ok s) ->
            wrap_io (write o) s
            >>= fun () ->
            print_all stopped
          | `end_of_stream ->
            if stopped then
              dbg "=====  WELL TERMINATED \n%!"
            else begin
              dbg "=====  PREMATURE TERMINATION \n%!"
              >>= fun () ->
              failf "file_to_file: premature termination"
            end
          | `not_ready ->
            dbg "NOT READY" >>= fun () ->
            if stopped then print_all stopped else return ()
          | `output (Error (`string s)) ->
            dbg "=====  ERROR: %s\n%!" s
        in
        let rec loop () =
          wrap_io (read ~count:input_buffer_size) i
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


let go_through_input ~transform ~max_read_bytes ~input_buffer_size filename =
  Lwt_io.(
    with_file ~mode:input ~buffer_size:input_buffer_size filename (fun i ->
      let rec count_all stopped =
        match Biocaml_transform.next transform with
        | `output (Ok _) -> count_all stopped
        | `end_of_stream ->
          if stopped then
            dbg "=====  WELL TERMINATED \n%!"
          else begin
            dbg "=====  PREMATURE TERMINATION \n%!"
            >>= fun () ->
            failf "go_through_input (%s): premature termination" filename
          end
        | `not_ready ->
          dbg "NOT READY" >>= fun () ->
          if stopped then count_all stopped else return ()
        | `output (Error (`bed s)) ->
          failf "go_throught_input:   ERROR: %s\n%!"
            (Biocaml_bed.sexp_of_parse_error s |! Sexp.to_string_hum)
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
        wrap_io (read ~count:input_buffer_size) i
        >>= fun read_string ->
        let read_bytes = (String.length read_string) + c in
        dbg "read_string: %d, c: %d" (String.length read_string) c
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

let pull_next ~in_channel ~transform =
  let rec loop () =
    match Biocaml_transform.next transform with
    | `output o -> return (Some o)
    | `end_of_stream -> return None
    | `not_ready ->
      wrap_io () ~f:(fun () ->
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

let flush_transform ~out_channel ~transform =
  let rec loop () =
    match Biocaml_transform.next transform with
    | `output o ->
      Lwt_io.(wrap_io (write out_channel) o)
      >>= fun () ->
      loop ()
    | `end_of_stream -> return ()
    | `not_ready -> return ()
  in
  loop ()

let push_to_the_max ~out_channel ~transform input =
  Biocaml_transform.feed transform input;
  flush_transform ~out_channel ~transform



