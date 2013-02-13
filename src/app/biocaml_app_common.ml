
open Core.Std
open Flow
open Biocaml


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
  | Error (`lwt_exn e) -> error (sprintf "Lwt-exn: %s" (Exn.to_string e))
end

let wrap_deferred_lwt f =
  wrap_deferred (fun () -> f ()) ~on_exn:(fun e -> `lwt_exn e)

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
            wrap_deferred_lwt  (fun () -> write o s)
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
          wrap_deferred_lwt (fun () -> read ~count:input_buffer_size i)
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
        wrap_deferred_lwt (fun () -> read ~count:input_buffer_size i)
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

let flush_transform ~out_channel ~transform =
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

let push_to_the_max ~out_channel ~transform input =
  Biocaml_transform.feed transform input;
  flush_transform ~out_channel ~transform


type output_error =
[ `bam of Biocaml_bam.Transform.item_to_raw_error
| `sam of Biocaml_sam.Error.item_to_raw
]
with sexp_of
type output_transform = [
| `to_sam_item of (Sam.item, (string, output_error) Result.t) Transform.t
| `to_gff of(Gff.stream_item, string) Transform.t
| `to_wig of (Wig.t, string) Transform.t
| `to_bed of (Bed.t, string) Transform.t
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

let output_transform_of_tags ~zlib_buffer_size (output_tags: Tags.t) =
  let rec output_transform ?with_zip  ~zlib_buffer_size output_tags =
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
    match output_tags with
    | `raw_zip tags ->
      output_transform
        ~with_zip:(Zip.Transform.zip ~zlib_buffer_size ~format:`raw ())
        ~zlib_buffer_size tags
    | `gzip tags ->
      output_transform
        ~with_zip:(Zip.Transform.zip ~zlib_buffer_size ~format:`gzip ())
        ~zlib_buffer_size tags
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
      let t = Bed.Transform.t_to_string  () in
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
  output_transform ~zlib_buffer_size output_tags

