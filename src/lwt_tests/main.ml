open Core.Std
open Lwt
  
let failf fmt =
  ksprintf (fun s -> fail (Failure s)) fmt
    
module Command_line = struct
  include  Core_extended.Std.Core_command
    
    
  let lwts_to_run = ref ([]: unit Lwt.t list)
  let uses_lwt () =
    Spec.step (fun lwt -> lwts_to_run := lwt :: !lwts_to_run)

  

  let bench_flags () =
    let default_reps = 1 in
    Spec.(
      step (fun k repetitions -> k ~repetitions)
      ++ flag "repetitions" ~aliases:["r"] (optional_with_default default_reps int)
        ~doc:(sprintf "<int> Number of benchmark repetitions (Default: %d)"
                default_reps)
      ++ step (fun k input_buffer_sizes -> k ~input_buffer_sizes)
      ++ flag "input-buffer-sizes" ~aliases:["IB"]
        (optional_with_default [4096]
           (arg_type (fun s -> List.map (String.split ~on:',' s) Int.of_string)))
        ~doc:"<b1,b2,…> Buffer sizes to experiment with"
      ++ step (fun k output_buffer_sizes -> k ~output_buffer_sizes)
      ++ flag "output-buffer-sizes" ~aliases:["OB"]
        (optional_with_default [4096]
           (arg_type (fun s -> List.map (String.split ~on:',' s) Int.of_string)))
        ~doc:"<b1,b2,…> Output buffer sizes to experiment with"
    )
end
let lwt_file_to_file ~transform ?(input_buffer_size=42_000) bamfile
    ?(output_buffer_size=42_000) samfile =
  Lwt_io.(
    with_file ~mode:input ~buffer_size:input_buffer_size bamfile (fun i ->
      with_file ~mode:output ~buffer_size:output_buffer_size samfile (fun o ->
        let rec print_all stopped =
          match Biocaml_transform.next transform with
          | `output (Ok s) ->
            write o s >>= fun () -> print_all stopped
          | `end_of_stream ->
            if stopped then
              Lwt_io.eprintf "=====  WELL TERMINATED \n%!"
            else begin
              Lwt_io.eprintf "=====  PREMATURE TERMINATION \n%!"
              >>= fun () ->
              fail (Failure "End")
            end
          | `not_ready ->
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
            Biocaml_transform.stop transform;
            print_all true
          ) else (
            Biocaml_transform.feed transform read_string;
            print_all false
            >>= fun () ->
            loop ()
          )
        in
        loop ()
      )
    )
  )


let lwt_go_through_input ~transform ~max_read_bytes ~input_buffer_size filename =
  Lwt_io.(
    with_file ~mode:input ~buffer_size:input_buffer_size filename (fun i ->
      let rec count_all stopped =
        match Biocaml_transform.next transform with
        | `output (Ok _) -> count_all stopped
        | `end_of_stream ->
          if stopped then
            Lwt_io.eprintf "=====  WELL TERMINATED \n%!"
          else begin
            Lwt_io.eprintf "=====  PREMATURE TERMINATION \n%!"
            >>= fun () ->
            fail (Failure "End")
          end
        | `not_ready ->
          if stopped then count_all stopped else return ()
        | `output (Error (`string s)) -> 
          Lwt_io.eprintf "=====  ERROR: %s\n%!" s
      in
      let rec loop c =
        read ~count:input_buffer_size i
        >>= fun read_string ->
        let read_bytes = (String.length read_string) + c in
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


let err_to_string sexp e = Error (`string (Sexp.to_string_hum (sexp e)))
  
let bam_to_sam input_buffer_size: (_, _) Biocaml_transform.t =
  Biocaml_transform.(
    on_output
      (bind_result_merge_error
         (bind_result_merge_error
            (Biocaml_bam.Transform.string_to_raw
               ~zlib_buffer_size:(10 * input_buffer_size) ())
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

let fastq_file_trimmer filename =
  Biocaml_transform.(
    map_result
      (bind_result
         ~on_error:(function
         | `left sti ->
           `string (Sexp.to_string_hum (Biocaml_fastq.Error.sexp_of_t sti))
         | `right (`invalid_size o) ->
           `string (sprintf "fastq invalid size: %d" o)
         )
         (Biocaml_fastq.Transform.string_to_item ~filename ())
         (Biocaml_fastq.Transform.trim (`beginning 10)))
      (Biocaml_fastq.Transform.item_to_string ())
  )
(*  string  ---  fastq-record --- trimmed-fast \
                                               f --- named-fastq --- string 
    unit  ---  count --------------------------/                              *)    

    
let cmd_convert =
  Command_line.(
    basic ~summary:"Benchmark the conversion from BAM to SAM or trimming FASTQs"
      Spec.(
        bench_flags ()
        ++ anon ("INPUT-FILE" %: string)
        ++ anon ("OUT-DIR" %: string)
      )
      (fun ~repetitions ~input_buffer_sizes ~output_buffer_sizes input_file outdir ->
        let transform input_buffer_size =
          let tags =
            match Biocaml_tags.guess_from_filename input_file with
            | Ok o -> o
            | Error e -> `bam
          in
          begin match tags with
          | `bam -> bam_to_sam input_buffer_size
          | `fastq -> fastq_file_trimmer input_file
          | _ -> failwithf "cannot handle input file: %s" input_file ()
          end
        in
        let results = ref [] in
        List.iter input_buffer_sizes (fun input_buffer_size ->
          List.iter output_buffer_sizes (fun output_buffer_size ->
            let start = Time.now () in
            for i = 1 to repetitions do
              let outfile =
                sprintf "%s/samlwt_%d_%d_%d" outdir input_buffer_size
                  output_buffer_size i in
              let transform = transform input_buffer_size in
              lwt_file_to_file ~input_buffer_size ~transform
                input_file ~output_buffer_size outfile |! Lwt_main.run
            done;
            let time = Time.(diff (now ()) start) in
            results :=
              `lwt (input_buffer_size, output_buffer_size, Core.Span.to_float time)
            :: !results;
          );
        );
        List.iter input_buffer_sizes (fun input_buffer_size ->
          List.iter output_buffer_sizes (fun output_buffer_size ->
            let start = Time.now () in
            for i = 1 to repetitions do
              let transform = transform input_buffer_size in
              let outfile =
                sprintf "%s/samix_%d_%d_%d" outdir input_buffer_size
                  output_buffer_size i in
              In_channel.with_file input_file ~f:(fun inch ->
                Out_channel.with_file outfile ~f:(fun ouch ->
                  let stream =
                    Biocaml_transform.Pull_based.(
                      of_in_channel ~buffer_size:input_buffer_size inch transform
                      |! to_stream_exn ~error_to_exn:(function `string s -> Failure s)
                    ) in
                  Stream.iter (fun s -> Out_channel.output_string ouch s) stream
                )
              )
            done;
            let time = Time.(diff (now ()) start) in
            results :=
              `unix (input_buffer_size, output_buffer_size, Core.Span.to_float time)
            :: !results;
          );
        );
        List.iter !results (function
        | `lwt (ib, ob, t) ->
          printf "Lwt\t%d\t%d\t%.2f\t%.2f\n" ib ob t (t /. float repetitions) 
        | `unix (ib, ob, t) ->
          printf "Unix\t%d\t%d\t%.2f\t%.2f\n" ib ob t (t /. float repetitions) 
        );
      )
  )
let cmd_just_parse_bam =
  Command_line.(
    basic ~summary:"Benchmark the BAM parsing"
      Spec.(
        bench_flags ()
        ++ anon ("BAM-FILE" %: string)
      )
      (fun ~repetitions ~input_buffer_sizes ~output_buffer_sizes bam ->
        let results = ref [] in
        List.iter input_buffer_sizes (fun input_buffer_size ->
          let start = Time.now () in
          for i = 1 to repetitions do
            let transform =
              Biocaml_transform.(
                on_output
                  (Biocaml_bam.Transform.string_to_raw
                     ~zlib_buffer_size:(10 * input_buffer_size) ())
                  ~f:(function
                  | Ok o -> Ok o
                  | Error ( (`bam e)) ->
                    err_to_string Biocaml_bam.Transform.sexp_of_raw_bam_error e
                  | Error ( (`unzip e)) ->
                    err_to_string Biocaml_zip.Transform.sexp_of_unzip_error e
                  )) in
            lwt_go_through_input ~transform ~max_read_bytes:max_int
              ~input_buffer_size bam |! Lwt_main.run
          done;
          let time = Time.(diff (now ()) start) in
          results := `raw (input_buffer_size, Core.Span.to_float time) :: !results;
        );
        List.iter input_buffer_sizes (fun input_buffer_size ->
          let start = Time.now () in
          for i = 1 to repetitions do
            let transform =
              Biocaml_transform.(
                on_output
                  (bind_result_merge_error
                     (Biocaml_bam.Transform.string_to_raw
                        ~zlib_buffer_size:(10 * input_buffer_size) ())
                     (Biocaml_bam.Transform.raw_to_item ()))
                  ~f:(function
                  | Ok o -> Ok o
                  | Error (`left (`bam e)) ->
                    err_to_string Biocaml_bam.Transform.sexp_of_raw_bam_error e
                  | Error (`left (`unzip e)) ->
                    err_to_string Biocaml_zip.Transform.sexp_of_unzip_error e
                  | Error (`right e) ->
                    err_to_string Biocaml_bam.Transform.sexp_of_raw_to_item_error e
                  )) in
            lwt_go_through_input ~transform ~max_read_bytes:max_int
              ~input_buffer_size bam |! Lwt_main.run
          done;
          let time = Time.(diff (now ()) start) in
          results := `item (input_buffer_size, Core.Span.to_float time) :: !results;
        );
        List.iter !results (function
        | `item (ib, t) ->
          printf "Item\t%d\t%.2f\t%.2f\n" ib t (t /. float repetitions) 
        | `raw (ib, t) ->
          printf "Raw\t%d\t%.2f\t%.2f\n" ib t (t /. float repetitions) 
        );
      )
  )
  
    
let () =
  Command_line.(
    let whole_thing =
      group ~summary:"Biocaml's benchmarks" [
        ("convert", cmd_convert);
        ("parse-bam", cmd_just_parse_bam);
      ] in
    run whole_thing;
    List.iter !lwts_to_run Lwt_main.run
  );
