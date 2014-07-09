open Core.Std
open Async.Std
open Biocaml_async

let dump_sam = 
  Command.async_basic
    ~summary:"Dump SAM file"
    Command.Spec.(
      Biocaml_app_common.Command_line.global_input_buffer_size_flag ()
      +> flag "-pretty" no_arg
        ~doc:" Convert to uppercase"
      +> anon (sequence ("INPUT-FILE:tag-definition" %: string))
    )
    (fun pretty input_files () -> 
       Deferred.List.iter ~how:`Sequential input_files ~f:(fun input_file ->
           let buf_len =
             !Biocaml_app_common.Global_configuration.input_buffer_size in
           Reader.with_file ~buf_len input_file ~f:(fun reader ->
               let pipe = Sam.read reader in
               Pipe.iter pipe ~f:(fun sam_item_or_error ->
                   let sexp_str = 
                     Or_error.sexp_of_t Sam.sexp_of_item sam_item_or_error
                     |> (if pretty then Sexp.to_string_hum ~indent:4
                         else Sexp.to_string)
                   in
                   printf "%s%s" sexp_str 
                     (if pretty then "\n" ^ String.make 80 ';' ^ "\n" else "");
                   return ()))))

let () =
  let version =
    sprintf "%s%s"
      Biocaml_about.version
      (Option.value_map Biocaml_about.git_commit ~default:"" ~f:(sprintf "+%s"))
  in
  let open Command in
  let whole_thing =
    group ~summary:"Biocaml's command-line application" [
      (* ("transform", Biocaml_app_transform.command); *)
      ("dump-sam", dump_sam);
    ] in
  run ~version whole_thing

(*
open Core.Std
open Flow
open Biocaml_app_common

let cmd_info =
  let say fmt =
    ksprintf (fun s -> wrap_deferred_lwt (fun () -> Lwt_io.print s)) fmt in
  Command_line.(
    basic ~summary:"Get information about files"
      Spec.(
        empty +> anon (sequence ("FILES" %: string))
        ++ uses_lwt ()
      )
      (fun files ->
        let f s =
          say "File: %S\n" s
          >>= fun () ->
          begin match Biocaml_tags.guess_from_filename s with
          | Ok tags ->
            say "  Inferred Tags: %s\n"
              (Biocaml_tags.sexp_of_file_format tags |> Sexp.to_string_hum)
          | Error e ->
            say "  Cannot retrieve tags: %s\n"
              begin match e with
              | `extension_absent -> "no extension"
              | `extension_unknown s -> sprintf "unknown extension: %S" s
              end
          end
        in
        (* List.fold files ~init:(return ()) ~f:(fun m v -> m >>= fun () -> f v)) *)
        begin
          while_sequential ~f files
          >>= fun _ ->
          return ()
        end
        >>< common_error_to_string)
    )


let () =
  Command_line.(
    let whole_thing =
      group ~summary:"Biocaml's command-line application" [
        ("bed", Biocaml_app_bed_operations.command);
        ("transform", Biocaml_app_transform.command);
        ("entrez", Biocaml_app_entrez.command);
        ("demux", Biocaml_app_demux.command);
        ("random", Biocaml_app_random.command);
        ("alignments", Biocaml_app_count_alignments.command);
        ("info", cmd_info);
      ] in
    run ~version:Biocaml_about.version whole_thing;
    let m =
      List.fold !lwts_to_run ~init:(return ()) ~f:(fun m n ->
        m >>= fun () -> n)
    in

    begin match Lwt_main.run m with
    | Ok () -> ()
    | Error s ->
      eprintf "ERROR: %s\n%!" s
    end
  )
*)
