open Core.Std
open Flow
open Biocaml_app_common

let cmd_info =
  let say fmt = ksprintf (fun s -> wrap_deferred_lwt (fun () -> Lwt_io.print s)) fmt in
  Command_line.(
    basic
      ~summary:"Get information about files"
      Spec.(empty +> anon (sequence ("FILES" %: string)) ++ uses_lwt ())
      (fun files ->
        let f s =
          say "File: %S\n" s
          >>= fun () ->
          match Biocaml_tags.guess_from_filename s with
          | Ok tags ->
            say
              "  Inferred Tags: %s\n"
              (Biocaml_tags.sexp_of_file_format tags |> Sexp.to_string_hum)
          | Error e ->
            say
              "  Cannot retrieve tags: %s\n"
              (match e with
               | `extension_absent -> "no extension"
               | `extension_unknown s -> sprintf "unknown extension: %S" s)
        in
        (* List.fold files ~init:(return ()) ~f:(fun m v -> m >>= fun () -> f v)) *)
        while_sequential ~f files >>= (fun _ -> return ()) >>< common_error_to_string))
;;

let () =
  Command_line.(
    let whole_thing =
      group
        ~summary:"Biocaml's command-line application"
        [ "bed", Biocaml_app_bed_operations.command
        ; "transform", Biocaml_app_transform.command
        ; "entrez", Biocaml_app_entrez.command
        ; "demux", Biocaml_app_demux.command
        ; "random", Biocaml_app_random.command
        ; "alignments", Biocaml_app_count_alignments.command
        ; "info", cmd_info
        ]
    in
    run ~version:Biocaml_about.version whole_thing;
    let m = List.fold !lwts_to_run ~init:(return ()) ~f:(fun m n -> m >>= fun () -> n) in
    match Lwt_main.run m with
    | Ok () -> ()
    | Error s -> eprintf "ERROR: %s\n%!" s)
;;
