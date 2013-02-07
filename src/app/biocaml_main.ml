
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
              (Biocaml_tags.sexp_of_t tags |! Sexp.to_string_hum)
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
        ("info", cmd_info);
      ] in
    run whole_thing;
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

