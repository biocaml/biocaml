
open Core.Std
open Lwt
  
let dbg verbose fmt =
  ksprintf (fun s ->
    if verbose
    then (eprintf "bamt: %s\n%!" s; return ())
    else return ()) fmt

let bam_to_sam ?(verbose=false) ?(input_buffer_size=42_000) bamfile
    ?(output_buffer_size=42_000) samfile =
  if verbose then Biocaml_internal_pervasives.Debug.enable "BAM";
  if verbose then Biocaml_internal_pervasives.Debug.enable "SAM";
  if verbose then Biocaml_internal_pervasives.Debug.enable "ZIP";
  let transfo =
    Biocaml_transform.(
      compose 
        (compose (Biocaml_bam.raw_parser ~zlib_buffer_size:input_buffer_size ())
           (Biocaml_bam.item_parser ()))
        (compose (Biocaml_sam.downgrader ()) (Biocaml_sam.raw_printer ())))
  in
  Lwt_io.(
    with_file ~mode:input ~buffer_size:input_buffer_size bamfile (fun i ->
      with_file ~mode:output ~buffer_size:output_buffer_size samfile (fun o ->
        let rec print_all stopped =
          match Biocaml_transform.next transfo with
          | `output s ->
            write o s >>= fun () -> print_all stopped
          | `end_of_stream ->
            Lwt_io.printf "=====  WELL TERMINATED \n%!"
          | `not_ready ->
            dbg verbose "NOT READY" >>= fun () ->
            if stopped then print_all stopped else return ()
          | `error e -> 
            Lwt_io.eprintf "=====  ERROR: TODO\n%!"
        in
        let rec loop () =
          read ~count:input_buffer_size i
          >>= fun read_string ->
          dbg verbose "read_string: %d" (String.length read_string)
          >>= fun () ->
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
  
let main =
  let input_buffer_size = Int.of_string Sys.argv.(2) in
  let output_buffer_size = Int.of_string Sys.argv.(4) in
  eprintf "Start: %s\n%!" Time.(now () |! to_string);
  bam_to_sam ~verbose:Bool.(of_string Sys.argv.(1))
    ~input_buffer_size Sys.argv.(3) ~output_buffer_size Sys.argv.(5)
  >>= fun () ->
  eprintf "End: %s\n%!" Time.(now () |! to_string);
  return ()

let () =
  Lwt_main.run main
