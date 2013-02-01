open Core.Std
open Flow
open Biocaml_app_common
open Biocaml

type spec =  Tags.t * int with sexp
  
let parse_spec s =
  try
    return (Sexp.of_string s |! spec_of_sexp)
  with
    e -> error (`parse_spcification e)

let io_with_out_channel out ?buffer_size ~f =
  begin match out with
  | `stdout -> return Lwt_io.stdout
  | `strerr -> return Lwt_io.stderr
  | `file file ->
    wrap_io (Lwt_io.open_file ~mode:Lwt_io.output ?buffer_size) file
  end
  >>= fun outchan ->
  begin
    f outchan
    >>< begin function
    | Ok o ->
      wrap_io Lwt_io.close outchan
      >>= fun () ->
      return o
    | Error e ->
      begin match out with
      | `file _ ->
        wrap_io Lwt_io.close outchan
        >>= fun _ ->
        error e
      | _ -> error e
      end
    end
  end
      
let io_fprintf out fmt =
  ksprintf (fun s -> wrap_io (Lwt_io.fprint out) s) fmt
  
let random_fastq_transform () =
  let todo = ref 0 in
  let seq_num = ref 0 in
  Transform.make ()
    ~next:(fun stopped ->
      match !todo, stopped with
      | 0, true -> `end_of_stream
      | 0, false -> `not_ready
      | n, _  when n < 0 -> assert false
      | n, _ ->
        decr todo;
        incr seq_num;
        `output (Fastq.(
          { name = sprintf "Random sequence: %d" !seq_num; 
            sequence = "ACGTTGTTAANNTGA";
            comment = "";
            qualities = "!!!!!!!!!!!!!!!" } )))
    ~feed:(fun () -> incr todo)

  
let do_random ~output_file spec =
  let zlib_buffer_size  = 4200 in
  let output_meta_channel =
    match output_file with
    | None -> `stdout
    | Some f -> `file f in
  parse_spec spec
  >>= fun (tags, number_of_elements) ->
  output_transform_of_tags ~zlib_buffer_size tags
  >>= begin function
  | `to_fastq tr ->
    let transform = Transform.compose (random_fastq_transform ()) tr in
    io_with_out_channel output_meta_channel (fun out ->
      let rec loop = function
        | 0 -> io_fprintf out "%!"
        | n ->
          Transform.feed transform ();
          begin match Transform.next transform with
          | `not_ready | `end_of_stream -> assert false
          | `output s ->
            io_fprintf out "%s" s
            >>= fun () ->
            loop (n - 1)
          end
      in
      loop number_of_elements)
    >>= fun () ->
    return ()
  | _ ->
    io_with_out_channel `stdout (fun out ->
      io_fprintf out "not to-fastq!\n%!")
    >>= fun () ->
    return ()
  end
  >>= fun () ->
    
  
  return ()



    
let stringify m =
  m >>< begin function
  | Ok o -> return o
  | Error e ->
    error (<:sexp_of< [
    | `io_exn of exn
    | `parse_spcification of exn
    ] >> e |! Sexp.to_string_hum)
  end

let do_random ~output_file spec =
  stringify (do_random ~output_file spec)
  
let command =
  let open Command_line in
  let spec =
    let open Spec in
    empty
    ++ step (fun k v -> k ~output_file:v)
    +> flag "output-file" ~aliases:["o"] (optional string)
      ~doc:"<filename> output to a file"
    +> anon (("specification" %: string))
    ++ uses_lwt ()
  in
  basic ~summary:"Generate random files" spec do_random
