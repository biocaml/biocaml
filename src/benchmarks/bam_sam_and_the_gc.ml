
open Core.Std
open CFStream

let main ?gc_call input_file () =
  let open Biocaml in
  let module Sam = Biocaml_sam_deprecated in
  let open Sam.Flags in
  let call_gc, experiment_description = 
    match gc_call with
    | None -> ((fun () -> ()), "no GC call")
    | Some "minor" -> (Gc.minor, "call Gc.minor")
    | Some "major" -> (Gc.minor, "call Gc.major")
    | Some hmm -> 
      failwithf "cannot understand gc_call: %S" hmm ()
  in
  let update accu = function
    | `alignment { Sam.flags = al } ->
      call_gc ();
      if secondary_alignment al then accu
      else accu + 1
    | _ -> accu
  in
  let parser_function ic =
    match Filename.check_suffix input_file ".bam" with
    | true -> 
      Bam.in_channel_to_item_stream_exn ic
    | false -> Sam.in_channel_to_item_stream_exn ic
  in
  let start = Time.(now () |> to_float) in
  let count = 
    In_channel.with_file input_file ~f:(fun ic ->
        parser_function ic
        |> Stream.fold ~init:0 ~f:update
      )
  in
  let stop = Time.(now () |> to_float) in
  printf "File: %s\n" input_file;
  printf "Call GC: %s\n" experiment_description;
  printf "Secondary alignements: %d\n%!" count;
  Gc.print_stat stdout;
  printf "Total time: %f s\n%!" (stop -. start);
  ()

let command = 
  let open Command in
  basic ~summary:"Benchmark the BAM/SAM parsing"
    Spec.(
      step (fun k gc_call -> k ?gc_call)
      +> flag "call-gc" (optional string) 
          ~doc:"{minor|major} call Gc.{minor,major} at each iteration"
      +> anon ("INPUT-FILE" %: string)
    )
    main




