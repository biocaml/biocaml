open Core.Std
open CFStream
open Biocaml

let transform_count fn =
  let open Sam.Flags in
  let update accu = function
    | `alignment _ -> accu + 1
    | _ -> accu
  in
  In_channel.with_file fn ~f:(fun ic ->
      Bam.in_channel_to_item_stream_exn ic
      |> Stream.fold ~init:0 ~f:update
    )
  |> print_int

let bam_alt_count fn =
  Bam_alt.with_file fn ~f:(fun header alignments ->
      Stream.fold alignments ~init:0 ~f:(fun accu _ -> accu + 1)
    )
  |> ok_exn
  |> print_int

let main mode fn () = match mode with
  | "transform" -> transform_count fn
  | "bam_alt" -> bam_alt_count fn
  | _ -> failwithf "Unknown mode %s" mode ()

let command =
  let open Command in
  basic ~summary:"Count reads in a BAM file"
    Spec.(
      step (fun k mode fn -> k mode fn)
      +> flag "mode" (required string) ~doc:"{transform|bam_alt} Choose implementation"
      +> anon ("INPUT-FILE" %: string)
    )
    main
