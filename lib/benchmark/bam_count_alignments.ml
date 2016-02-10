open Core.Std
open CFStream
open Biocaml_unix.Std

let transform_count fn =
  let open Sam.Flags in
  let update accu = function
    | `alignment _ -> accu + 1
    | _ -> accu
  in
  In_channel.with_file fn ~f:(fun ic ->
      Transform_bam.in_channel_to_item_stream_exn ic
      |> Stream.fold ~init:0 ~f:update
    )
  |> printf "%d\n"

let bam_count fn =
  Bam.with_file fn ~f:(fun _ alignments ->
      try
        Ok (
          Stream.fold alignments ~init:0 ~f:(fun accu -> function
              | Ok _ -> accu + 1
              | Error e -> failwith (Error.to_string_hum e)
            )
        )
      with Failure s -> Or_error.error_string s
    )
  |> ok_exn
  |> printf "%d\n"

let bam0_count fn =
  Bam.with_file0 fn ~f:(fun _ alignments ->
      try
        Ok (
          Stream.fold alignments ~init:0 ~f:(fun accu -> function
              | Ok _ -> accu + 1
              | Error e -> failwith (Error.to_string_hum e)
            )
        )
      with Failure s -> Or_error.error_string s
    )
  |> ok_exn
  |> printf "%d\n"

let main mode fn () = match mode with
  | "transform" -> transform_count fn
  | "bam" -> bam_count fn
  | "bam0" -> bam0_count fn
  | _ -> failwithf "Unknown mode %s" mode ()

let command =
  let open Command in
  basic ~summary:"Count reads in a BAM file"
    Spec.(
      step (fun k mode fn -> k mode fn)
      +> flag "mode" (required string) ~doc:"{bam|bam0|transform} Choose implementation"
      +> anon ("INPUT-FILE" %: string)
    )
    main
