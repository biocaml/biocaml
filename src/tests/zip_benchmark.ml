

open Core.Std
open Biocaml
let gzip_benchotest ~feeds () =
  let transform level zlib_buffer_size =
    match level with
    | 0 -> Transform.identity ()
    | _ -> Zip.Transform.zip ~format:`gzip ~level ~zlib_buffer_size ()
  in
  let random_string size =
    String.init size
      (fun _ ->
        (* for the compression to have a meaning we need a non-uniform
           distribution: this an ASCII string with a lot of 'A' and
           more B, C, Ds than others *)
        match Random.bool (), Random.int 4 with
        | true, _ -> 'A'
        | false, 0 -> 'B'
        | false, 1 -> 'C'
        | false, 2 -> 'D'
        | _ -> Random.int 127 |> Char.of_int_exn)
  in
  let bench ((level, bufsize), string_size) =
    Random.init 0;
    let start = Time.now () in
    let transform = transform level bufsize in
    let gz_byte_count = ref 0 in
    let in_byte_count = ref 0 in
    let rec loop_until_not_ready transform =
      match Transform.next transform with
      | `output o ->
        gz_byte_count := String.length o + !gz_byte_count;
        loop_until_not_ready transform
      | `not_ready -> ()
      | `end_of_stream -> ()
    in
    for i = 0 to feeds - 1 do
      let s = random_string string_size in
      Transform.feed transform s;
      in_byte_count := String.length s + !in_byte_count;
      loop_until_not_ready transform;
    done;
    let stop = Time.now () in
    (level, bufsize, feeds, string_size, start, stop,
     float !gz_byte_count /. float !in_byte_count)
  in
  let inputs =
    List.(
      cartesian_product
        (cartesian_product [0; 2; 4; 6; 8] [100; 4096;  100_000])
        [100; 4096; 100_000]
    )
  in

  let start = Time.now () in
  let results = List.map inputs bench in
  let stop = Time.now () in
  printf "\n\n";
  printf "     Input-size       Zip-level   ZLibBuffer       Time (s)     Compression\n";
  printf "--------------------  --------- --------------- ------------  ---------------\n";
  List.iter results (fun (level, busize, feeds, string_size, start, stop, comp) ->
    printf "% 7d × % 7d        %d           % 7d       % 8.2f        %.2f\n"
      feeds string_size level busize Time.(to_float stop -. to_float start) comp
  );
  printf "\nTotal time: %s\n" (Time.diff stop start |> Time.Span.to_short_string);
  printf "\n%!"


let () =
  let open Command in
  run (basic ~summary:"Benchmark the 'zip' transform"
    Spec.(
      step (fun k feeds -> k ~feeds)
      +> flag "feeds" ~aliases:["f"] (optional_with_default 10 int)
          ~doc:"<n> Call Transform.feed <n> times."
    )
    gzip_benchotest)
