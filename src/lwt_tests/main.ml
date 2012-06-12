open Lwt
  
let test_fastq_lines file =
  let parser = Biocaml_fastq.parser () in
  let stream_of_lines = Lwt_io.lines_of_file file in
  Lwt_stream.iter_s (fun l ->
    Biocaml_fastq.feed_line parser l;
    match Biocaml_fastq.next parser with
    | `nothing_ready -> Lwt_io.printf "nothing\n"
    | `record {Biocaml_fastq. name; sequence; comment; qualities; } ->
      Lwt_io.printf "Read %S (%d bp)\n" name (String.length sequence)
    | `error (`sequence_and_qualities_do_not_match (l, seq, qs)) ->
      Lwt_io.printf "Error line %d: %d bp Vs %d q-scores\n" l
        (String.length seq) (String.length qs)
    | `error (`wrong_comment_line (l, _)) ->
      Lwt_io.printf "Syntax error (comment line) line: %d\n" l
    | `error (`wrong_name_line (l, _)) ->
      Lwt_io.printf "Syntax error (name line) line: %d\n" l
  )
    stream_of_lines


  
let () =
  Lwt_main.run (test_fastq_lines Sys.argv.(1))
