open Lwt
  
let print_next_ones parser =
  let rec next_m () =
    match Biocaml_fastq.next parser with
    | `not_ready -> Lwt_io.printf "%%"
    | `record {Biocaml_fastq. name; sequence; comment; qualities; } ->
      Lwt_io.printf "Read %S (%d bp)\n" name (String.length sequence)
      >>= fun () ->
      next_m ()
    | `error (`sequence_and_qualities_do_not_match (l, seq, qs)) ->
      Lwt_io.printf "Error line %d: %d bp Vs %d q-scores\n" l
        (String.length seq) (String.length qs)
      >>= fun () ->
      next_m ()
    | `error (`wrong_comment_line (l, _)) ->
      Lwt_io.printf "Syntax error (comment line) line: %d\n" l
      >>= fun () ->
      next_m ()
    | `error (`wrong_name_line (l, _)) ->
      Lwt_io.printf "Syntax error (name line) line: %d\n" l
      >>= fun () ->
      next_m ()
  in
  next_m ()
        
      
let test_fastq_lines file =
  let parser = Biocaml_fastq.parser () in
  let stream_of_lines = Lwt_io.lines_of_file file in
  Lwt_stream.iter_s (fun l ->
    Biocaml_fastq.feed_line parser l;
    print_next_ones parser)
    stream_of_lines

let test_fastq_string count file =
  let parser = Biocaml_fastq.parser () in
  Lwt_io.(with_file ~mode:input file (fun i ->
    let rec loop () =
      read ~count i
      >>= fun read_string ->
      if read_string = "" then
        return ()
      else (
        Biocaml_fastq.feed_string parser read_string;
        print_next_ones parser
        >>= fun () ->
        loop ())
    in
    loop ()))

let reprint_fastq file =
  let parser = Biocaml_fastq.parser () in
  let stream_of_lines = Lwt_io.lines_of_file file in
  let stream_of_records =
    Lwt_stream.filter_map (fun l ->
      Biocaml_fastq.feed_line parser l;
      match Biocaml_fastq.next parser with
      | `record r -> Some r
      | _ -> None) stream_of_lines in
  Lwt_stream.to_list stream_of_records
  >>= fun list_of_records ->
  let printer = Biocaml_fastq.printer () in
  Lwt_list.iter_s (fun r ->
    Biocaml_fastq.feed_record printer r;
    Lwt_io.printf "%s" (Biocaml_fastq.get_string printer))
    list_of_records
  
  
  
let () =
  Lwt_main.run (
    Lwt_io.printf "Line by line:\n" >>= fun () ->
    test_fastq_lines Sys.argv.(1) >>= fun () ->
    Lwt_io.printf "\nBy pieces of %d bytes:\n" 42 >>= fun () ->
    test_fastq_string 42 Sys.argv.(1) >>= fun () ->
    Lwt_io.printf "\nBy pieces of %d bytes:\n" 4200 >>= fun () ->
    test_fastq_string 4200 Sys.argv.(1) >>= fun () ->
    Lwt_io.printf "\nRe-print the FASTQ:\n" >>= fun () ->
    reprint_fastq Sys.argv.(1)

  )
