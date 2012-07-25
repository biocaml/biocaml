open Core.Std
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
      Lwt_io.printf "Error %s: %d bp Vs %d q-scores\n" (Biocaml_pos.to_string l)
        (String.length seq) (String.length qs)
      >>= fun () ->
      next_m ()
    | `error (`wrong_comment_line (l, _)) ->
      Lwt_io.printf "Syntax error (comment line): %s\n" (Biocaml_pos.to_string l)
      >>= fun () ->
      next_m ()
    | `error (`wrong_name_line (l, _)) ->
      Lwt_io.printf "Syntax error (name line): %s\n" (Biocaml_pos.to_string l)
      >>= fun () ->
      next_m ()
    | `error _ -> return ()
  in
  next_m ()
        
      
let test_fastq_lines file =
  let parser = Biocaml_transform.Line_oriented.parser () in
  let stream_of_lines = Lwt_io.lines_of_file file in
  Lwt_stream.iter_s (fun l ->
    Biocaml_transform.Line_oriented.feed_line parser l;
    print_next_ones parser)
    stream_of_lines

let test_fastq_string count file =
  let parser = Biocaml_transform.Line_oriented.parser () in
  Lwt_io.(with_file ~mode:input file (fun i ->
    let rec loop () =
      read ~count i
      >>= fun read_string ->
      if read_string = "" then
        return ()
      else (
        Biocaml_transform.Line_oriented.feed_string parser read_string;
        print_next_ones parser
        >>= fun () ->
        loop ())
    in
    loop ()))

let reprint_fastq file =
  let parser = Biocaml_transform.Line_oriented.parser () in
  let stream_of_lines = Lwt_io.lines_of_file file in
  let stream_of_records =
    Lwt_stream.filter_map (fun l ->
      Biocaml_transform.Line_oriented.feed_line parser l;
      match Biocaml_fastq.next parser with
      | `record r -> Some r
      | _ -> None) stream_of_lines in
  Lwt_stream.to_list stream_of_records
  >>= fun list_of_records ->
  let printer = Biocaml_fastq.printer () in
  Lwt_list.iter_s (fun r ->
    Biocaml_transform.Printer_queue.feed printer r;
    Lwt_io.printf "%s" (Biocaml_transform.Printer_queue.flush printer))
    list_of_records
  
let test_classy_trimmer file =
  let fastq_file_trimmer =
    let counter_transform =
      object
        val mutable id =  0
        method feed () = ()
        method stop = ()
        method next = id <- id + 1; `output id
      end in
    Biocaml_transform.(
      (* with_termination *)
      (compose
         (mix
            (compose
               (compose
                  (Biocaml_fastq.fastq_parser ())
                  (on_input (Biocaml_fastq.trimmer (`beginning 10))
                     ~f:(fun i ->
                       Printf.eprintf "=~= trimmer B10 got input!\n%!"; i)))
               (Biocaml_fastq.trimmer (`ending 2)))
            counter_transform
            ~f:(fun r c ->
              { r with Biocaml_fastq.name =
                  Printf.sprintf "%s -- %d" r.Biocaml_fastq.name c }))
         (Biocaml_fastq.fastq_printer ()))
       |!
      on_error ~f:(function
      | `left (`left (`left (`left parser_error))) ->
        sprintf "parser_error: %s" (Biocaml_fastq.string_of_parser_error parser_error)
      | `left (`left (`left (`right (`invalid_size _)))) -> "invalid_size"
      | `left (`left  (`right (`invalid_size _))) -> "invalid_size"
      | `left (`right _) -> assert false
      | `left `end_of_left_stream -> "end_of_left_stream"
      | `left `end_of_right_stream -> "end_of_right_stream"
      | `right _ -> assert false
      )
    )
(*  string  ---  fastq-record --- trimmed-fast \
                                               f --- named-fastq --- string 
    unit  ---  count --------------------------/                              *)    
  in
  Lwt_io.(with_file ~mode:input file (fun i ->
    let rec print_all () =
      begin match Biocaml_transform.next fastq_file_trimmer with
      | `output ( o) ->
        Lwt_io.printf "%s" o >>= fun () ->
        print_all ()
      | `end_of_stream ->
        Lwt_io.printf "=====  WELL TERMINATED \n"
      | `not_ready ->
        Lwt_io.printf "=====  NOT READY \n"
      | `error s -> 
        Lwt_io.printf "=====  ERROR: %s\n" s
      end
    in
    let rec loop () =
      read i
      >>= fun read_string ->
      if read_string = "" then (
        Biocaml_transform.stop fastq_file_trimmer;
        print_all ()
      ) else (
        Biocaml_transform.feed fastq_file_trimmer (read_string, ());
        print_all ()
        >>= fun () ->
        loop ()
      )
    in
    loop ()))
  >>= fun () ->
  Lwt_io.(flush stdout)
  
  
let () =
  Lwt_main.run (
    Lwt_io.printf "Line by line:\n" >>= fun () ->
    test_fastq_lines Sys.argv.(1) >>= fun () ->
    Lwt_io.printf "\nBy pieces of %d bytes:\n" 42 >>= fun () ->
    test_fastq_string 42 Sys.argv.(1) >>= fun () ->
    Lwt_io.printf "\nBy pieces of %d bytes:\n" 4200 >>= fun () ->
    test_fastq_string 4200 Sys.argv.(1) >>= fun () ->
    Lwt_io.printf "\nRe-print the FASTQ:\n" >>= fun () ->
    reprint_fastq Sys.argv.(1) >>= fun () ->
    Lwt_io.printf "\nTrim the FASTQ:\n" >>= fun () ->
    test_classy_trimmer Sys.argv.(1)

  )
