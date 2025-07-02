(******************************************************************************)
(* Input/Output                                                               *)
(******************************************************************************)
module MakeIO (Future : Future.S) = struct
  open Future

  let read_item ic : Biocaml.Fastq.item Or_error.t Reader.Read_result.t Deferred.t =
    Reader.read_line ic
    >>= function
    | `Eof -> return `Eof
    | `Ok line -> (
      match Biocaml.Fastq.name_of_line (Biocaml.Line.of_string_unsafe line) with
      | Error _ as e -> return (`Ok e)
      | Ok name -> (
        Reader.read_line ic
        >>= function
        | `Eof -> return (`Ok (Error (Error.of_string "incomplete input")))
        | `Ok line -> (
          let sequence =
            Biocaml.Fastq.sequence_of_line (Biocaml.Line.of_string_unsafe line)
          in
          Reader.read_line ic
          >>= function
          | `Eof -> return (`Ok (Error (Error.of_string "incomplete input")))
          | `Ok line -> (
            match Biocaml.Fastq.comment_of_line (Biocaml.Line.of_string_unsafe line) with
            | Error _ as e -> return (`Ok e)
            | Ok comment -> (
              Reader.read_line ic
              >>= function
              | `Eof -> return (`Ok (Error (Error.of_string "incomplete input")))
              | `Ok line -> (
                match
                  Biocaml.Fastq.qualities_of_line
                    ~sequence
                    (Biocaml.Line.of_string_unsafe line)
                with
                | Error _ as e -> return (`Ok e)
                | Ok qualities ->
                  return (`Ok (Ok { Biocaml.Fastq.name; sequence; comment; qualities }))))
            ))))
  ;;

  let read ic = Reader.read_all ic read_item

  let write_item (w : Writer.t) (x : Biocaml.Fastq.item) : unit Deferred.t =
    let open Writer in
    write_char w '@'
    >>= fun () ->
    write_line w x.name
    >>= fun () ->
    write_line w x.sequence
    >>= fun () ->
    write_char w '+'
    >>= fun () -> write_line w x.comment >>= fun () -> write_line w x.qualities
  ;;

  let write w pipe_r = Pipe.iter pipe_r ~f:(write_item w)

  let write_file ?perm ?append file pipe_r =
    Writer.with_file ?perm ?append file ~f:(fun w -> write w pipe_r)
  ;;
end

include MakeIO (Future_unix)
