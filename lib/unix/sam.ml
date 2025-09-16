module Result = Biocaml_result

(******************************************************************************)
(* Input/Output                                                               *)
(******************************************************************************)
module MakeIO (Future : Future.S) = struct
  open Future

  module Lines = struct
    include Lines
    include MakeIO (Future)
  end

  let read_header lines : Biocaml.Sam.Header.t Or_error.t Deferred.t =
    let rec loop hdr_items : Biocaml.Line.t list Or_error.t Deferred.t =
      Pipe.peek_deferred lines
      >>= function
      | `Eof -> return (Ok hdr_items)
      | `Ok line ->
        if not (Biocaml.Sam.must_be_header (line : Biocaml.Line.t :> string))
        then return (Ok hdr_items)
        else Pipe.junk lines >>= fun () -> loop (line :: hdr_items)
    in
    loop []
    >>| function
    | Error _ as e -> e
    | Ok hdr_lines ->
      (hdr_lines :> string list) |> List.rev |> Biocaml.Sam.Header.of_lines
  ;;

  let read ?(start = Biocaml.Pos.(incr_line unknown)) r =
    let pos = ref start in
    let lines =
      Pipe.map (Lines.read r) ~f:(fun line ->
        pos := Biocaml.Pos.incr_line !pos;
        line)
    in
    read_header lines
    >>| function
    | Error _ as e -> Or_error.tag_arg e "position" !pos Biocaml.Pos.sexp_of_t
    | Ok hdr ->
      let alignments =
        Pipe.map lines ~f:(fun line ->
          Or_error.tag_arg
            (Biocaml.Sam.Alignment.of_line (line : Biocaml.Line.t :> string))
            "position"
            !pos
            Biocaml.Pos.sexp_of_t)
      in
      Ok (hdr, alignments)
  ;;

  let write_header w (header : Biocaml.Sam.Header.t) =
    (header :> Biocaml.Sam.Header.Item.t list)
    |> Deferred.List.iter ~how:`Sequential ~f:(fun x ->
      Writer.write_line w (Biocaml.Sam.Header.Item.to_line x))
  ;;

  let write w ?header alignments =
    (match header with
     | None -> return ()
     | Some header -> write_header w header)
    >>= fun () ->
    Pipe.iter alignments ~f:(fun a ->
      Writer.write_line w (Biocaml.Sam.Alignment.to_line a))
  ;;

  let write_file ?perm ?append file ?header alignments =
    Writer.with_file ?perm ?append file ~f:(fun w -> write w ?header alignments)
  ;;
end

include MakeIO (Future_unix)

let parse_header text = read_header (Lines.of_string text)
