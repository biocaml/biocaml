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

  let read_header lines =
    let rec loop hdr : Biocaml.Sam.header Or_error.t Deferred.t =
      Pipe.peek_deferred lines
      >>= function
      | `Eof -> return (Ok hdr)
      | `Ok line -> (
        if String.length (line : Biocaml.Line.t :> string) = 0
        then return (Or_error.error_string "invalid empty line")
        else if Char.((line : Biocaml.Line.t :> string).[0] <> '@')
        then return (Ok hdr)
        else
          Pipe.junk lines
          >>= fun () ->
          Biocaml.Sam.parse_header_item line
          |> function
          | Error _ as e -> return e
          | Ok
              (`HD
                ({ Biocaml.Sam.version; sort_order; group_order } :
                  Biocaml.Sam.header_line)) -> (
            match hdr.Biocaml.Sam.version with
            | Some _ -> return (Or_error.error_string "multiple @HD lines not allowed")
            | None -> loop { hdr with version = Some version; sort_order; group_order })
          | Ok (`SQ x) -> loop { hdr with ref_seqs = x :: hdr.ref_seqs }
          | Ok (`RG x) -> loop { hdr with read_groups = x :: hdr.read_groups }
          | Ok (`PG x) -> loop { hdr with programs = x :: hdr.programs }
          | Ok (`CO x) -> loop { hdr with comments = x :: hdr.comments }
          | Ok (`Other x) -> loop { hdr with others = x :: hdr.others })
    in
    loop Biocaml.Sam.empty_header
    >>| function
    | Error _ as e -> e
    | Ok ({ version; sort_order; group_order; _ } as x) ->
      let ref_seqs = List.rev x.ref_seqs in
      let read_groups = List.rev x.read_groups in
      let programs = List.rev x.programs in
      let comments = List.rev x.comments in
      let others = List.rev x.others in
      Biocaml.Sam.header
        ?version
        ?sort_order
        ?group_order
        ~ref_seqs
        ~read_groups
        ~programs
        ~comments
        ~others
        ()
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
            (Biocaml.Sam.parse_alignment line)
            "position"
            !pos
            Biocaml.Pos.sexp_of_t)
      in
      Ok (hdr, alignments)
  ;;

  let write_header w (h : Biocaml.Sam.header) =
    let open Writer in
    (match h.version with
     | None -> Deferred.unit
     | Some version ->
       write_line
         w
         (Biocaml.Sam.print_header_line
            { Biocaml.Sam.version
            ; sort_order = h.sort_order
            ; group_order = h.group_order
            }))
    >>= fun () ->
    Deferred.List.iter h.ref_seqs ~f:(fun x -> write_line w (Biocaml.Sam.print_ref_seq x))
    >>= fun () ->
    Deferred.List.iter h.read_groups ~f:(fun x ->
      write_line w (Biocaml.Sam.print_read_group x))
    >>= fun () ->
    Deferred.List.iter h.programs ~f:(fun x -> write_line w (Biocaml.Sam.print_program x))
    >>= fun () ->
    Deferred.List.iter h.comments ~f:(fun x ->
      write w "@CO\t" >>= fun () -> write_line w x)
    >>= fun () ->
    Deferred.List.iter h.others ~f:(fun x -> write_line w (Biocaml.Sam.print_other x))
  ;;

  let write w ?(header = Biocaml.Sam.empty_header) alignments =
    write_header w header
    >>= fun () ->
    Pipe.iter alignments ~f:(fun a -> Writer.write_line w (Biocaml.Sam.print_alignment a))
  ;;

  let write_file ?perm ?append file ?header alignments =
    Writer.with_file ?perm ?append file ~f:(fun w -> write w ?header alignments)
  ;;
end

include MakeIO (Future_unix)

let parse_header text = read_header (Lines.of_string text)
