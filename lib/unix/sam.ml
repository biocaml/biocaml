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
    let rec loop hdr : Biocaml.Sam.Header.t Or_error.t Deferred.t =
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
          Biocaml.Sam.Header.Item.parse line
          |> function
          | Error _ as e -> return e
          | Ok (`HD ({ version; sort_order; group_order } : Biocaml.Sam.Header.HD.t)) -> (
            match hdr.Biocaml.Sam.Header.version with
            | Some _ -> return (Or_error.error_string "multiple @HD lines not allowed")
            | None -> loop { hdr with version = Some version; sort_order; group_order })
          | Ok (`SQ x) ->
            loop { hdr with ref_seqs = x :: hdr.Biocaml.Sam.Header.ref_seqs }
          | Ok (`RG x) ->
            loop { hdr with read_groups = x :: hdr.Biocaml.Sam.Header.read_groups }
          | Ok (`PG x) ->
            loop { hdr with programs = x :: hdr.Biocaml.Sam.Header.programs }
          | Ok (`CO x) ->
            loop { hdr with comments = x :: hdr.Biocaml.Sam.Header.comments }
          | Ok (`Other x) -> loop { hdr with others = x :: hdr.Biocaml.Sam.Header.others }
        )
    in
    loop Biocaml.Sam.Header.empty
    >>| function
    | Error _ as e -> e
    | Ok ({ version; sort_order; group_order; _ } as x) ->
      let ref_seqs = List.rev x.Biocaml.Sam.Header.ref_seqs in
      let read_groups = List.rev x.Biocaml.Sam.Header.read_groups in
      let programs = List.rev x.Biocaml.Sam.Header.programs in
      let comments = List.rev x.Biocaml.Sam.Header.comments in
      let others = List.rev x.Biocaml.Sam.Header.others in
      Biocaml.Sam.Header.make
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
            (Biocaml.Sam.Alignment.parse line)
            "position"
            !pos
            Biocaml.Pos.sexp_of_t)
      in
      Ok (hdr, alignments)
  ;;

  let write_header w (h : Biocaml.Sam.Header.t) =
    let open Writer in
    (match h.Biocaml.Sam.Header.version with
     | None -> Deferred.unit
     | Some version ->
       write_line
         w
         (Biocaml.Sam.Header.HD.print
            { version
            ; sort_order = h.Biocaml.Sam.Header.sort_order
            ; group_order = h.Biocaml.Sam.Header.group_order
            }))
    >>= fun () ->
    Deferred.List.iter ~how:`Sequential h.Biocaml.Sam.Header.ref_seqs ~f:(fun x ->
      write_line w (Biocaml.Sam.Header.SQ.print x))
    >>= fun () ->
    Deferred.List.iter ~how:`Sequential h.Biocaml.Sam.Header.read_groups ~f:(fun x ->
      write_line w (Biocaml.Sam.Header.RG.print x))
    >>= fun () ->
    Deferred.List.iter ~how:`Sequential h.Biocaml.Sam.Header.programs ~f:(fun x ->
      write_line w (Biocaml.Sam.Header.PG.print x))
    >>= fun () ->
    Deferred.List.iter ~how:`Sequential h.Biocaml.Sam.Header.comments ~f:(fun x ->
      write w "@CO\t" >>= fun () -> write_line w x)
    >>= fun () ->
    Deferred.List.iter ~how:`Sequential h.Biocaml.Sam.Header.others ~f:(fun x ->
      write_line w (Biocaml.Sam.Header.Other.print x))
  ;;

  let write w ?(header = Biocaml.Sam.Header.empty) alignments =
    write_header w header
    >>= fun () ->
    Pipe.iter alignments ~f:(fun a -> Writer.write_line w (Biocaml.Sam.Alignment.print a))
  ;;

  let write_file ?perm ?append file ?header alignments =
    Writer.with_file ?perm ?append file ~f:(fun w -> write w ?header alignments)
  ;;
end

include MakeIO (Future_unix)

let parse_header text = read_header (Lines.of_string text)
