
open Core.Std
open Flow
open Biocaml_app_common

module Bam_conversion = struct

  let err_to_string sexp e = Error (`string (Sexp.to_string_hum (sexp e)))

  let bam_to_sam ?input_buffer_size =
    file_to_file ?input_buffer_size
      Biocaml_transform.(
        on_output
          (compose_results_merge_error
             (compose_results_merge_error
                (Biocaml_bam.Transform.string_to_raw
                   ?zlib_buffer_size:input_buffer_size ())
                (Biocaml_bam.Transform.raw_to_item ()))
             (compose_result_left
                (Biocaml_sam.Transform.item_to_raw ())
                (Biocaml_sam.Transform.raw_to_string ())))
          ~f:(function
          | Ok o -> Ok o
          | Error (`left (`left (`bam e))) ->
            err_to_string Biocaml_bam.Transform.sexp_of_raw_bam_error e
          | Error (`left (`left (`unzip e))) ->
            err_to_string Biocaml_zip.Transform.sexp_of_unzip_error e
          | Error (`left (`right e)) ->
            err_to_string Biocaml_bam.Transform.sexp_of_raw_to_item_error e
          | Error (`right  e) ->
            err_to_string Biocaml_sam.Error.sexp_of_item_to_raw e
          )
      )

  let bam_to_bam ~input_buffer_size ?output_buffer_size =
    file_to_file ~input_buffer_size ?output_buffer_size
      Biocaml_transform.(
        on_output
          (compose_results_merge_error
             (compose_results_merge_error
                (Biocaml_bam.Transform.string_to_raw
                   ~zlib_buffer_size:(10 * input_buffer_size) ())
                (Biocaml_bam.Transform.raw_to_item ()))
             (compose_result_left
                (Biocaml_bam.Transform.item_to_raw ())
                (Biocaml_bam.Transform.raw_to_string
                   ?zlib_buffer_size:output_buffer_size ())))
          ~f:(function
          | Ok o -> Ok o
          | Error (`left (`left (`bam e))) ->
            err_to_string Biocaml_bam.Transform.sexp_of_raw_bam_error e
          | Error (`left (`left (`unzip e))) ->
            err_to_string Biocaml_zip.Transform.sexp_of_unzip_error e
          | Error (`left (`right e)) ->
            err_to_string Biocaml_bam.Transform.sexp_of_raw_to_item_error e
          | Error (`right  e) ->
            err_to_string Biocaml_bam.Transform.sexp_of_item_to_raw_error e
          )
      )
  let cmd_bam_to_sam =
    Command_line.(
      basic ~summary:"convert from BAM to SAM"
        Spec.(
          file_to_file_flags ()
          +> anon ("BAM-FILE" %: string)
          +> anon ("SAM-FILE" %: string)
          ++ uses_lwt ())
        (fun ~input_buffer_size ~output_buffer_size bam sam ->
          bam_to_sam ~input_buffer_size bam ~output_buffer_size sam
          >>< common_error_to_string))

  let cmd_bam_to_bam =
    Command_line.(
      basic ~summary:"convert from BAM to BAM again (after parsing everything)"
        Spec.(
          file_to_file_flags ()
          +> anon ("BAM-FILE" %: string)
          +> anon ("BAM-FILE" %: string)
          ++ uses_lwt ()
        )
        (fun ~input_buffer_size ~output_buffer_size bam bam2 ->
          bam_to_bam ~input_buffer_size bam ~output_buffer_size bam2
          >>< common_error_to_string)
    )


  module With_set = struct
    module E = struct
      type t = O of int | C of int with sexp
      let compare t1 t2 =
        match t1, t2 with
        | O n, O m -> compare n m
        | C n, C m -> compare n m
        | O n, C m when n = m -> -1
        | C n, O m when n = m -> 1
        | O n, C m -> compare n m
        | C n, O m -> compare n m
    end
    module S = Set.Make (E)

    open E
    let create () = ref String.Map.empty
    let add_interval t n b e =
      match Map.find !t n with
      | Some set ->
        set := S.add !set (O b);
        set := S.add !set (C e)
      | None ->
        let set = ref S.empty in
        set := S.add !set (O b);
        set := S.add !set (C e);
        t := Map.add !t n set

    module Bed_set = Set.Make (struct
      type t = string * int * int * float with sexp
      let compare = Pervasives.compare
    end)

    let bed_set t =
      let beds = ref Bed_set.empty in
      Map.iter !t (fun ~key ~data ->
        let c_idx = ref (-1) in
        let c_val = ref 0 in
      (* printf "key: %s data length: %d\n" key (S.length !data); *)
        S.iter !data (function
        | O o ->
        (* printf "O %d -> c_idx: %d c_val: %d\n" o !c_idx !c_val; *)
          if o <> !c_idx then begin
            if !c_val > 0 then
              beds := Bed_set.add !beds (key, !c_idx, o, float !c_val);
            c_idx := o;
          end;
          incr c_val
        | C c ->
        (* printf "C %d -> c_idx: %d c_val: %d\n" c !c_idx !c_val; *)
          if c <> !c_idx then begin
            if !c_val > 0 then
              beds := Bed_set.add !beds (key, !c_idx, c, float !c_val);
            c_idx := c;
          end;
          decr c_val
        ));
      !beds


  end

  let build_wig ?(max_read_bytes=Int.max_value)
      ?(input_buffer_size=42_000) ?(output_buffer_size=42_000) bamfile wigfile =
    let tags =
      match Biocaml_tags.guess_from_filename bamfile with
      | Ok o -> o
      | Error e -> `bam
    in
    begin match tags with
    | `bam ->
      return (
        Biocaml_transform.compose_results
          ~on_error:(function `left l -> l | `right r -> `bam_to_item r)
          (Biocaml_bam.Transform.string_to_raw
             ~zlib_buffer_size:(10 * input_buffer_size) ())
          (Biocaml_bam.Transform.raw_to_item ()))
    | `sam ->
      return (
        Biocaml_transform.compose_results
          ~on_error:(function `left l -> `sam l | `right r -> `sam_to_item r)
          (Biocaml_sam.Transform.string_to_raw ())
          (Biocaml_sam.Transform.raw_to_item ()))
    | `gzip `sam ->
      return (
        Biocaml_transform.compose_results
          ~on_error:(function `left l -> `unzip l | `right r -> r)
          (Biocaml_zip.Transform.unzip
             ~zlib_buffer_size:(10 * input_buffer_size)
             ~format:`gzip ())
          (Biocaml_transform.compose_results
             ~on_error:(function `left l -> `sam l | `right r -> `sam_to_item r)
             (Biocaml_sam.Transform.string_to_raw ())
             (Biocaml_sam.Transform.raw_to_item ())))
    | _ ->
      failf "cannot handle file format"
    end
    >>= fun transfo ->
    let open With_set in
    let tree = create () in
    let transform =
      Biocaml_transform.on_output transfo ~f:(function
      | Ok (`alignment al) ->
        let open Biocaml_sam in
        Option.iter al.position (fun pos ->
          begin match al with
          | { reference_sequence = `reference_sequence rs;
              sequence = `reference; _ } ->
            add_interval tree rs.ref_name pos (pos + rs.ref_length)
          | { reference_sequence = `reference_sequence rs;
              sequence = `string s; _ } ->
            add_interval tree rs.ref_name pos (pos + String.length s)
          | _ -> ()
          end);
        Ok (`alignment al)
      | o -> o)
    in
    go_through_input ~transform ~max_read_bytes ~input_buffer_size bamfile
    >>= fun () ->
    let bed_set = bed_set tree in
    Lwt_io.(
      with_file ~mode:output
        ~buffer_size:output_buffer_size wigfile (fun o ->
          Bed_set.fold bed_set ~init:(return ()) ~f:(fun prev (chr, b, e, f) ->
            prev >>= fun () ->
            wrap_io (fprintf o "%s %d %d %g\n" chr b e) f)
        )
    )

  let cmd_extract_wig =
    Command_line.(
      basic ~summary:"Get the WIG out of a BAM or a SAM (potentially gzipped)"
        Spec.(
          file_to_file_flags ()
          +> flag "stop-after" (optional int)
            ~doc:"<n> Stop after reading <n> bytes"
          +> anon ("SAM-or-BAM-FILE" %: string)
          +> anon ("WIG-FILE" %: string)
          ++ uses_lwt ()
        )
        (fun ~input_buffer_size ~output_buffer_size max_read_bytes input_file wig ->
          build_wig ~input_buffer_size ~output_buffer_size ?max_read_bytes
            input_file wig
          >>< common_error_to_string)
    )

  let command =
    Command_line.(
      group ~summary:"Operations on BAM/SAM files (potentially gzipped)" [
        ("to-sam", cmd_bam_to_sam);
        ("to-bam", cmd_bam_to_bam);
        ("extract-wig", cmd_extract_wig);
      ])

end
module Bed_operations = struct


  let load ~on_output ~input_buffer_size ?(max_read_bytes=Int.max_value) filename =
    let tags =
      match Biocaml_tags.guess_from_filename filename with
      | Ok o -> o
      | Error e -> `bed
    in
    let parsing_transform =
      match tags with
      | `bed ->
        Biocaml_transform.on_output
          (Biocaml_bed.Transform.string_to_t ())
          ~f:(function Ok o -> Ok o | Error e -> Error (`bed e))
      | `gzip `bed ->
        Biocaml_transform.compose_results
          ~on_error:(function `left l -> `unzip l | `right r -> `bed r)
          (Biocaml_zip.Transform.unzip
             ~zlib_buffer_size:(10 * input_buffer_size) ~format:`gzip ())
          (Biocaml_bed.Transform.string_to_t ())
      | _ ->
        (failwith "cannot handle file-format")
    in
    let transform = Biocaml_transform.on_output parsing_transform ~f:on_output in
    go_through_input ~transform ~max_read_bytes ~input_buffer_size filename

  let load_itree () =
    let map = ref String.Map.empty in
    let add n low high content =
      match Map.find !map n with
      | Some tree_ref ->
        tree_ref := Biocaml_interval_tree.add !tree_ref
          ~low ~high ~data:(n, low, high, content);
      | None ->
        let tree_ref = ref Biocaml_interval_tree.empty in
        tree_ref := Biocaml_interval_tree.add !tree_ref
          ~low ~high ~data:(n, low, high, content);
        map := Map.add !map ~key:n ~data:tree_ref
    in
    let on_output = function
      | Ok (n, l, r, content) ->
        add n l r content;
        Ok ()
      | Error e -> Error e
    in
    (map, on_output)

  let load_rset () =
    let map = ref String.Map.empty in
    let add n low high content =
      match Map.find !map n with
      | Some rset_ref ->
        rset_ref := Biocaml_rSet.(union !rset_ref (of_range_list [low, high]))
      | None ->
        let rset = ref Biocaml_rSet.(of_range_list [low, high]) in
        map := Map.add !map ~key:n ~data:rset
    in
    let on_output = function
      | Ok (n, l, r, content) ->
        add n l r content;
        Ok ()
      | Error e -> Error e
    in
    (map, on_output)


  let intersects map name low high =
    match Map.find map name with
    | Some tree_ref ->
      if Biocaml_interval_tree.intersects !tree_ref ~low ~high
      then wrap_io Lwt_io.printf "Yes\n"
      else wrap_io Lwt_io.printf "No\n"
    | None ->
      wrap_io (Lwt_io.printf "Record for %S not found\n") name

  let rset_folding ~fold_operation ~fold_init
      ~input_buffer_size max_read_bytes input_files =
    let all_names = ref String.Set.empty in
    List.fold input_files ~init:(return []) ~f:(fun prev file ->
      prev >>= fun current_list ->
      let map_ref, on_output = load_rset () in
      load ~input_buffer_size ?max_read_bytes ~on_output file
      >>= fun () ->
      all_names := Set.union !all_names
        (String.Set.of_list (Map.keys !map_ref));
      return (!map_ref :: current_list))
    >>= fun files_maps ->
    Set.fold !all_names ~init:(return ()) ~f:(fun munit name ->
      munit >>= fun () ->
      List.filter_map files_maps (fun fm ->
        Map.find fm name |! Option.map ~f:(!))
      |! List.fold ~init:fold_init ~f:fold_operation
      |! Biocaml_rSet.to_range_list
      |! List.fold ~init:(return ()) ~f:(fun m (low, high) ->
        m >>= fun () ->
        wrap_io (Lwt_io.printf "%s\t%d\t%d\n" name low) high))

  let command =
    Command_line.(
      group ~summary:"Operations on BED files (potentially gzipped)" [
        ("intersects",
         basic
           ~summary:"Check if a bed file intersects and given interval"
           Spec.(
             verbosity_flags ()
             ++ input_buffer_size_flag ()
             +> flag "stop-after" (optional int)
               ~doc:"<n> Stop after reading <n> bytes"
             +> anon ("BED-ish-FILE" %: string)
             +> anon ("NAME" %: string)
             +> anon ("START" %: int)
             +> anon ("STOP" %: int)
             ++ uses_lwt ()
           )
           (fun ~input_buffer_size max_read_bytes input_file name start stop ->
             let map_ref, on_output = load_itree () in
             begin
               load ~input_buffer_size ?max_read_bytes ~on_output input_file
               >>= fun () ->
               intersects !map_ref  name start stop
             end
             >>< common_error_to_string));
        ("range-set",
         basic
           ~summary:"Output a non-overlapping set of intervals for a BED input file"
           Spec.(
             verbosity_flags ()
             ++ input_buffer_size_flag ()
             +> flag "stop-after" (optional int)
               ~doc:"<n> Stop after reading <n> bytes"
             +> anon ("BED-ish-FILE" %: string)
             ++ uses_lwt ()
           )
           (fun ~input_buffer_size max_read_bytes input_file ->
             let map_ref, on_output = load_rset () in
             begin
               load ~input_buffer_size ?max_read_bytes ~on_output input_file
               >>= fun () ->
               Map.fold !map_ref ~init:(return ()) ~f:(fun ~key ~data m ->
                 m >>= fun () ->
                 List.fold (Biocaml_rSet.to_range_list !data) ~init:(return ())
                   ~f:(fun m (low, high) ->
                     m >>= fun () ->
                     wrap_io (Lwt_io.printf "%s\t%d\t%d\n" key low) high))
             end
             >>< common_error_to_string));
        ("union",
         basic
           ~summary:"Compute the union of a bunch of BED files"
           Spec.(
             verbosity_flags ()
             ++ input_buffer_size_flag ()
             +> flag "stop-after" (optional int)
               ~doc:"<n> Stop after reading <n> bytes"
             +> anon (sequence ("BED-ish-FILES" %: string))
             ++ uses_lwt ()
           )
           (fun ~input_buffer_size max_read_bytes input_files ->
             rset_folding
               ~fold_operation:Biocaml_rSet.union
               ~fold_init:Biocaml_rSet.empty
               ~input_buffer_size max_read_bytes input_files
             >>< common_error_to_string)
        );
        ("intersection",
         basic
           ~summary:"Compute the intersection of a bunch of BED files"
           Spec.(
             verbosity_flags ()
             ++ input_buffer_size_flag ()
             +> flag "stop-after" (optional int)
               ~doc:"<n> Stop after reading <n> bytes"
             +> anon (sequence ("BED-ish-FILES" %: string))
             ++ uses_lwt ()
           )
           (fun ~input_buffer_size max_read_bytes input_files ->
             rset_folding
               ~fold_operation:Biocaml_rSet.inter
               ~fold_init:(Biocaml_rSet.of_range_list [Int.min_value, Int.max_value])
               ~input_buffer_size max_read_bytes input_files
             >>< common_error_to_string)
        );
      ])

end


let cmd_info =
  Command_line.(
    basic ~summary:"Get information about files"
      Spec.(
        empty +> anon (sequence ("FILES" %: string))
        ++ uses_lwt ()
      )
      (fun files ->
        let f s =
          wrap_io (Lwt_io.printf "File: %S\n") s
          >>= fun () ->
          begin match Biocaml_tags.guess_from_filename s with
          | Ok tags ->
            wrap_io (Lwt_io.printf "  Inferred Tags: %s\n")
              (Biocaml_tags.sexp_of_t tags |! Sexp.to_string_hum)
          | Error e ->
            wrap_io (Lwt_io.printf "  Cannot retrieve tags: %s\n")
              begin match e with
              | `extension_absent -> "no extension"
              | `extension_unknown s -> sprintf "unknown extension: %S" s
              end
          end
        in
        (* List.fold files ~init:(return ()) ~f:(fun m v -> m >>= fun () -> f v)) *)
        begin
          while_sequential ~f files
          >>= fun _ ->
          return ()
        end
        >>< common_error_to_string)
    )


let () =
  Command_line.(
    let whole_thing =
      group ~summary:"Biocaml's command-line application" [
        ("bed", Bed_operations.command);
        ("bam", Bam_conversion.command);
        ("entrez", Biocaml_app_entrez.command);
        ("demux", Biocaml_app_demux.command);
        ("info", cmd_info);
      ] in
    run whole_thing;
    let m =
      List.fold !lwts_to_run ~init:(return ()) ~f:(fun m n ->
        m >>= fun () -> n)
    in

    begin match Lwt_main.run m with
    | Ok () -> ()
    | Error s ->
      eprintf "ERROR: %s\n%!" s
    end
  )

