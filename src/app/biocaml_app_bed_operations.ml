
open Core.Std
open Flow
open Biocaml_app_common


let load ~on_output ?(max_read_bytes=Int.max_value) filename =
  let tags =
    match Biocaml_tags.guess_from_filename filename with
    | Ok o -> o
    | Error e -> `bed
  in
  let parsing_transform =
    match tags with
    | `bed ->
      Biocaml_transform.on_output
        (Biocaml_bed.Transform.string_to_item ())
        ~f:(function Ok o -> Ok o | Error e -> Error (`bed e))
    | `gzip `bed ->
      Biocaml_transform.compose_results
        ~on_error:(function `left l -> `unzip l | `right r -> `bed r)
        (Biocaml_zip.Transform.unzip
           ~zlib_buffer_size:(2 * !Global_configuration.input_buffer_size)
           ~format:`gzip ())
        (Biocaml_bed.Transform.string_to_item ())
    | _ ->
      (failwith "cannot handle file-format")
  in
  let transform = Biocaml_transform.on_output parsing_transform ~f:on_output in
  go_through_input ~transform ~max_read_bytes filename

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

let say fmt =
  ksprintf (fun s -> wrap_deferred_lwt (fun () -> Lwt_io.print s)) fmt

let intersects map name low high =
  match Map.find map name with
  | Some tree_ref ->
    if Biocaml_interval_tree.intersects !tree_ref ~low ~high
    then say "Yes\n"
    else say "No\n"
  | None ->
    say "Record for %S not found\n" name

let rset_folding ~fold_operation ~fold_init max_read_bytes input_files =
  let all_names = ref String.Set.empty in
  List.fold input_files ~init:(return []) ~f:(fun prev file ->
    prev >>= fun current_list ->
    let map_ref, on_output = load_rset () in
    load ?max_read_bytes ~on_output file
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
      say "%s\t%d\t%d\n" name low high))

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
         (fun max_read_bytes input_file name start stop ->
           let map_ref, on_output = load_itree () in
           begin
             load ?max_read_bytes ~on_output input_file
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
         (fun max_read_bytes input_file ->
           let map_ref, on_output = load_rset () in
           begin
             load ?max_read_bytes ~on_output input_file
             >>= fun () ->
             Map.fold !map_ref ~init:(return ()) ~f:(fun ~key ~data m ->
               m >>= fun () ->
               List.fold (Biocaml_rSet.to_range_list !data) ~init:(return ())
                 ~f:(fun m (low, high) ->
                   m >>= fun () ->
                   say "%s\t%d\t%d\n" key low high))
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
         (fun max_read_bytes input_files ->
           rset_folding
             ~fold_operation:Biocaml_rSet.union
             ~fold_init:Biocaml_rSet.empty
             max_read_bytes input_files
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
         (fun max_read_bytes input_files ->
           rset_folding
             ~fold_operation:Biocaml_rSet.inter
             ~fold_init:(Biocaml_rSet.of_range_list [Int.min_value, Int.max_value])
             max_read_bytes input_files
           >>< common_error_to_string)
      );
    ])

