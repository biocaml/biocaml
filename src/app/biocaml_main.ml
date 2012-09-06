
open Core.Std
open Lwt
  
let verbose = ref false
let dbg fmt =
  ksprintf (fun s ->
    if !verbose
    then (eprintf "biocaml: %s\n%!" s; return ())
    else return ()) fmt

let failf fmt =
  ksprintf (fun s -> fail (Failure s)) fmt
    
module Command_line = struct
  include  Core_extended.Std.Core_command
    
    
  let lwts_to_run = ref ([]: unit Lwt.t list)
  let uses_lwt () =
    Spec.step (fun lwt -> lwts_to_run := lwt :: !lwts_to_run)

  
  let input_buffer_size_flag () =
    Spec.(
      step (fun k v -> k ~input_buffer_size:v)
      ++ flag "input-buffer" ~aliases:["ib"] (optional_with_default 42_000 int)
        ~doc:"<int> input buffer size (Default: 42_000)")
      
  let verbosity_flags () = 
    let set_verbosity v =
      if v then Biocaml_internal_pervasives.Debug.enable "BAM";
      if v then Biocaml_internal_pervasives.Debug.enable "SAM";
      if v then Biocaml_internal_pervasives.Debug.enable "ZIP";
      verbose := v;
    in
    Spec.(
      step (fun k v -> set_verbosity v; k)
      ++ flag "verbose-all" ~aliases:["V"] no_arg ~doc:" make everything over-verbose"
      ++ step (fun k v -> if v then Biocaml_internal_pervasives.Debug.enable "BAM"; k)
      ++ flag "verbose-bam"  no_arg ~doc:" make Biocaml_bam verbose"
      ++ step (fun k v -> if v then Biocaml_internal_pervasives.Debug.enable "SAM"; k)
      ++ flag "verbose-sam"  no_arg ~doc:" make Biocaml_sam verbose"
      ++ step (fun k v -> if v then Biocaml_internal_pervasives.Debug.enable "ZIP"; k)
      ++ flag "verbose-zip"  no_arg ~doc:" make Biocaml_zip verbose"
      ++ step (fun k v ->  if v then verbose := true; k)
      ++ flag "verbose-app"  no_arg ~doc:" make 'biocaml' itself verbose"
    )

  let file_to_file_flags () =
    Spec.(
      verbosity_flags ()
      ++ input_buffer_size_flag ()
      ++ step (fun k v -> k ~output_buffer_size:v)
      ++ flag "output-buffer" ~aliases:["ob"] (optional_with_default 42_000 int)
        ~doc:"<int> output buffer size (Default: 42_000)"
    )

end

let file_to_file transfo ?(input_buffer_size=42_000) bamfile
    ?(output_buffer_size=42_000) samfile =
  Lwt_io.(
    with_file ~mode:input ~buffer_size:input_buffer_size bamfile (fun i ->
      with_file ~mode:output ~buffer_size:output_buffer_size samfile (fun o ->
        let rec print_all stopped =
          match Biocaml_transform.next transfo with
          | `output (Ok s) ->
            write o s >>= fun () -> print_all stopped
          | `end_of_stream ->
            if stopped then
              Lwt_io.eprintf "=====  WELL TERMINATED \n%!"
            else begin
              Lwt_io.eprintf "=====  PREMATURE TERMINATION \n%!"
              >>= fun () ->
              fail (Failure "End")
            end
          | `not_ready ->
            dbg "NOT READY" >>= fun () ->
            if stopped then print_all stopped else return ()
          | `output (Error (`string s)) -> 
            Lwt_io.eprintf "=====  ERROR: %s\n%!" s
        in
        let rec loop () =
          read ~count:input_buffer_size i
          >>= fun read_string ->
          (* dbg verbose "read_string: %d" (String.length read_string) *)
          (* >>= fun () -> *)
          if read_string = "" then (
            Biocaml_transform.stop transfo;
            print_all true
          ) else (
            Biocaml_transform.feed transfo read_string;
            print_all false
            >>= fun () ->
            loop ()
          )
        in
        loop ()
      )
    )
  )


let go_through_input ~transform ~max_read_bytes ~input_buffer_size filename =
  Lwt_io.(
    with_file ~mode:input ~buffer_size:input_buffer_size filename (fun i ->
      let rec count_all stopped =
        match Biocaml_transform.next transform with
        | `output (Ok _) -> count_all stopped
        | `end_of_stream ->
          if stopped then
            Lwt_io.eprintf "=====  WELL TERMINATED \n%!"
          else begin
            Lwt_io.eprintf "=====  PREMATURE TERMINATION \n%!"
            >>= fun () ->
            fail (Failure "End")
          end
        | `not_ready ->
          dbg "NOT READY" >>= fun () ->
          if stopped then count_all stopped else return ()
        | `output (Error (`bed s)) -> 
          Lwt_io.eprintf "=====  ERROR: %s\n%!"
            (Biocaml_bed.sexp_of_parse_error s |! Sexp.to_string_hum)
        | `output (Error (`bam s)) -> 
          Lwt_io.eprintf "=====  ERROR: %s\n%!"
            (Biocaml_bam.Transform.sexp_of_raw_bam_error s |! Sexp.to_string_hum)
        | `output (Error (`sam s)) -> 
          Lwt_io.eprintf "=====  ERROR: %s\n%!"
            (Biocaml_sam.Transform.sexp_of_string_to_raw_error s |! Sexp.to_string_hum)
        | `output (Error (`unzip s)) -> 
          Lwt_io.eprintf "=====  ERROR: %s\n%!"
            (Biocaml_zip.Transform.sexp_of_unzip_error s |! Sexp.to_string_hum)
        | `output (Error (`bam_to_item s)) -> 
          Lwt_io.eprintf "=====  ERROR: %s\n%!"
            (Biocaml_bam.Transform.sexp_of_raw_to_item_error s |! Sexp.to_string_hum)
        | `output (Error (`sam_to_item s)) -> 
          Lwt_io.eprintf "=====  ERROR: %s\n%!"
            (Biocaml_sam.Transform.sexp_of_raw_to_item_error s |! Sexp.to_string_hum)
      in
      let rec loop c =
        read ~count:input_buffer_size i
        >>= fun read_string ->
        let read_bytes = (String.length read_string) + c in
        dbg "read_string: %d, c: %d" (String.length read_string) c
        >>= fun () ->
        if read_bytes >= max_read_bytes then count_all false
        else if read_string = "" then (
          Biocaml_transform.stop transform;
          count_all true
        ) else (
          Biocaml_transform.feed transform read_string;
          count_all false
          >>= fun () ->
          loop read_bytes
        )
      in
      loop 0
    ))

module Http_method = struct
  type t = string -> (string, string) Result.t Lwt.t

  let detect_exe exe =
    Lwt_unix.system ("which \"" ^ exe ^ "\" > /dev/null 2>&1")
    >>= fun ps ->
    begin match ps with
    | Lwt_unix.WEXITED 0 -> return true
    | _ -> return false
    end

  let shell_command_to_string s =
    dbg "Running %S" s >>= fun () ->
    let inprocess = Lwt_process.(open_process_full (shell s)) in
    Lwt_list.map_p Lwt_io.read [inprocess#stdout; inprocess#stderr; ]
    >>= fun output ->
    inprocess#status
    >>= fun status ->
    begin match status with
    | Lwt_unix.WEXITED 0 -> return (List.hd_exn output)
    | _ -> failf "Cmd %S failed: %s" s (List.nth_exn output 1)
    end
    
      
  let discover () =
    detect_exe "curl"
    >>= fun curls_there ->
    if curls_there
    then return (fun s ->
      let cmd = sprintf "curl -f -k -L %S" s in
      shell_command_to_string cmd)
    else begin
      detect_exe "wget"
      >>= fun wgets_there ->
      if wgets_there
      then return (fun s ->
        let cmd = sprintf "wget --no-check-certificate -q -O - %S" s in
        shell_command_to_string cmd)
      else fail (Failure "No HTTP command found")
    end
end

module Entrez = struct
    
  let pubmed s = 
    Biocaml_entrez.esearch_url `pubmed (String.concat ~sep:" " s)
      
  let command = 
    Command_line.(
      group ~summary:"Query the Entrez/EUtils database" [
        ("pubmed", 
         basic
           ~summary:"Test a simple query in pubmed journals"
           Spec.(
             verbosity_flags ()
             ++ anon (sequence "SEARCH" string)
             ++ uses_lwt ()
           )
           (fun search ->
             let query = pubmed search in
             Lwt_io.printf "Query: %s\n" query >>= fun () ->
             Http_method.discover () >>= fun http ->
             http query >>= fun result ->
             Lwt_io.printf "Result:\n%s\n" result));
      ])
      
end
      





    
module Bam_conversion = struct

  let err_to_string sexp e = Error (`string (Sexp.to_string_hum (sexp e)))
    
  let bam_to_sam ?input_buffer_size =
    file_to_file ?input_buffer_size
      Biocaml_transform.(
        on_output
          (bind_result_merge_error
             (bind_result_merge_error
                (Biocaml_bam.Transform.string_to_raw
                   ?zlib_buffer_size:input_buffer_size ())
                (Biocaml_bam.Transform.raw_to_item ()))
             (map_result
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
            err_to_string Biocaml_sam.Transform.sexp_of_item_to_raw_error e
          )
      )

  let bam_to_bam ~input_buffer_size ?output_buffer_size =
    file_to_file ~input_buffer_size ?output_buffer_size
      Biocaml_transform.(
        on_output
          (bind_result_merge_error
             (bind_result_merge_error
                (Biocaml_bam.Transform.string_to_raw
                   ~zlib_buffer_size:(10 * input_buffer_size) ())
                (Biocaml_bam.Transform.raw_to_item ()))
             (map_result 
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
          ++ anon ("BAM-FILE" %: string)
          ++ anon ("SAM-FILE" %: string)
          ++ uses_lwt ())
        (fun ~input_buffer_size ~output_buffer_size bam sam ->
          bam_to_sam ~input_buffer_size bam ~output_buffer_size sam))
      
  let cmd_bam_to_bam =
    Command_line.(
      basic ~summary:"convert from BAM to BAM again (after parsing everything)"
        Spec.(
          file_to_file_flags ()
          ++ anon ("BAM-FILE" %: string)
          ++ anon ("BAM-FILE" %: string)
          ++ uses_lwt ()
        )
        (fun ~input_buffer_size ~output_buffer_size bam bam2 ->
          bam_to_bam ~input_buffer_size bam ~output_buffer_size bam2)
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

  let build_wig ?(max_read_bytes=max_int)
      ?(input_buffer_size=42_000) ?(output_buffer_size=42_000) bamfile wigfile =
    let tags =
      match Biocaml_tags.guess_from_filename bamfile with
      | Ok o -> o
      | Error e -> `bam
    in
    begin match tags with
    | `bam ->
      return (
        Biocaml_transform.bind_result
          ~on_error:(function `left l -> l | `right r -> `bam_to_item r)
          (Biocaml_bam.Transform.string_to_raw
             ~zlib_buffer_size:(10 * input_buffer_size) ())
          (Biocaml_bam.Transform.raw_to_item ()))
    | `sam ->
      return (
        Biocaml_transform.bind_result
          ~on_error:(function `left l -> `sam l | `right r -> `sam_to_item r)
          (Biocaml_sam.Transform.string_to_raw ())
          (Biocaml_sam.Transform.raw_to_item ()))
    | `gzip `sam ->
      return (
        Biocaml_transform.bind_result
          ~on_error:(function `left l -> `unzip l | `right r -> r)
          (Biocaml_zip.Transform.unzip
             ~zlib_buffer_size:(10 * input_buffer_size)
             ~format:`gzip ())
          (Biocaml_transform.bind_result
             ~on_error:(function `left l -> `sam l | `right r -> `sam_to_item r)
             (Biocaml_sam.Transform.string_to_raw ())
             (Biocaml_sam.Transform.raw_to_item ())))
    | _ ->
      fail (Failure "cannot handle file format")
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
            fprintf o "%s %d %d %g\n" chr b e f)
        )
    )
      
  let cmd_extract_wig =
    Command_line.(
      basic ~summary:"Get the WIG out of a BAM or a SAM (potentially gzipped)"
        Spec.(
          file_to_file_flags ()
          ++ flag "stop-after" (optional int)
            ~doc:"<n> Stop after reading <n> bytes"
          ++ anon ("SAM-or-BAM-FILE" %: string)
          ++ anon ("WIG-FILE" %: string)
          ++ uses_lwt ()
        )
        (fun ~input_buffer_size ~output_buffer_size max_read_bytes input_file wig ->
          build_wig ~input_buffer_size ~output_buffer_size ?max_read_bytes
            input_file wig)
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

    
  let load ~on_output ~input_buffer_size ?(max_read_bytes=max_int) filename =
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
        Biocaml_transform.bind_result
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
      then Lwt_io.printf "Yes\n"
      else Lwt_io.printf "No\n"
    | None ->
      Lwt_io.printf "Record for %S not found\n" name
      
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
        Lwt_io.printf "%s\t%d\t%d\n" name low high))
      
  let command =
    Command_line.(
      group ~summary:"Operations on BED files (potentially gzipped)" [
        ("intersects",
         basic
           ~summary:"Check if a bed file intersects and given interval"
           Spec.(
             verbosity_flags ()
             ++ input_buffer_size_flag ()
             ++ flag "stop-after" (optional int)
               ~doc:"<n> Stop after reading <n> bytes"
             ++ anon ("BED-ish-FILE" %: string)
             ++ anon ("NAME" %: string)
             ++ anon ("START" %: int)
             ++ anon ("STOP" %: int)
             ++ uses_lwt ()
           )
           (fun ~input_buffer_size max_read_bytes input_file name start stop ->
             let map_ref, on_output = load_itree () in
             load ~input_buffer_size ?max_read_bytes ~on_output input_file
             >>= fun () ->
             intersects !map_ref  name start stop));
        ("range-set",
         basic
           ~summary:"Output a non-overlapping set of intervals for a BED input file"
           Spec.(
             verbosity_flags ()
             ++ input_buffer_size_flag ()
             ++ flag "stop-after" (optional int)
               ~doc:"<n> Stop after reading <n> bytes"
             ++ anon ("BED-ish-FILE" %: string)
             ++ uses_lwt ()
           )
           (fun ~input_buffer_size max_read_bytes input_file ->
             let map_ref, on_output = load_rset () in
             load ~input_buffer_size ?max_read_bytes ~on_output input_file
             >>= fun () ->
             Map.fold !map_ref ~init:(return ()) ~f:(fun ~key ~data m ->
               m >>= fun () ->
               List.fold (Biocaml_rSet.to_range_list !data) ~init:(return ())
                 ~f:(fun m (low, high) ->
                   m >>= fun () ->
                   Lwt_io.printf "%s\t%d\t%d\n" key low high)
             )));
        ("union",
         basic
           ~summary:"Compute the union of a bunch of BED files"
           Spec.(
             verbosity_flags ()
             ++ input_buffer_size_flag ()
             ++ flag "stop-after" (optional int)
               ~doc:"<n> Stop after reading <n> bytes"
             ++ anon (sequence "BED-ish-FILES" string)
             ++ uses_lwt ()
           )
           (fun ~input_buffer_size max_read_bytes input_files ->
             rset_folding
               ~fold_operation:Biocaml_rSet.union
               ~fold_init:Biocaml_rSet.empty
               ~input_buffer_size max_read_bytes input_files)
        );
        ("intersection",
         basic
           ~summary:"Compute the intersection of a bunch of BED files"
           Spec.(
             verbosity_flags ()
             ++ input_buffer_size_flag ()
             ++ flag "stop-after" (optional int)
               ~doc:"<n> Stop after reading <n> bytes"
             ++ anon (sequence "BED-ish-FILES" string)
             ++ uses_lwt ()
           )
           (fun ~input_buffer_size max_read_bytes input_files ->
             rset_folding
               ~fold_operation:Biocaml_rSet.inter
               ~fold_init:(Biocaml_rSet.of_range_list [min_int, max_int])
               ~input_buffer_size max_read_bytes input_files)
        );
      ])
      
end


    

let cmd_info =
  Command_line.(
    basic ~summary:"Get information about files"
      Spec.(
        anon (sequence "FILES" string)
        ++ uses_lwt ()
      )
      (fun files ->
        let f s =
          Lwt_io.printf "File: %S\n" s
          >>= fun () ->
          begin match Biocaml_tags.guess_from_filename s with
          | Ok tags ->
            Lwt_io.printf "  Inferred Tags: %s\n"
              (Biocaml_tags.sexp_of_t tags |! Sexp.to_string_hum)
          | Error e ->
            Lwt_io.printf "  Cannot retrieve tags: %s\n"
              begin match e with
              | `extension_absent -> "no extension"
              | `extension_unknown s -> sprintf "unknown extension: %S" s
              end
          end
        in
        (* List.fold files ~init:(return ()) ~f:(fun m v -> m >>= fun () -> f v)) *)
        Lwt_list.iter_s f files)
    )

  
let () =
  Command_line.(
    let whole_thing =
      group ~summary:"Biocaml's command-line application" [
        ("bed", Bed_operations.command);
        ("bam", Bam_conversion.command);
        ("entrez", Entrez.command);
        ("info", cmd_info);
      ] in
    run whole_thing;
    List.iter !lwts_to_run Lwt_main.run
  );

