
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
  include  Command


  let lwts_to_run = ref ([]: unit Lwt.t list)
  let uses_lwt () =
    Spec.step (fun lwt -> lwts_to_run := lwt :: !lwts_to_run)


  let input_buffer_size_flag () =
    Spec.(
      step (fun k v -> k ~input_buffer_size:v)
      +> flag "input-buffer" ~aliases:["ib"] (optional_with_default 42_000 int)
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
      +> flag "verbose-all" ~aliases:["V"] no_arg ~doc:" make everything over-verbose"
      ++ step (fun k v -> if v then Biocaml_internal_pervasives.Debug.enable "BAM"; k)
      +> flag "verbose-bam"  no_arg ~doc:" make Biocaml_bam verbose"
      ++ step (fun k v -> if v then Biocaml_internal_pervasives.Debug.enable "SAM"; k)
      +> flag "verbose-sam"  no_arg ~doc:" make Biocaml_sam verbose"
      ++ step (fun k v -> if v then Biocaml_internal_pervasives.Debug.enable "ZIP"; k)
      +> flag "verbose-zip"  no_arg ~doc:" make Biocaml_zip verbose"
      ++ step (fun k v ->  if v then verbose := true; k)
      +> flag "verbose-app"  no_arg ~doc:" make 'biocaml' itself verbose"
    )

  let file_to_file_flags () =
    Spec.(
      verbosity_flags ()
      ++ input_buffer_size_flag ()
      ++ step (fun k v -> k ~output_buffer_size:v)
      +> flag "output-buffer" ~aliases:["ob"] (optional_with_default 42_000 int)
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
            (Biocaml_sam.Error.sexp_of_string_to_raw s |! Sexp.to_string_hum)
        | `output (Error (`unzip s)) ->
          Lwt_io.eprintf "=====  ERROR: %s\n%!"
            (Biocaml_zip.Transform.sexp_of_unzip_error s |! Sexp.to_string_hum)
        | `output (Error (`bam_to_item s)) ->
          Lwt_io.eprintf "=====  ERROR: %s\n%!"
            (Biocaml_bam.Transform.sexp_of_raw_to_item_error s |! Sexp.to_string_hum)
        | `output (Error (`sam_to_item s)) ->
          Lwt_io.eprintf "=====  ERROR: %s\n%!"
            (Biocaml_sam.Error.sexp_of_raw_to_item s |! Sexp.to_string_hum)
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

let pull_next ~in_channel ~transform =
  let rec loop () =
    match Biocaml_transform.next transform with
    | `output o -> return (Some o)
    | `end_of_stream -> return None
    | `not_ready ->
      Lwt_io.(read ~count:(buffer_size in_channel) in_channel)
      >>= fun read_string ->
      if read_string = ""
      then begin
        Biocaml_transform.stop transform;
        loop ()
      end
      else begin
        Biocaml_transform.feed transform read_string;
        loop ()
      end
  in
  loop ()

let flush_transform ~out_channel ~transform =
  let rec loop () =
    match Biocaml_transform.next transform with
    | `output o ->
      Lwt_io.(write out_channel o)
      >>= fun () ->
      loop ()
    | `end_of_stream ->
      return ()
    | `not_ready -> return ()
  in
  loop ()

let push_to_the_max ~out_channel ~transform input =
  Biocaml_transform.feed transform input;
  flush_transform ~out_channel ~transform

    
  


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
  module Fetch = struct
    type 'a fetched = 'a Lwt.t

    let fetch url f =
      Http_method.discover () >>= fun http ->
      http url >|= f

    let ( >>= ) = ( >>= )
    let ( >|= ) = ( >|= )
  end

  module Entrez = Biocaml_entrez.Make(Fetch)

  let pubmed s =
    Entrez.PubmedSummary.search (String.concat ~sep:" " s) >>= fun result ->
    Lwt_io.printf "Result:\n" >>= fun () ->
    Lwt_list.iter_s 
      (fun { Entrez.PubmedSummary.pmid ; title ; doi } -> 
        Lwt_io.printf "* ID: %d\n\tTitle: %s\n%s" 
          pmid title 
          Option.(value_map doi ~default:"" ~f:(sprintf "\tDOI: %s\n")))
      result

  let gene s =
    let open Entrez in
    Gene.search (String.concat ~sep:" " s) >>= fun result ->
    Lwt_io.printf "Result:\n" >>= fun () ->
    Lwt_list.iter_s 
      (fun { Gene.summary ; gene } -> 
        Lwt_io.printf "* Gene: %s\n\tIdentifiers: %s\n%s"
          Option.(value gene.Gene_ref.locus ~default:"")
          (String.concat 
             ~sep:", " 
             (List.map 
                gene.Gene_ref.db
                (fun db -> sprintf "%s:%s" db.Dbtag.db (Object_id.to_string db.Dbtag.tag))))
          Option.(value_map summary ~default:"" ~f:(sprintf "\tSummary: %s\n")))
      result

  let command =
    Command_line.(
      group ~summary:"Query the Entrez/EUtils database" [
        ("pubmed",
         basic
           ~summary:"Test a simple query in pubmed journals"
           Spec.(
             verbosity_flags ()
             +> anon (sequence ("SEARCH" %: string))
             ++ uses_lwt ()
           )
           pubmed) ;
        ("gene",
         basic
           ~summary:"Test a simple query in Entrez Gene"
           Spec.(
             verbosity_flags ()
             +> anon (sequence ("SEARCH" %: string))
             ++ uses_lwt ()
           )
           gene) ;
      ])

end


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
          bam_to_sam ~input_buffer_size bam ~output_buffer_size sam))

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
          +> flag "stop-after" (optional int)
            ~doc:"<n> Stop after reading <n> bytes"
          +> anon ("SAM-or-BAM-FILE" %: string)
          +> anon ("WIG-FILE" %: string)
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
             load ~input_buffer_size ?max_read_bytes ~on_output input_file
             >>= fun () ->
             intersects !map_ref  name start stop));
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
             +> flag "stop-after" (optional int)
               ~doc:"<n> Stop after reading <n> bytes"
             +> anon (sequence ("BED-ish-FILES" %: string))
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
             +> flag "stop-after" (optional int)
               ~doc:"<n> Stop after reading <n> bytes"
             +> anon (sequence ("BED-ish-FILES" %: string))
             ++ uses_lwt ()
           )
           (fun ~input_buffer_size max_read_bytes input_files ->
             rset_folding
               ~fold_operation:Biocaml_rSet.inter
               ~fold_init:(Biocaml_rSet.of_range_list [Int.min_value, Int.max_value])
               ~input_buffer_size max_read_bytes input_files)
        );
      ])

end


module Demultiplexer = struct

  type barcode_specification = {
    barcode: string;
    position: int;
    on_read: int;
    mismatch: int option;
  }
  let barcode_specification ?mismatch ~position ~on_read barcode =
    Blang.base { mismatch; barcode; position; on_read }
  let barcode_specification_of_sexp =
    let open Sexp in
    function
    | Atom all_in_one as sexp ->
      begin match String.split ~on:':' all_in_one with
      | barcode :: on_read_str :: position_str :: rest ->
        let on_read = Int.of_string on_read_str in
        let position = Int.of_string position_str in
        let mismatch =
          match rest with [] -> None | h :: _ -> Some (Int.of_string h) in
        { mismatch; barcode; position; on_read}
        | _ -> of_sexp_error (sprintf "wrong barcode spec") sexp
      end
    | List (Atom barcode :: Atom "on" :: Atom "read" :: Atom on_read_str
            :: Atom "at" :: Atom "position" :: Atom position_str
            :: more_or_not) as sexp ->
      let on_read = Int.of_string on_read_str in
      let position = Int.of_string position_str in
      let mismatch =
        match more_or_not with
        | [ Atom "with"; Atom "mismatch"; Atom mm ] -> Some (Int.of_string mm) 
        | [] -> None
        | _ -> of_sexp_error (sprintf "wrong barcode spec") sexp
      in
      { mismatch; barcode; position; on_read}
    | sexp -> of_sexp_error (sprintf "wrong barcode spec") sexp

  let sexp_of_barcode_specification { mismatch; barcode; position; on_read} =
    let open Sexp in
    let more =
      match mismatch with
      | None -> []
      | Some mm -> [ Atom "with"; Atom "mismatch"; Atom (Int.to_string mm) ] in
    List (Atom barcode
          :: Atom "on" :: Atom "read" :: Atom (Int.to_string on_read)
          :: Atom "at" :: Atom "position" :: Atom (Int.to_string position)
          :: more)
      

  type library = {
    name_prefix: string;
    barcoding: barcode_specification Blang.t;
    (* Disjunctive normal form: or_list (and_list barcode_specs))*)
  }

  let library_of_sexp =
    let open Sexp in
    function
    | List [Atom "library"; Atom name_prefix; sexp] ->
      { name_prefix;
        barcoding = Blang.t_of_sexp barcode_specification_of_sexp  sexp }
    | sexp ->
      of_sexp_error (sprintf "wrong library") sexp

  let sexp_of_library {name_prefix; barcoding} =
    let open Sexp in
    let rest = Blang.sexp_of_t sexp_of_barcode_specification barcoding in
    List [Atom "library"; Atom name_prefix; rest]
        
  type demux_specification = library list
  let demux_specification_of_sexp =
    let open Sexp in
    function
    | List (Atom "demux" :: rest) ->
      List.map rest library_of_sexp
    | List [] -> []
    | sexp -> of_sexp_error (sprintf "wrong sexp") sexp

  let sexp_of_demux_specification l =
    let open Sexp in
    List (Atom "demux" :: List.map l sexp_of_library)
    

  let join_pair t1 t2 =
    Lwt_list.map_p ident [t1; t2]
    >>= begin function
    | [ r1; r2] -> return (r1, r2)
    | _ -> failf "join_pair did not return 2 elements"
    end

  let map_list_parallel l ~f = Lwt_list.map_p f l

  let map_list_parallel_with_index l ~f =
    let c = ref (-1) in
    map_list_parallel l ~f:(fun x -> incr c; f !c x)
      
  let check_barcode ~mismatch ~position ~barcode sequence =
    let allowed_mismatch = ref mismatch in
    let index = ref 0 in
    while !allowed_mismatch >= 0 && !index <= String.length barcode - 1 do
      if sequence.[position - 1 + !index] <> barcode.[!index]
      then decr allowed_mismatch;
      incr index
    done;
    (!allowed_mismatch >= 0, mismatch - !allowed_mismatch)
      
  let string_of_error e =
    let module M = struct
      type t = [ Biocaml_fastq.Error.t
               | `unzip of Biocaml_zip.Transform.unzip_error ]
      with sexp
    end in
    Sexp.to_string_hum (M.sexp_of_t e)
    
  type library_statistics = {
    mutable read_count: int;
    mutable no_mismatch_read_count: int;
  }
  let library_statistics () =
    { read_count = 0; no_mismatch_read_count = 0; }

  let perform ~mismatch ?gzip_output ?do_statistics
      ~input_buffer_size ~read_files
      ~output_buffer_size ~demux_specification =

    map_list_parallel read_files (fun filename ->
      Lwt_io.(open_file ~mode:input ~buffer_size:input_buffer_size filename)
      >>= fun inp ->
      let transform =
        match Biocaml_tags.guess_from_filename filename with
        | Ok (`gzip `fastq) | _ when String.is_suffix filename ".gz" ->
          Biocaml_transform.compose_results
            ~on_error:(function `left l -> `unzip l | `right r -> r)
            (Biocaml_zip.Transform.unzip
               ~zlib_buffer_size:(3 * input_buffer_size) ~format:`gzip ())
            (Biocaml_fastq.Transform.string_to_item ~filename ())
        | _ -> 
          (Biocaml_fastq.Transform.string_to_item ~filename ())
      in
      return (transform, inp))
    >>= fun transform_inputs ->
    
    map_list_parallel demux_specification (fun {name_prefix; barcoding} ->
      map_list_parallel_with_index read_files (fun i _ ->
        let actual_filename, transform =
          match gzip_output with
          | None ->
            (sprintf "%s_R%d.fastq" name_prefix (i + 1),
             Biocaml_fastq.Transform.item_to_string ())
          | Some level ->
            (sprintf "%s_R%d.fastq.gz" name_prefix (i + 1),
             Biocaml_transform.compose
               (Biocaml_fastq.Transform.item_to_string ())
               (Biocaml_zip.Transform.zip ~format:`gzip ~level
                  ~zlib_buffer_size:output_buffer_size ()))
        in 
        Lwt_io.(open_file ~mode:output ~buffer_size:output_buffer_size
                  actual_filename)
        >>= fun o ->
        return (transform, o))
      >>= fun outs ->
      return (name_prefix, outs, barcoding,
              Option.map do_statistics (fun _ -> library_statistics ())))
    >>= fun output_specs ->

    let rec loop () =
      map_list_parallel transform_inputs (fun (transform, in_channel) ->
        pull_next ~transform  ~in_channel)
      >>= fun all_the_nexts ->
      if List.for_all all_the_nexts ((=) None)
      then return ()
      else begin
        map_list_parallel_with_index all_the_nexts (fun i -> function
        | Some (Ok item) -> return item
        | Some (Error e) ->
          failf "error while parsing read %d: %s" (i + 1) (string_of_error e)
        | None -> failf "read %d is not long enough" (i + 1))
        >>= fun items ->
        map_list_parallel output_specs (fun (_, outs, spec, stats_opt) ->
          let matches, max_mismatch = 
            let default_mismatch = mismatch in
            let max_mismatch = ref 0 in
            let matches =
              Blang.eval spec (fun {on_read; barcode; position; mismatch} ->
                let mismatch =
                  Option.value ~default:default_mismatch mismatch in
                try
                  let item = List.nth_exn items (on_read - 1) in
                  let matches, mm =
                    check_barcode ~position ~barcode ~mismatch
                      item.Biocaml_fastq.sequence in
                  if matches then max_mismatch := max !max_mismatch mm;
                  matches
                with e -> false) in
            matches, !max_mismatch
          in
          if matches then begin
            Option.iter stats_opt (fun s ->
              s.read_count <- s.read_count + 1;
              if max_mismatch = 0 then
                s.no_mismatch_read_count <- s.no_mismatch_read_count + 1;
            );
            List.fold2_exn outs items ~init:(return ())
              ~f:(fun m (transform, out_channel) item ->
                m >>= fun () ->
                push_to_the_max ~transform ~out_channel item)
          end
          else
            return ())
        >>= fun _ ->
        loop ()
      end
    in
    loop () >>= fun () ->
    begin match do_statistics with
    | Some s -> 
      let open Lwt_io in
      open_file ~mode:output ~buffer_size:output_buffer_size s >>= fun o ->
      fprintf o ";; library_name read_count 0_mismatch_read_count\n"
      >>= fun () ->
      return (Some o)
    | None -> return None
    end
    >>= fun stats_channel ->
    map_list_parallel output_specs (fun (name, os, spec, stats) ->
      map_list_parallel os ~f:(fun (transform, out_channel) ->
        Biocaml_transform.stop transform;
        flush_transform ~out_channel ~transform >>= fun () ->
        Lwt_io.close out_channel)
      >>= fun _ ->
      begin match stats, stats_channel with
      | Some { read_count; no_mismatch_read_count }, Some o ->
        Lwt_io.fprintf o "(%S %d %d)\n" name
          read_count no_mismatch_read_count
      | _, _ -> return ()
      end
    )
    >>= fun _ ->
    map_list_parallel transform_inputs (fun (_, i) -> Lwt_io.close i)
    >>= fun _ ->
    Option.value_map stats_channel ~default:(return ()) ~f:Lwt_io.close 

  let parse_configuration s =
    let open Sexp in
    let sexp = ksprintf of_string "(\n%s\n)" s in
    let entries =
      match sexp with List entries -> entries | _ -> assert false in
    let mismatch =
      List.find_map entries (function
      | List [Atom "default-mismatch"; Atom vs] ->
        Some (Int.of_string vs)
      | _ -> None) in
    let undetermined = 
      List.find_map entries (function
      | List [Atom "undetermined"; Atom vs] ->
        Some vs
      | _ -> None) in
    let gzip = 
      List.find_map entries (function
      | List [Atom "gzip-output"; Atom vs] ->
        Some (Int.of_string vs)
      | _ -> None) in
    let demux =
      List.find_map entries (function
      | List (Atom "demux" :: _) as s ->
        Some (demux_specification_of_sexp s)
      | _ -> None) in
    let stats =
      List.find_map entries (function
      | List [Atom "statistics"; Atom vs] ->
        Some vs
      | _ -> None) in
    let inputs =
      List.find_map entries (function
      | List (Atom "input" :: files) ->
        Some (List.map files (function
        | Atom a -> a
        | s ->
          failwithf "wrong input files specification: %s" (to_string_hum s) ()))
      | _ -> None) in
    (mismatch, gzip, undetermined, stats, demux, inputs)
    
  let command =
    Command_line.(
      basic ~summary:"Fastq deumltiplexer"
        ~readme:begin fun () ->
          let open Blang in
          let ex v =
            (Sexp.to_string_hum (sexp_of_demux_specification v)) in
          String.concat ~sep:"\n\n" [
            "** Examples of S-Expressions:";
            sprintf "An empty one:\n%s" (ex []);
            sprintf "Two Illumina-style libraries (the index is read n°2):\n%s"
              (ex [
                { name_prefix = "LibONE";
                  barcoding = 
                    barcode_specification ~position:1 "ACTGTT"
                      ~mismatch:1 ~on_read:2 };
                { name_prefix = "LibTWO";
                  barcoding = 
                    barcode_specification ~position:1 "CTTATG"
                      ~mismatch:1 ~on_read:2 };
              ]);
            sprintf "A library with two barcodes to match:\n%s"
              (ex [
                { name_prefix = "LibAND";
                  barcoding =
                    and_ [
                      barcode_specification ~position:5 "ACTGTT"
                        ~mismatch:1 ~on_read:1;
                      barcode_specification ~position:1 "TTGT"
                        ~on_read:2;
                    ]}
              ]);
            sprintf "A merge of two barcodes into one “library”:\n%s"
              (ex [
                { name_prefix = "LibOR";
                  barcoding = or_ [
                    barcode_specification ~position:5 "ACTGTT"
                      ~mismatch:1 ~on_read:1;
                    barcode_specification ~position:1 "TTGT" ~on_read:2; 
                  ] }]);
            begin
              let example =
                "(demux\n\
                \  (library \"Lib with ånnœ¥ing name\" ACCCT:1:2) \
                    ;; one barcode on R1, at pos 2\n\
                \  (library GetALL true) ;; get everything\n\
                \  (library Merge (and AGTT:2:42:1 ACCC:2:42:1 \n\
                \                   (not AGTGGTC:1:1:2)) \
                ;; a ∧ b ∧ ¬c  matching\n\
                ))" in
              let spec =
                Sexp.of_string example |! demux_specification_of_sexp in
              sprintf "This one:\n%s\nis equivalent to:\n%s" example (ex spec)
            end;
          ]
        end
        Spec.(
          file_to_file_flags ()
          +> flag "default-mismatch" (optional int)
            ~doc:"<int> default maximal mismatch allowed (default 0)"
          +> flag "gzip-output" ~aliases:["gz"] (optional int)
            ~doc:"<level> output GZip files (compression level: <level>)"
          +> flag "demux" (optional string)
            ~doc:"<string> give the specification as a list of S-Expressions"
          +> flag "specification" ~aliases:["spec"] (optional string)
            ~doc:"<file> give a path to a file containing the specification"
          +> flag "undetermined" (optional string)
            ~doc:"<name> put all the non-matched reads in a library"
          +> flag "statistics" ~aliases:["stats"] (optional string)
            ~doc:"<file> do some basic statistics and write them to <file>"
          +> anon (sequence ("READ-FILES" %: string))
          ++ uses_lwt ())
        begin fun ~input_buffer_size ~output_buffer_size
          mismatch_cl gzip_cl demux_cl spec undetermined_cl stats_cl
          read_files_cl ->
            begin match spec with
            | Some s ->
              Lwt_io.(with_file ~mode:input s (fun i -> read i))
              >|= parse_configuration
            | None -> return (None, None, None, None, None, None)
            end
            >>= fun (mismatch, gzip, undetermined, stats, demux, inputs) ->
            begin match read_files_cl, inputs with
            | [], Some l -> return l
            | l, None -> return l
            | l, Some ll ->
              failf "conflict: input files defined in command line \
                     and configuration file"
            end
            >>= fun read_files ->
            let mismatch =
              match mismatch_cl with
              | Some s -> s
              | None -> match mismatch with Some s -> s | None -> 0 in
            let gzip_output = if gzip_cl <> None then gzip_cl else gzip in
            let undetermined =
              if undetermined_cl <> None then undetermined_cl else undetermined
            in
            let do_statistics = if stats_cl <> None then stats_cl else stats in
            let demux_spec_from_cl =
              Option.map demux_cl ~f:(fun s ->
                Sexp.of_string (sprintf "(demux %s)" s)
                |! demux_specification_of_sexp) in
            let demux =
              if demux_spec_from_cl <> None then demux_spec_from_cl
              else demux in
            let demux_specification =
              let default = Option.value ~default:[] demux in
              Option.value_map undetermined ~default
                ~f:(fun name_prefix ->
                  let open Blang in
                  { name_prefix; 
                    barcoding =
                      not_ (or_ (List.map default (fun l -> l.barcoding))) }
                  :: default)
            in
            perform ~mismatch ?gzip_output ?do_statistics
              ~input_buffer_size ~read_files ~output_buffer_size
              ~demux_specification
        end)

      

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
        ("demux", Demultiplexer.command);
        ("info", cmd_info);
      ] in
    run whole_thing;
    List.iter !lwts_to_run Lwt_main.run
  );

