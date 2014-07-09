
(*
open Core.Std
open Flow
open Biocaml_app_common
open Biocaml




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
        (Biocaml_sam_deprecated.Transform.string_to_raw ())
        (Biocaml_sam_deprecated.Transform.raw_to_item ()))
  | `gzip `sam ->
    return (
      Biocaml_transform.compose_results
        ~on_error:(function `left l -> `unzip l | `right r -> r)
        (Biocaml_zip.Transform.unzip
           ~zlib_buffer_size:(10 * input_buffer_size)
           ~format:`gzip ())
        (Biocaml_transform.compose_results
           ~on_error:(function `left l -> `sam l | `right r -> `sam_to_item r)
           (Biocaml_sam_deprecated.Transform.string_to_raw ())
           (Biocaml_sam_deprecated.Transform.raw_to_item ())))
  | _ ->
    failf "cannot handle file format"
  end
  >>= fun transfo ->
  let open With_set in
  let tree = create () in
  let transform =
    Biocaml_transform.on_output transfo ~f:(function
    | Ok (`alignment al) ->
      let open Biocaml_sam_deprecated in
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
  go_through_input ~transform ~max_read_bytes bamfile
  >>= fun () ->
  let bed_set = bed_set tree in
  Lwt_io.(
    with_file ~mode:output
      ~buffer_size:output_buffer_size wigfile (fun o ->
        Bed_set.fold bed_set ~init:(return ()) ~f:(fun prev (chr, b, e, f) ->
          prev >>= fun () ->
          wrap_deferred_lwt (fun () -> fprintf o "%s %d %d %g\n" chr b e f))
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
      (fun max_read_bytes input_file wig ->
        build_wig  ?max_read_bytes input_file wig
        >>< common_error_to_string)
  )

let command =
  Command_line.(
    group ~summary:"Count aligned base-pairs from BAM/SAM files (potentially gzipped)" [
      ("extract-wig", cmd_extract_wig);
    ])

  *)
