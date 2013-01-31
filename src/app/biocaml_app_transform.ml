
open Core.Std
open Flow
open Biocaml_app_common
open Biocaml


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

type input_error =
[ `bam of Biocaml_bam.Transform.raw_bam_error
| `bam_to_item of [ Biocaml_bam.Transform.raw_to_item_error ]
| `sam of [ Biocaml_sam.Error.string_to_raw ]
| `sam_to_item of [ Biocaml_sam.Error.raw_to_item ]
| `unzip of Biocaml_zip.Transform.unzip_error
| `gff of Gff.parse_error
| `wig of Wig.parse_error
| `bed of Bed.parse_error
| `fastq of Fastq.Error.t
| `fasta of Fasta.Error.t
]
with sexp_of

type input_transform = [
| `from_sam_item of (string, (Sam.item, input_error) Result.t) Transform.t
| `from_gff of(string, (Gff.stream_item, input_error) Result.t) Transform.t
| `from_wig of (string, (Wig.t, input_error) Result.t) Transform.t
| `from_bed of (string, (Bed.t, input_error) Result.t) Transform.t
| `from_fastq of (string, (Fastq.item, input_error) Result.t) Transform.t
| `from_char_fasta of
    (string, (Fasta.char_seq Fasta.Transform.raw_item, input_error) Result.t) Transform.t
| `from_int_fasta of
    (string, (Fasta.int_seq Fasta.Transform.raw_item, input_error) Result.t) Transform.t
]
let input_transform_name = function
| `from_sam_item _ -> "from_sam_item"
| `from_gff _ -> "from_gff"
| `from_wig _ -> "from_wig"
| `from_bed _ -> "from_bed"
| `from_fastq _ -> "from_fastq"
| `from_char_fasta _ -> "from_char_fasta"
| `from_int_fasta _ -> "from_int_fasta"


type output_error =
[ `bam of Biocaml_bam.Transform.item_to_raw_error
| `sam of Biocaml_sam.Error.item_to_raw 
]
with sexp_of
type output_transform = [
| `to_sam_item of (Sam.item, (string, output_error) Result.t) Transform.t
| `to_gff of(Gff.stream_item, string) Transform.t
| `to_wig of (Wig.t, string) Transform.t
| `to_bed of (Bed.t, string) Transform.t
| `to_fastq of (Fastq.item, string) Transform.t
| `to_char_fasta of (Fasta.char_seq Fasta.Transform.raw_item, string) Transform.t
| `to_int_fasta of (Fasta.int_seq Fasta.Transform.raw_item, string) Transform.t
]
let output_transform_name = function
| `to_sam_item _ -> "to_sam_item"
| `to_gff _ -> "to_gff"
| `to_wig _ -> "to_wig"
| `to_bed _ -> "to_bed"
| `to_fastq _ -> "to_fastq"
| `to_char_fasta _ -> "to_char_fasta"
| `to_int_fasta _ -> "to_int_fasta"
    
let rec input_transform ?with_unzip ~zlib_buffer_size input_tags =
  let with_unzip t =
    match with_unzip with
    | Some z ->
      Transform.compose_results
        ~on_error:(function `left l -> `unzip l | `right r -> r)
        z t
    | None -> t
  in
  let from_sam_item t = return (`from_sam_item (with_unzip t) : input_transform) in
  match input_tags with
  | `raw_zip tags ->
    input_transform
      ~with_unzip:(Zip.Transform.unzip ~zlib_buffer_size ~format:`raw ())
      ~zlib_buffer_size tags
  | `gzip tags ->
    input_transform
      ~with_unzip:(Zip.Transform.unzip ~zlib_buffer_size ~format:`gzip ())
      ~zlib_buffer_size tags
  | `bam ->
    from_sam_item (
      Transform.compose_results
        ~on_error:(function `left l -> l | `right r -> `bam_to_item r)
        (Bam.Transform.string_to_raw ~zlib_buffer_size ())
        (Bam.Transform.raw_to_item ()))
  | `sam ->
    from_sam_item (
      Transform.compose_results
        ~on_error:(function `left l -> `sam l | `right r -> `sam_to_item r)
        (Sam.Transform.string_to_raw ())
        (Sam.Transform.raw_to_item ()))
  | `gff gff_tag_list ->
    let t =
      Transform.on_output  
        (Gff.Transform.string_to_item ~tags:gff_tag_list ())
        (function Ok o -> Ok o | Error e -> Error (`gff e))
    in
    return (`from_gff (with_unzip t) : input_transform)
  | `wig wig_tag_list ->
    let t =
      Transform.on_output  
        (Wig.Transform.string_to_t ~tags:wig_tag_list ())
        (function Ok o -> Ok o | Error e -> Error (`wig e))
    in
    return (`from_wig (with_unzip t) : input_transform)
  | `bed ->
    let t =
      Transform.on_output  
        (Bed.Transform.string_to_t ())
        (function Ok o -> Ok o | Error e -> Error (`bed e))
    in
    return (`from_bed (with_unzip t) : input_transform)
  | `fastq ->
    let t =
      Transform.on_output  
        (Fastq.Transform.string_to_item ())
        (function Ok o -> Ok o | Error e -> Error (`fastq e))
    in
    return (`from_fastq (with_unzip t) : input_transform)
  | `fasta `unknown
  | `fasta `char ->
    (* TODO output warning? if `unknown *)
    let t =
      Transform.on_output  
        (Fasta.Transform.string_to_char_seq_raw_item ())
        (function Ok o -> Ok o | Error e -> Error (`fasta e))
    in
    return (`from_char_fasta (with_unzip t) : input_transform)
  | `fasta `int ->
    let t =
      Transform.on_output  
        (Fasta.Transform.string_to_int_seq_raw_item ())
        (function Ok o -> Ok o | Error e -> Error (`fasta e))
    in
    return (`from_int_fasta (with_unzip t) : input_transform)

let rec output_transform ?with_zip ~zlib_buffer_size output_tags =
  let with_zip_result t =
    match with_zip with
    | Some z -> Transform.compose_result_left t z
    | None -> t
  in
  let with_zip_no_error t =
    match with_zip with
    | Some z -> Transform.compose t z 
    | None -> t
  in
  let to_sam_item t = return (`to_sam_item (with_zip_result t) : output_transform) in
  match output_tags with
  | `raw_zip tags ->
    output_transform
      ~with_zip:(Zip.Transform.zip ~zlib_buffer_size ~format:`raw ())
      ~zlib_buffer_size tags
  | `gzip tags ->
    output_transform
      ~with_zip:(Zip.Transform.zip ~zlib_buffer_size ~format:`gzip ())
      ~zlib_buffer_size tags
  | `bam ->
    to_sam_item (
      Transform.compose_result_left
        (Transform.on_output 
           (Bam.Transform.item_to_raw ())
           (function Ok o -> Ok o | Error e -> Error (`bam e)))
        (Bam.Transform.raw_to_string ~zlib_buffer_size ()))
  | `sam ->
    to_sam_item (
      Transform.compose_result_left
        (Transform.on_output 
           (Sam.Transform.item_to_raw ())
           (function Ok o -> Ok o | Error e -> Error (`sam e)))
        (Sam.Transform.raw_to_string ()))
  | `gff tag_list ->
    let t = Gff.Transform.item_to_string ~tags:tag_list () in
    return (`to_gff (with_zip_no_error t) : output_transform)
  | `wig tag_list ->
    let t = Wig.Transform.t_to_string  ~tags:tag_list () in
    return (`to_wig (with_zip_no_error t) : output_transform)
  | `bed ->
    let t = Bed.Transform.t_to_string  () in
    return (`to_bed (with_zip_no_error t) : output_transform)
  | `fastq ->
    let t = Fastq.Transform.item_to_string () in
    return (`to_fastq (with_zip_no_error t) : output_transform)
  | `fasta `unknown
  | `fasta `char ->
    (* TODO output warning? if `unknown *)
    let t = Fasta.Transform.char_seq_raw_item_to_string () in
    return (`to_char_fasta (with_zip_no_error t) : output_transform)
  | `fasta `int ->
    let t = Fasta.Transform.int_seq_raw_item_to_string () in
    return (`to_int_fasta (with_zip_no_error t) : output_transform)

let parse_tags s =
  try return (Tags.t_of_sexp (Sexp.of_string s))
  with e -> error (`parse_tags e)
let tags_to_string t = Tags.sexp_of_t t |! Sexp.to_string_hum
  
let run_transform  ~input_buffer_size ~output_buffer_size ~output_tags files =
  begin
    let zlib_buffer_size = 2 * input_buffer_size in
    while_sequential files (fun file_optionally_tagged ->
      match String.lsplit2 ~on:':' file_optionally_tagged with
      | None ->
        of_result (Tags.guess_from_filename file_optionally_tagged)
        >>= fun tags ->
        input_transform ~zlib_buffer_size tags
        >>= fun meta_transform ->
        return (file_optionally_tagged, tags, meta_transform)
      | Some (one, two) ->
        parse_tags two
        >>= fun tags ->
        input_transform ~zlib_buffer_size tags
        >>= fun meta_transform ->
        return (one, tags, meta_transform))
    >>= fun input_files_tags_and_transforms ->
    parse_tags output_tags
    >>= fun tags ->
    output_transform ~zlib_buffer_size:(output_buffer_size) tags
    >>= fun meta_output_transform ->
    while_sequential input_files_tags_and_transforms (fun (filename, tags, tr) ->
      dbg "Convert %s (%s) %s %s"
        filename (tags_to_string tags) (input_transform_name tr)
        (output_transform_name meta_output_transform)
    )
    >>= fun _ ->
    
    return ()
  end
  >>< begin function
  | Ok () -> return ()
  | Error e ->
    error (
      <:sexp_of< [ `extension_absent
                 | `extension_unknown of string
                 | `parse_tags of exn] >> e
      |! Sexp.to_string_hum)
  end
             

let command =
  let open Command_line in
  let spec =
    let open Spec in
    file_to_file_flags ()
    ++ step (fun k v -> k ~output_tags:v)
    +> flag "output-tags" ~aliases:["to"] (required string)
      ~doc:"<string> give the specification of the output"
    +> anon (sequence ("INPUT-FILE:tag-definition" %: string))
    ++ uses_lwt ()
  in
  basic ~summary:"Transform files" spec run_transform
