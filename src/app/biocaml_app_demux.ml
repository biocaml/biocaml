open Core.Std
open Flow
open Biocaml
open Biocaml_app_common


(** A [barcode_specification] is a matching rule for a single barcode
   (a library can have more than one barcode). *)
type barcode_specification = {
  barcode: string;
  position: int;
  on_read: int;
  (** [mismatch]: [None] means “leave decision to application defaults” *)
  mismatch: int option;
}

(** Create a [barcode_specification]. *)
let barcode_specification ?mismatch ~position ~on_read barcode =
  Blang.base { mismatch; barcode; position; on_read }

(** Parse an S-Expression into a [barcode_specification]. *)
let barcode_specification_of_sexp : Sexp.t -> barcode_specification =
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

(** Serialize a [barcode_specification] to an S-Expression (as
   human-readable as possible). *)
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

(** We define a [library] as a “prefix” (to output files
   [prefix_{R1,R2,R3}]), and a matching rule. The later can be either
   [`undetermined], which means “catch” all the non-matched reads, or
   [`barcoding expr], where [expr] is a boolean expression where the
   atoms are [barcode_specification] rules.  *)
type library = {
  name_prefix: string;
  filter: [
    | `barcoding of barcode_specification Blang.t
    | `undetermined
  ]
}

(** Parse an S-Expression into a [library] specification. *)
let library_of_sexp =
  let open Sexp in
  function
  | List [Atom "library"; Atom name_prefix; List [Atom "barcoding"; sexp]] ->
    { name_prefix;
      filter =
        `barcoding (Blang.t_of_sexp barcode_specification_of_sexp  sexp) }
  | List [Atom "library"; Atom name_prefix; Atom "undetermined"] ->
    { name_prefix; filter = `undetermined }
  | sexp ->
    of_sexp_error (sprintf "wrong library") sexp

(** Serialize a [library] into an [SExp.t]. *)
let sexp_of_library {name_prefix; filter} =
  let open Sexp in
  let rest =
    match filter with
    | `barcoding barcoding ->
      List [Atom "barcoding";
            Blang.sexp_of_t sexp_of_barcode_specification barcoding]
    | `undetermined -> Atom "undetermined" in
  List [Atom "library"; Atom name_prefix; rest]

(** The specification of a whole demultiplexing is a list of library
   matching specifications, and global matching parameters. For now the
   only parameter is the [demux_policy] which dictates how are handled
   libraries that match more than one rule. *)
type demux_specification = {
  demux_rules: library list;
  demux_policy: [ `exclusive | `inclusive ];
}

(** Parse an S-Expression into a [demux_specification]. *)
let demux_specification_of_sexp =
  let open Sexp in
  function
  | List (Atom "demux" :: rest) ->
    dbgi "demux";
    let demux_rules = List.map rest library_of_sexp in
    dbgi "demux_rules";
    { demux_rules; demux_policy = `inclusive }
  | List (Atom "exclusive-demux" :: rest) ->
    let demux_rules = List.map rest library_of_sexp in
    { demux_rules; demux_policy = `exclusive; }
  | sexp -> of_sexp_error (sprintf "wrong sexp") sexp

(** Serialize a [demux_specification] into a [Sexp.t]. *)
let sexp_of_demux_specification l =
  let open Sexp in
  match l.demux_policy with
  | `inclusive ->
    List (Atom "demux" :: List.map l.demux_rules sexp_of_library)
  | `exclusive ->
    List (Atom "exclusive-demux" :: List.map l.demux_rules sexp_of_library)

(** [check_barcode] is the function that tells if a
   [barcode_specification] (here exploded into many arguments) matches a
   given read-sequence. If [true], it also returns the mismatch that
   was needed. *)
let check_barcode
    ~mismatch ~position ~barcode
    (sequence: string): bool * int =
  let allowed_mismatch = ref mismatch in
  let index = ref 0 in
  while !allowed_mismatch >= 0 && !index <= String.length barcode - 1 do
    if sequence.[position - 1 + !index] <> barcode.[!index]
    then decr allowed_mismatch;
    incr index
  done;
  (!allowed_mismatch >= 0, mismatch - !allowed_mismatch)

(** Convert FASTQ input errors to strings. *)
let string_of_error e =
  let module M = struct
    type t = [ Biocaml_fastq.Error.t
             | `unzip of Biocaml_zip.Transform.unzip_error ]
    with sexp
  end in
  Sexp.to_string_hum (M.sexp_of_t e)

(** Structure used to record demux-statistics. *)
type library_statistics = {
  mutable read_count: int;
  mutable no_mismatch_read_count: int;
}
let library_statistics () =
  { read_count = 0; no_mismatch_read_count = 0; }

(** Big demultiplexing function. *)
let perform ~mismatch ?do_statistics ~read_files ~demux_specification =

  (* Get some parameters from App_common.Global_configuration *)
  let input_buffer_size = !Global_configuration.input_buffer_size in
  let output_buffer_size = !Global_configuration.output_buffer_size in

  (* Open all the input files and prepare their parsing transforms
     (using the tags to guess if unzipping is needed).

     TODO: also handle pairs of fasta, and non-aligned BAM/SAM content
  *)
  for_concurrent read_files (fun filename ->
    wrap_deferred_lwt (fun () ->
      Lwt_io.(open_file ~mode:input ~buffer_size:input_buffer_size filename))
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
  >>= fun (transform_inputs, errors) ->

  (* Cleanly close all the input files (used for error management). *)
  let close_all_inputs () =
    while_sequential transform_inputs (fun (_, i) ->
      wrap_deferred_lwt (fun () -> Lwt_io.close i))
    >>= fun _ ->
    return ()
  in

  (* Check the errors of the previous for_concurrent *)
  begin match errors with
  | [] -> return ()
  | some ->
    close_all_inputs () >>= fun () ->
    error (`openning_files some)
  end
  >>= fun () ->

  (* For all the output-library-specifications, prepare the output
     channels (one per read of each library!) and the output transforms
     (optionally with GZip). *)
  for_concurrent demux_specification.demux_rules (fun {name_prefix; filter} ->
    for_concurrent_with_index read_files (fun i _ ->
      let actual_filename, transform =
        match Global_configuration.gzip_output_transform () with
        | None ->
          (sprintf "%s_R%d.fastq" name_prefix (i + 1),
           Biocaml_fastq.Transform.item_to_string ())
        | Some zip_transform ->
          (sprintf "%s_R%d.fastq.gz" name_prefix (i + 1),
           Biocaml_transform.compose
             (Biocaml_fastq.Transform.item_to_string ()) zip_transform)
      in
      wrap_deferred_lwt (fun () ->
        Lwt_io.(open_file ~mode:output ~buffer_size:output_buffer_size
                  actual_filename))
      >>= fun o ->
      return (transform, o))
    >>= begin function
    | (outs, []) ->
      return (name_prefix, outs, filter,
              Option.map do_statistics (fun _ -> library_statistics ()))
    | (outs, some_errors) ->
      close_all_inputs ()
      >>= fun () ->
      while_sequential outs (fun (_, o) ->
        wrap_deferred_lwt (fun () -> Lwt_io.close o))
      >>= fun _ ->
      error (`openning_files some_errors)
    end)
  >>= fun (output_specs, errors) ->

  (* Close all inputs *and* outputs. *)
  let close_all () =
    close_all_inputs ()
    >>= fun () ->
    while_sequential output_specs (fun (_, outs, _, _) ->
      while_sequential outs (fun (_, o) ->
        wrap_deferred_lwt (fun () -> Lwt_io.close o)))
  in

  (* Go through a list of errors, and propagate them altogether after
     closing I/O channels. *)
  let check_errors = function
    | [] -> return ()
    | some -> close_all () >>= fun _ -> error (`io_multi_error some)
  in
  check_errors errors
  >>= fun () ->

  (* This defines the main demultiplexing loop. *)
  let rec loop () =
    for_concurrent transform_inputs (fun (transform, in_channel) ->
      pull_next ~transform  ~in_channel)
    >>= fun (all_the_nexts, errors) ->
    check_errors errors
    >>= fun () ->
    if List.for_all all_the_nexts ((=) None)
    then return ()
    else begin
      for_concurrent_with_index all_the_nexts (fun i -> function
      | Some (Ok item) -> return item
      | Some (Error e) ->
        failf "error while parsing read %d: %s" (i + 1) (string_of_error e)
      | None -> failf "read %d is not long enough" (i + 1))
      >>= fun (items, errors) ->
      check_errors errors
      >>= fun () ->
      for_concurrent output_specs (fun (name_prefix, outs, spec, stats_opt) ->
        let matches, max_mismatch =
          let default_mismatch = mismatch in
          let max_mismatch = ref 0 in
          let matches =
            match spec with
            | `barcoding barcoding ->
              Blang.eval barcoding (fun {on_read; barcode; position; mismatch} ->
                  let mismatch =
                    Option.value ~default:default_mismatch mismatch in
                  try
                    let item = List.nth_exn items (on_read - 1) in
                    let matches, mm =
                      check_barcode ~position ~barcode ~mismatch
                        item.Biocaml_fastq.sequence in
                    if matches then max_mismatch := max !max_mismatch mm;
                    matches
                  with e -> false)
            | `undetermined -> false in
          matches, !max_mismatch
        in
        return (name_prefix, matches, max_mismatch, outs, stats_opt))
      >>= fun (match_results, errors) ->
      check_errors errors
      >>= fun () ->
      let matches_also_policy =
        let filtered =
          List.filter_map match_results
            (fun (name, matches, max_mismatch, outs, stats_opt) ->
              if matches then Some (name, max_mismatch, outs, stats_opt) else None)
        in
        let the_undetermined () =
          List.filter_map output_specs ~f:(function
          | (name, outs, `undetermined, stats_opt) -> Some (name, 0, outs, stats_opt)
          | _ -> None) in
        match filtered with
        | [one] -> [one]
        | [] -> the_undetermined ()
        | any_other_number when demux_specification.demux_policy = `exclusive ->
          (* The only difference with the previous case *is* the
             verbose logging: *)
          dbg "demux.exclusive:\n  discarding matches:\n    %s"
            (List.map any_other_number (fun (name_prefix, _, _, _) -> name_prefix)
             |! String.concat ~sep:", ") |> Lwt.ignore_result;
          the_undetermined ()
        | more (* inclusive *) -> more
      in
      for_concurrent matches_also_policy (fun (name, max_mismatch, outs, stats_opt) ->
        Option.iter stats_opt (fun s ->
          s.read_count <- s.read_count + 1;
          if max_mismatch = 0 then
            s.no_mismatch_read_count <- s.no_mismatch_read_count + 1;
        );
        List.fold2_exn outs items ~init:(return ())
          ~f:(fun m (transform, out_channel) item ->
            m >>= fun () ->
            push_to_the_max ~transform ~out_channel item))
      >>= fun (_, errors) ->
      check_errors errors
      >>= fun () ->
      loop ()
    end
  in
  loop () >>= fun () ->

  (* Optionally open the statistics file. *)
  begin match do_statistics with
  | Some s ->
    let open Lwt_io in
    wrap_deferred_lwt (fun () ->
      open_file ~mode:output ~buffer_size:output_buffer_size s) >>= fun o ->
    wrap_deferred_lwt (fun () ->
      fprintf o ";; library_name read_count 0_mismatch_read_count\n")
    >>= fun () ->
    return (Some o)
  | None -> return None
  end
  >>= fun stats_channel ->

  (* Cleanly stop all the transformations, close the out_channels, and
     output the statistics. *)
  for_concurrent output_specs (fun (name, os, spec, stats) ->
    for_concurrent os ~f:(fun (transform, out_channel) ->
      Biocaml_transform.stop transform;
      flush_transform ~out_channel ~transform >>= fun () ->
      wrap_deferred_lwt (fun () -> Lwt_io.close out_channel))
    >>= fun (_, errors) ->
    (* TODO: check errors *)
    begin match stats, stats_channel with
    | Some { read_count; no_mismatch_read_count }, Some o ->
      wrap_deferred_lwt (fun () ->
          Lwt_io.fprintf o "(%S %d %d)\n" name
            read_count no_mismatch_read_count)
    | _, _ -> return ()
    end
  )
  >>= fun _ ->
  for_concurrent transform_inputs (fun (_, i) ->
    wrap_deferred_lwt (fun () -> Lwt_io.close i))
  >>= fun (_, errors) ->
  (* TODO: check errors *)
  Option.value_map stats_channel ~default:(return ())
    ~f:(fun c -> wrap_deferred_lwt (fun () -> Lwt_io.close c))

(** Parse the “global” configuration file (containing parameters that
   can be set on command line *and* the demultiplexing specification). *)
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
  let gzip =
    List.find_map entries (function
    | List [Atom "gzip-output"; Atom vs] ->
      Some (Int.of_string vs)
    | _ -> None) in
  let demux =
    List.find_map entries (function
    | List (Atom "demux" :: _) as s ->
      Some (demux_specification_of_sexp s)
    | List (Atom "exclusive-demux" :: _) as s ->
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
  (mismatch, gzip, stats, demux, inputs)

(** Return a string containing a manual in markdown-ish syntax. *)
let more_help () : string =
  let open Blang in
  let ex ?(demux_policy = `inclusive) v =
    let incl = { demux_rules = v ; demux_policy } in
    (Sexp.to_string_hum (sexp_of_demux_specification incl)) in
  let outbuf = Buffer.create 1024 in
  let out fmt = ksprintf (Buffer.add_string outbuf) fmt in
  let title fmt = out ("# " ^^ fmt ^^ "\n\n") in
  let section fmt = out ("## " ^^ fmt ^^ "\n\n") in
  let word_wrap text =
    (* In Core_extended there were also: String.{squeeze,word_wrap} *)
    let outbuf = Buffer.create 42 in
    let out fmt = ksprintf (Buffer.add_string outbuf) fmt in
    let words =
      String.(split ~on:' '
          (map text (function '\n' | '\t' | '\r' -> ' ' | c -> c))) in
    let rec loop count = function
    | [] -> ()
    | "" :: more
    | "\n" :: more -> loop count more
    | one :: more ->
      let lgth = String.length one + count + 1 in
      if lgth > 72
      then (out "\n%s" one; loop (String.length one + 1) more)
      else (out "%s%s" (if count > 0 then " " else "") one; loop lgth more)
    in
    loop 0 words;
    Buffer.contents outbuf
  in
  let par fmt = ksprintf (fun s ->
      out "%s\n\n" (word_wrap s)
    ) fmt
  in
  let ul l = List.iter l ~f:(fun s -> out "- %s\n" (word_wrap s)); out "\n" in
  let code s =
    out "%s\n\n" (String.split s ~on:'\n'
                  |> List.map ~f:(sprintf "    %s")
                  |> String.concat ~sep:"\n") in
  title "Biocaml's Demultiplexer";
  section "Usage";
  par "The general idea is to call";
  code "`biocaml demux <specification> [<more options>] <R1> <R2> <R3> ...`";
  par "where";
  ul [
    "`specification` is either `-specification <file>` or `-demux <string>`. \
     A specification file containing `(demux <string>)` is equivalent to \
     `-demux <string>`";
    "`R1`, `R2`, … are (potentially gzipped) fastq files containing \
     the reads to demultiplex";
    "many other options are available, see `biocaml demux -help`";
  ];
  section "Examples of Demux-Specifications:";
  par "Two Illumina-style libraries (the index is read n°2):";
  code
    (ex [
       { name_prefix = "LibONE";
         filter =
           `barcoding (barcode_specification ~position:1 "ACTGTT"
               ~mismatch:1 ~on_read:2) };
       { name_prefix = "LibTWO";
         filter = `barcoding (
             barcode_specification ~position:1 "CTTATG"
               ~mismatch:1 ~on_read:2) };
     ]);
  par "A library with two barcodes to match:";
  code
    (ex [
       { name_prefix = "LibAND";
         filter = `barcoding (
             and_ [
               barcode_specification ~position:5 "ACTGTT"
                 ~mismatch:1 ~on_read:1;
               barcode_specification ~position:1 "TTGT"
                 ~on_read:2;
             ])}
     ]);
  par "A merge of two barcodes into one “library”:";
  code
    (ex [
       { name_prefix = "LibOR";
         filter = `barcoding (or_ [
               barcode_specification ~position:5 "ACTGTT"
                 ~mismatch:1 ~on_read:1;
               barcode_specification ~position:1 "TTGT" ~on_read:2;
             ]) }]);
  let example =
    "(demux\n\
    \  (library \"Lib with ånnœ¥ing name\" (barcoding ACCCT:1:2)) \
     ;; one barcode on R1, at pos 2\n\
    \  (library GetALL (barcoding true)) ;; get everything\n\
    \  (library Merge (barcoding (and AGTT:2:42:1 ACCC:2:42:1 \n\
    \                   (not AGTGGTC:1:1:2))) \
     ;; a ∧ b ∧ ¬c  matching\n\
     ))" in
  let spec = Sexp.of_string example |! demux_specification_of_sexp in
  par "The following one uses the abbreviated syntax:";
  code example;
  par "which is equivalent to:";
  code (ex spec.demux_rules);
  Buffer.contents outbuf


let command =
  Command_line.(
    basic ~summary:"Fastq deumltiplexer"
      ~readme:begin fun () ->
        "See option [-manual] for more detailed explanations."
      end
      Spec.(
        file_to_file_flags ()
        ++ gzip_output_flags ~activation:true
        +> flag "default-mismatch" (optional int)
            ~doc:"<int> default maximal mismatch allowed (default 0)"
        +> flag "demux" (optional string)
            ~doc:"<string> give the specification as a list of S-Expressions"
        +> flag "specification" ~aliases:["spec"] (optional string)
            ~doc:"<file> give a path to a file containing the specification"
        +> flag "statistics" ~aliases:["stats"] (optional string)
            ~doc:"<file> do some basic statistics and write them to <file>"
        +> flag "manual" ~aliases:["man"] (no_arg)
            ~doc:" display more help about demultiplexing"
        +> anon (sequence ("READ-FILES" %: string))
        ++ uses_lwt ())
      begin fun mismatch_cl demux_cl spec stats_cl manual read_files_cl ->
        begin
          if manual then printf "\n%s\n%!" (more_help ());
          dbgi "before doing anything";
          begin match spec with
          | Some s ->
            wrap_deferred_lwt (fun () ->
                Lwt_io.(with_file ~mode:input s (fun i -> read i)))
            >>| parse_configuration
          | None -> return (None, None, None, None, None)
          end
          >>= fun (mismatch, gzip_conf, stats, demux, inputs) ->
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
          let () =
            Option.iter gzip_conf (fun level ->
              Global_configuration.gzip_set_if_not_set ~level ()) in
          dbgi "Before do_statistics";
          let do_statistics = if stats_cl <> None then stats_cl else stats in
          let demux_spec_from_cl =
            Option.bind demux_cl (fun s ->
              try
                Some (Sexp.of_string (sprintf "(demux %s)" s)
                      |> demux_specification_of_sexp)
              with e ->
                dbgi "Parsing error: %s" (Exn.to_string e); None) in
          let demux =
            if demux_spec_from_cl <> None then demux_spec_from_cl
            else demux in
          let demux_specification =
            (* let default = *)
            Option.value demux
              ~default:{demux_rules = []; demux_policy = `inclusive} in
          perform ~mismatch ?do_statistics ~read_files ~demux_specification
        end
        >>< begin function
        | Ok () -> return ()
        | Error e ->
          let s =
            Sexp.to_string_hum
              (<:sexp_of<
                  [ `failure of string
                  | `lwt_exn of exn
                  | `io_multi_error of
                      [ `failure of string
                      | `lwt_exn of exn
                      | `openning_files of [ `lwt_exn of exn ] list ] list
                  | `openning_files of [ `lwt_exn of exn ] list ] >>
                 e)
          in
          error s
        end
      end)


