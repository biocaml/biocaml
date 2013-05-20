open Core.Std
open Flow
open Biocaml_app_common
open Biocaml


module Genlex = struct
  include Genlex
  let sexp_of_token = function
    | Kwd s ->    Sexp.Atom (sprintf "[Kwd %S]" s)
    | Ident s ->  Sexp.Atom (sprintf "[Ident %S]" s)
    | Int s ->    Sexp.Atom (sprintf "[Int %d]" s)
    | Float s ->  Sexp.Atom (sprintf "[Float %f]" s)
    | String s -> Sexp.Atom (sprintf "[String %S]" s)
    | Char s ->   Sexp.Atom (sprintf "[Char %S]" (String.of_char s))

end
let spec_lexer = Genlex.make_lexer [":"; "with"; "without"; "="; "("; ")"; "and"]

let parse_spec_aux s =
  let open Genlex in
  let char_stream = String.to_list (s ^ " special_end") |! Stream.of_list in
  begin try
    let tokens = Biocaml_stream.to_list (spec_lexer char_stream) in
    match List.rev tokens with
    | (Ident "special_end") :: more ->
      return (List.rev more)
    | [] -> error (`lexical "Nothing parsed")
    | i -> error (`lexical "Unfinished Input")
    with
    | Stream.Error e -> error (`lexical e)
  end
  >>= fun tokens ->
  let parse_meta_int = function
    | Int i :: t -> return (`int i, t)
    | Ident "random" :: Int m :: Int n :: t -> return (`random (m, n), t)
    | l -> error (`not_a_meta_int l)
  in
  let parse_with_or_without_n ?(default=`with_n) = function
    | Kwd "with" :: Ident "N" :: t ->
      (`with_n, t, true)
    | Kwd "without" :: Ident "N" :: t ->
      (`without_n, t, true)
    | l -> (default, l, false)
  in
  let read_length_spec = function
    | Kwd "with" :: Ident "read" :: Ident "length" :: Kwd "=" :: some ->
      parse_meta_int some
      >>= fun (meta_int, rest) ->
      return (`read_length meta_int, rest, true)
    | l -> return (`none, l, false)
  in
  let typed_columns =
    let rec get_columns acc = function
      | []
      | Kwd "and" :: _ as l -> return (acc, l)
      | Ident "int" :: l -> get_columns (`int :: acc) l
      | Ident "float" :: l -> get_columns (`float :: acc) l
      | l -> error (`expecting_column_type_or_and_but_got l)
    in
    function
    | Kwd "with" :: Ident "columns" :: t ->
      get_columns [] t
      >>= fun (cols, rest) ->
      return (`columns (List.rev cols), rest, true)
    | l -> return (`none, l, false)
  in
  let rec parse_list how acc = function
    | [] -> return acc
    | Kwd "and" :: [] as l -> error (`expecting_spec_but_got l)
    | Kwd "and" :: q :: t as l when acc = [] -> error (`expecting_spec_but_got l)
    | Kwd "and" :: q :: t
    | q :: t ->
      how acc (q :: t)
  in
  let rec parse_bam_or_sam accumulator current_list =
    let wn, next, advance_wn = parse_with_or_without_n current_list in
    read_length_spec next
    >>= fun (rls, next, advance_rls) ->
    if advance_wn || advance_rls then
      parse_list parse_bam_or_sam (rls :: wn :: accumulator) next
    else
      error (`expecting_spec_but_got current_list)
  in
  let rec parse_fastq accumulator current_list =
    let wn, next, advance_wn = parse_with_or_without_n current_list in
    read_length_spec next
    >>= fun (rls, next, advance_rls) ->
    if advance_wn || advance_rls then
      parse_list parse_fastq (rls :: wn :: accumulator) next
    else
      error (`expecting_spec_but_got current_list)
  in
  let parse_bed l =
    let rec parse_bed_aux accumulator current_list =
      typed_columns current_list
      >>= fun (cols, next, advanced) ->
      if advanced then
        parse_list parse_bed_aux (cols :: accumulator) next
      else
        error (`expecting_spec_but_got current_list)
    in
    parse_list parse_bed_aux [] l
  in
  let rec parse_table accumulator current_list =
    typed_columns current_list
    >>= fun (cols, next, advanced) ->
    if advanced then
      parse_list parse_table (cols :: accumulator) next
    else
      error (`expecting_spec_but_got current_list)
  in

  begin match tokens with
  | Ident f :: t when String.lowercase f = "fastq" ->
    parse_list parse_fastq [] t
    >>= fun spec ->
    return (`fastq spec)
  | Ident f :: t when String.lowercase f = "bed" ->
    parse_bed t
    >>= fun args ->
    return (`bed args)
  | Ident f :: t when String.lowercase f = "sam" ->
    parse_list parse_bam_or_sam [] t
    >>= fun args ->
    return (`sam args)
  | Ident f :: t when String.lowercase f = "bam" ->
    parse_list parse_bam_or_sam [] t
    >>= fun args ->
    return (`bam args)
  | Ident f :: t when String.lowercase f = "table" ->
    parse_list parse_table [] t
    >>= fun args ->
    return (`table args)
  | tokens ->
    error (`uknown_specification tokens)
  end

let parse_spec s =
  begin
    let s' = String.strip s in
    if String.is_prefix s' ~prefix:"fasta:"
    then
      Fasta.Random.specification_of_string
        (String.sub s' 6 (String.length s' - 6)) |> of_result
      >>= fun spec ->
      return (`fasta spec)
    else
      parse_spec_aux s
  end
  >>< begin function
  | Ok o -> return o
  | Error e -> error (`parse_specification e)
  end

let random_dna ?(with_n=true) read_length_spec =
  let length =
    match read_length_spec with
    | `int n -> n
    | `random (n, m) -> n + Random.int m
  in
  String.init length (fun _ ->
    match Random.int (if with_n then 5 else 4) with
    | 0 -> 'A'
    | 1 -> 'C'
    | 2 -> 'G'
    | 3 -> 'T'
    | _ -> 'N')

let random_quality_array seq =
  Array.init (String.length seq) (fun _ ->
    Phred_score.of_probability_exn (Random.float 1.0))


let random_fastq_transform ~args () =
  let todo = ref 0 in
  let seq_num = ref 0 in
  let read_length_spec =
    List.find_map args (function `read_length m -> Some m | _ -> None)
    |! Option.value ~default:(`int 50) in
  let with_n =
    match List.find args (function `without_n -> true | _ -> false) with
    | Some _ -> false
    | _ -> true in
  let make_read () = random_dna ~with_n read_length_spec in
  let make_qualities read =
    String.map read (fun _ ->
      Phred_score.(of_int_exn (Random.int (126 - 33)) |! to_ascii_exn))
  in
  Transform.make ()
    ~next:(fun stopped ->
      match !todo, stopped with
      | 0, true -> `end_of_stream
      | 0, false -> `not_ready
      | n, _  when n < 0 -> assert false
      | n, _ ->
        decr todo;
        incr seq_num;
        let sequence = make_read () in
        let qualities = make_qualities sequence in
        `output (Fastq.(
          { name = sprintf "Random sequence: %d" !seq_num;
            sequence;
            comment = "";
            qualities } )))
    ~feed:(fun () -> incr todo)

let random_bed_transform ~args () =
  let columns =
    List.find_map args (function `columns m -> Some m | _ -> None)
    |! Option.value ~default:[] in
  let todo = ref 0 in
  let seq_num = ref 0 in
  let make_item seqnum =
    let span = Random.int 4242 in
    let start = Random.int 3 *  seqnum in
    let rest =
      List.map columns (function
      | `int -> `int (Random.int 42)
      | `float -> `float (Random.float 1000.)) in
    (sprintf "chr%d" seqnum, start, start + span, Array.of_list rest)
  in
  Transform.make ()
    ~next:(fun stopped ->
      match !todo, stopped with
      | 0, true -> `end_of_stream
      | 0, false -> `not_ready
      | n, _  when n < 0 -> assert false
      | n, _ ->
        decr todo;
        incr seq_num;
        `output (make_item !seq_num))
    ~feed:(fun () -> incr todo)

let random_table_transform ~args () =
  let columns =
    List.find_map args (function `columns m -> Some m | _ -> None)
    |! Option.value ~default:[] in
  let todo = ref 0 in
  let make_item () =
    let cols =
      List.map columns (function
      | `int -> `int (Random.int 42)
      | `float -> `float (Random.float 1000.)) in
    Array.of_list cols
  in
  Transform.make ()
    ~next:(fun stopped ->
      match !todo, stopped with
      | 0, true -> `end_of_stream
      | 0, false -> `not_ready
      | n, _  when n < 0 -> assert false
      | n, _ ->
        decr todo;
        `output (make_item ()))
    ~feed:(fun () -> incr todo)

let random_sam_item_transform ~args () =
  let total_chromosomes = 23 in
  let read_length_spec =
    List.find_map args (function `read_length m -> Some m | _ -> None)
    |! Option.value ~default:(`int 50) in
  let with_n =
    match List.find args (function `without_n -> true | _ -> false) with
    | Some _ -> false
    | _ -> true in
  let header_done = ref false in
  let todo = ref 0 in
  let chromosomes_to_go = ref total_chromosomes in
  let ref_dictionary =
    Array.init !chromosomes_to_go ~f:(fun i ->
      {Sam.ref_name = sprintf "chr%d" i;
       Sam.ref_length = (Random.int 999_999_999 + 100_000_000);
       Sam.ref_assembly_identifier = None;
       Sam.ref_checksum = None; Sam.ref_species = None;
       Sam.ref_uri = None; Sam.ref_unknown = []} )
      in

  let make_item () =
    if not !header_done then (
      header_done := true;
      `header_line ("1.0", `unsorted, [])
    ) else if !chromosomes_to_go > 0 then (
      let name =
        ref_dictionary.(total_chromosomes - !chromosomes_to_go).Sam.ref_name in
      let length =
        ref_dictionary.(total_chromosomes - !chromosomes_to_go).Sam.ref_length
      in
      decr chromosomes_to_go;
      `header ("SQ", [("SN", name); ("LN", Int.to_string length)])
    ) else if !chromosomes_to_go = 0 then (
      decr chromosomes_to_go;
      (`header
          ("PG",
           [("ID", "Bowtie"); ("VN", "0.12.7");
            ("CL",
             "\"/usr/local/bowtie-0.12.7/bowtie -k 1 --best --sam \
              --phred33-quals -p 3 bowtie_index/mm9/mm9 \
              Sskdjkdjs.fastq this_one_.sam\"")]))
    ) else if !chromosomes_to_go = -1 then (
      decr chromosomes_to_go;
      (`reference_sequence_dictionary ref_dictionary)
    ) else (
      decr chromosomes_to_go;
      let ref_seq = ref_dictionary.(Random.int total_chromosomes) in
      let sequence = random_dna ~with_n read_length_spec in
      let qualities = random_quality_array sequence in
      (`alignment
          {Sam.query_template_name = sprintf "some.%d" (- !chromosomes_to_go - 2);
           Sam.flags = Sam.Flags.of_int 16;
           Sam.reference_sequence = `reference_sequence ref_seq;
           Sam.position = Some 3003188;
           Sam.mapping_quality = None;
           Sam.cigar_operations = [|`M 25|];
           Sam.next_reference_sequence = `none;
           Sam.next_position = None; Sam.template_length = None;
           Sam.sequence = `string sequence;
           Sam.quality = qualities;
           Sam.optional_content = [
             ("XA", 'C', `int 0);
             ("MD", 'Z', `string "25");
             ("NM", 'C', `int 0)]
          })
    )
  in
  Transform.make ()
    ~next:(fun stopped ->
      match !todo, stopped with
      | 0, true -> `end_of_stream
      | 0, false -> `not_ready
      | n, _  when n < 0 -> assert false
      | n, _ ->
        decr todo;
        `output (make_item ()))
    ~feed:(fun () -> incr todo)


let do_output output_meta_channel transform nb_items =
  IO.with_out_channel
    ~buffer_size:!Global_configuration.output_buffer_size
    output_meta_channel ~f:(fun out ->
    let rec loop = function
      | 0 ->
        Transform.stop transform;
        let rec all () =
          begin match Transform.next transform with
          | `not_ready -> all ()
          | `end_of_stream -> return ()
          | `output s ->
            IO.write out s
            >>= fun () ->
            all ()
          end in
        all ()
      | n ->
        Transform.feed transform ();
        begin match Transform.next transform with
        | `not_ready -> loop (n - 1)
        | `end_of_stream -> assert false
        | `output s ->
          IO.write out s
          >>= fun () ->
          loop (n - 1)
        end
    in
    loop nb_items)

let do_random ~output_file ~nb_items spec =
  let output_meta_channel =
    match output_file with
    | None -> `stdout
    | Some f -> `overwrite_file f in
  parse_spec (String.concat ~sep:" " spec)
  >>= fun spec ->
  let tags, args, fasta_tags =
    let t, a, ft =
      match spec with
      | `fastq args -> (`fastq,      args, [])
      | `bed args   -> (`bed,        args, [])
      | `sam args   -> (`sam,        args, [])
      | `bam args   -> (`bam,        args, [])
      | `table args -> (`table Table.Row.Tags.default, args, [])
      | `fasta spec ->
        let tags =
          Fasta.Random.get_tags spec
          |> Option.value ~default:Fasta.Tags.default in
        (`fasta tags, [], spec)
    in
    if Global_configuration.gzip_output_activated ()
    then (`gzip t, a, ft)
    else (t, a, ft)
  in
  output_transform_of_tags tags
  >>= begin function
  | `to_fastq tr ->
    let transform = Transform.compose (random_fastq_transform ~args ()) tr in
    do_output output_meta_channel transform nb_items
  | `to_bed tr ->
    let transform = Transform.compose (random_bed_transform ~args ()) tr in
    do_output output_meta_channel transform nb_items
  | `to_sam_item tr ->
    let transform =
      Transform.(
        compose (random_sam_item_transform ~args ()) tr
        |! on_output ~f:(function
          | Ok o -> o
          | Error e ->
            failwith "ERROR in SAM/BAM OUTPUT")
      ) in
    do_output output_meta_channel transform nb_items
  | `to_table tr ->
    let transform = Transform.compose (random_table_transform ~args ()) tr in
    (*
    let transform =
      Transform.(
        compose (random_table_transform ~args ()) tr
        |! on_output ~f:(function
          | Ok o -> o
          | Error e ->
            failwith "ERROR in SAM/BAM OUTPUT")
      ) in
 *)
    do_output output_meta_channel transform nb_items
  | `to_char_fasta (tr, _) ->
    of_result
      (Fasta.Random.unit_to_random_char_seq_raw_item fasta_tags)
    >>= fun random_transform ->
    let transform = Transform.compose random_transform tr in
    do_output output_meta_channel transform nb_items

  | _ -> error (`not_implemented "Random: do_random")
  end
  >>= fun () ->


  return ()




let stringify m =
  m >>< begin function
  | Ok o -> return o
  | Error e ->
    error (<:sexp_of< [
    | `io_exn of exn
    | `not_implemented of string
    | `inconsistent_tags of [ `int_sequence ]
    | `file_exists of string
    | `wrong_path of string
    | `parse_specification of
        [ `expecting_spec_but_got of Genlex.token list
        | `expecting_column_type_or_and_but_got of Genlex.token list
        | `lexical of string
        | `fasta of [> `parse_specification of exn ]
        | `tags_of_string of exn
        | `not_a_meta_int of Genlex.token list
        | `uknown_specification of Genlex.token list ]
    ] >> e |! Sexp.to_string_hum)
  end

let do_random ~output_file ~nb_items spec =
  stringify (do_random ~output_file ~nb_items  spec)

let command =
  let open Command_line in
  let spec =
    let open Spec in
    verbosity_flags ()
    ++ output_buffer_size_flag ()
    ++ gzip_output_flags ~activation:true
    ++ step (fun k v -> k ~output_file:v)
    +> flag "output-file" ~aliases:["o"] (optional string)
        ~doc:"<filename> output to a file"
    ++ step (fun k v -> k ~nb_items:v)
    +> flag "items" (optional_with_default 42 int)
      ~doc:"<nb> Number of records/items to produce"
    +> anon (sequence ("specification" %: string))
    ++ uses_lwt ()
  in
  basic ~summary:"Generate random files" spec do_random
