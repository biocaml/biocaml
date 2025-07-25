module Safe = struct
  type error =
    [ `invalid_int of string
    | `invalid_float of string
    ]

  let int_of_string s =
    try Ok (Int.of_string s) with
    | _ -> Error (`invalid_int s)
  ;;

  let float_of_string s =
    try Ok (Float.of_string s) with
    | _ -> Error (`invalid_float s)
  ;;
end

let is_valid_dna = String.for_all ~f:(String.contains "ACGTN")

type vcf_id = string
type vcf_description = string

type vcf_number =
  | Number of int
  | OnePerAllele
  | OnePerGenotype
  | Unknown

type vcf_format_type =
  [ `integer_value
  | `float_value
  | `character_value
  | `string_value
  ]

type vcf_info_type =
  [ vcf_format_type
  | `flag_value
  ]

type vcf_info_meta = Info of vcf_number * vcf_info_type * vcf_description
type vcf_filter_meta = Filter of vcf_description
type vcf_format_meta = Format of vcf_number * vcf_format_type * vcf_description
type vcf_alt_meta = Alt of vcf_description

type vcf_meta =
  { vcfm_version : string
  ; vcfm_id_cache : vcf_id Set.Poly.t
  ; vcfm_info : (vcf_id, vcf_info_meta) Hashtbl.t
  ; vcfm_filters : (vcf_id * vcf_filter_meta) list
  ; vcfm_format : (vcf_id, vcf_format_meta) Hashtbl.t
  ; vcfm_alt : (string, vcf_alt_meta) Hashtbl.t
  ; vcfm_arbitrary : (string, string) Hashtbl.t
  ; vcfm_header : string list
  ; vcfm_samples : string list
  }

type vcf_format =
  [ `integer of int
  | `float of float
  | `character of char
  | `string of string
  | `missing (* FIXME(superbobry): use option? *)
  ]

type vcf_info =
  [ vcf_format
  | `flag of string
  ]

type vcf_row =
  { vcfr_chrom : string (* FIXME(superbobry): Chr.t *)
  ; vcfr_pos : int
  ; vcfr_ids : string list
  ; vcfr_ref : string
  ; vcfr_alts : string list (* FIXME(superbobry): proper typing! *)
  ; vcfr_qual : float option
  ; vcfr_filter : vcf_id list
  ; vcfr_info : (vcf_id, vcf_info list) Hashtbl.t
  ; vcfr_samples : (vcf_id, (vcf_id * vcf_format list) list) Hashtbl.t
  }

(* +--------------+
   | Error types. |
   +--------------+ *)

type vcf_parse_row_error =
  [ Safe.error
  | `info_type_coersion_failure of vcf_info_type * string
  | `format_type_coersion_failure of vcf_format_type * string
  | `invalid_dna of string
  | `unknown_info of vcf_id
  | `unknown_filter of vcf_id
  | `unknown_alt of string
  | `duplicate_ids of vcf_id list
  | `invalid_arguments_length of vcf_id * int * int
  | `invalid_row_length of int * int
  | `malformed_sample of string
  | `unknown_format of vcf_id
  ]

type vcf_parse_error =
  [ `malformed_meta of Biocaml.Pos.t * string
  | `malformed_row of Biocaml.Pos.t * vcf_parse_row_error * string
  | `malformed_header of Biocaml.Pos.t * string
  | `incomplete_input of Biocaml.Pos.t * string list * string option
  | `not_ready
  ]

(* +---------------------------------------------------------+
   | Functions for converting VCF types to and from strings. |
   +---------------------------------------------------------+ *)

let string_to_vcf_number = function
  | "A" -> OnePerAllele
  | "G" -> OnePerGenotype
  | "." -> Unknown
  | arity -> Number (int_of_string arity)
;;

let string_to_vcf_format_type s =
  match String.lowercase s with
  | "integer" -> `integer_value
  | "float" -> `float_value
  | "character" -> `character_value
  | "string" -> `string_value
  | v -> failwith ("string_to_vcf_format_type: invalid format: " ^ v)
;;

let vcf_format_type_to_string = function
  | `integer_value -> "integer"
  | `float_value -> "float"
  | `character_value -> "character"
  | `string_value -> "string"
;;

let coerce_to_vcf_format_type t s =
  if String.equal s "."
  then
    (* Note(superbobry): any value might be missing, according to VCF4.1
        specification. *)
    Ok `missing
  else (
    match t with
    | `integer_value -> Result.map (Safe.int_of_string s) ~f:(fun x -> `integer x)
    | `float_value -> Result.map (Safe.float_of_string s) ~f:(fun x -> `float x)
    | `character_value when String.length s = 1 -> Ok (`character s.[0])
    | `string_value -> Ok (`string s)
    | _ -> Error (`format_type_coersion_failure (t, s)))
;;

let coerce_n ~f key n s =
  let open Result.Monad_infix in
  let res = lazy (Result.all (List.map ~f (String.split ~on:',' s))) in
  match n with
  | Number n ->
    Lazy.force res
    >>= fun values ->
    if List.length values = n
    then Ok values
    else Error (`invalid_arguments_length (key, List.length values, n))
  | Unknown
  (* TODO(superbobry): how do we know the nr. of alleles? *)
  | OnePerAllele
  (* TODO(superbobry): how to make sure that we have exactly _one_
     value per genotype? *)
  | OnePerGenotype -> Lazy.force res
;;

let string_to_vcf_info_type s =
  match String.lowercase s with
  | "flag" -> `flag_value
  | s -> string_to_vcf_format_type s
;;

let vcf_info_type_to_string = function
  | `flag_value -> "flag"
  | #vcf_format_type as t -> vcf_format_type_to_string t
;;

let coerce_to_vcf_info_type t s =
  let res =
    match t with
    | `flag_value -> Ok (`flag s)
    | #vcf_format_type -> coerce_to_vcf_format_type t s
  in
  Result.map_error res ~f:(fun _exn -> `info_type_coersion_failure (t, s))
;;

(* +-------------------+
   | Error formatting. |
   +-------------------+ *)

let parse_row_error_to_string = function
  | `invalid_int s -> sprintf "invalid_integer (%s)" s
  | `invalid_float s -> sprintf "invalid_float (%s)" s
  | `info_type_coersion_failure (t, s) ->
    sprintf "info_type_coersion_failure (%s, %S)" (vcf_info_type_to_string t) s
  | `format_type_coersion_failure (t, s) ->
    sprintf "format_type_coersion_failure (%s, %S)" (vcf_format_type_to_string t) s
  | `invalid_dna s -> sprintf "invalid_dna (%s)" s
  | `unknown_info s -> sprintf "unknown_info (%s)" s
  | `unknown_filter f -> sprintf "unknown_filter (%s)" f
  | `unknown_alt s -> sprintf "unknown_alt (%s)" s
  | `duplicate_ids ids -> sprintf "duplicate_ids (%s)" (String.concat ~sep:", " ids)
  | `invalid_arguments_length (key, got, expected) ->
    sprintf "invalid_arguments_length (%s, %i, %i)" key got expected
  | `invalid_row_length (got, expected) ->
    sprintf "invalid_row_length (%i, %i)" got expected
  | `malformed_sample s -> sprintf "malformed_sample (%s)" s
  | `unknown_format f -> sprintf "unknown_formar (%s)" f
;;

let parse_error_to_string =
  let pos () a = Biocaml.Pos.to_string a in
  function
  | `malformed_meta (p, s) -> sprintf "malformed_meta (%a, %S)" pos p s
  | `malformed_row (p, err, s) ->
    sprintf "malformed_row (%s, %a, %S)" (parse_row_error_to_string err) pos p s
  | `malformed_header (p, s) -> sprintf "malformed_header (%a, %s)" pos p s
  | _ -> sprintf "unknown_error"
;;

(* +-------------------------------------------------+
   | Types for reserved INFO, FORMAT and ALT entries |
   +-------------------------------------------------+ *)

(** Note(superbobry): this is mostly taken from PyVCF module by
    James Casbon. See https://github.com/jamescasbon/PyVCF. The
    standard does _NOT_ specify how many arguments are allowed
    for each reserved item, so we assume 'Unknown'. *)
let reserved_info =
  Hashtbl.Poly.of_alist_exn
    [ "AA", `string_value
    ; "AC", `integer_value
    ; "AF", `float_value
    ; "AN", `integer_value
    ; "BQ", `float_value
    ; "CIGAR", `string_value
    ; "DB", `flag_value
    ; "DP", `integer_value
    ; "H2", `flag_value
    ; "MQ", `float_value
    ; "MQ0", `integer_value
    ; "NS", `integer_value
    ; "SB", `string_value
    ; "SOMATIC", `flag_value
    ; "VALIDATED", `flag_value
    ; (* VCF 4.1 Additions *)
      "IMPRECISE", `flag_value
    ; "NOVEL", `flag_value
    ; "END", `integer_value
    ; "SVTYPE", `string_value
    ; "CIPOS", `integer_value
    ; "CIEND", `integer_value
    ; "HOMLEN", `integer_value
    ; "HOMSEQ", `integer_value
    ; "BKPTID", `string_value
    ; "MEINFO", `string_value
    ; "METRANS", `string_value
    ; "DGVID", `string_value
    ; "DBVARID", `string_value
    ; "MATEID", `string_value
    ; "PARID", `string_value
    ; "EVENT", `string_value
    ; "CILEN", `integer_value
    ; "CN", `integer_value
    ; "CNADJ", `integer_value
    ; "CICN", `integer_value
    ; "CICNADJ", `integer_value
    ]

and reserved_format =
  Hashtbl.Poly.of_alist_exn
    [ "GT", `string_value
    ; "DP", `integer_value
    ; "FT", `string_value
    ; "GL", `float_value
    ; "GQ", `float_value
    ; "HQ", `float_value
    ; (* VCF 4.1 Additions *)
      "CN", `integer_value
    ; "CNQ", `float_value
    ; "CNL", `float_value
    ; "NQ", `integer_value
    ; "HAP", `integer_value
    ; "AHAP", `integer_value
    ]

and reserved_alt =
  Hashtbl.Poly.of_alist_exn
    [ "DEL", Alt "Deletion relative to the reference"
    ; "INS", Alt "Insertion of novel sequence relative to the reference"
    ; "DUP", Alt "Region of elevated copy number relative to the reference"
    ; "INV", Alt "Inversion of reference sequence"
    ; "CNV", Alt "Copy number variable region"
    ; "DUP:TANDEM", Alt "TANDEM Tandem duplication"
    ; "DEL:ME", Alt "ME Deletion of mobile element relative to the reference"
    ; "INS:ME", Alt "ME Insertion of a mobile element relative to the reference"
    ]
;;

let default_meta =
  { vcfm_version = "<unknown>"
  ; vcfm_id_cache = Set.Poly.empty
  ; vcfm_info = Hashtbl.Poly.create ()
  ; vcfm_filters = []
  ; vcfm_format = Hashtbl.Poly.create ()
  ; vcfm_alt = reserved_alt
  ; vcfm_arbitrary = Hashtbl.Poly.create ()
  ; vcfm_header = []
  ; vcfm_samples = []
  }
;;

(* +--------------------+
   | VCF heavy-lifting. |
   +--------------------+ *)

let string_to_vcfr_ref s =
  let s = String.uppercase s in
  if is_valid_dna s then Ok s else Error (`invalid_dna s)
;;

let string_to_vcfr_info { vcfm_info; _ } s =
  let go values =
    List.map (String.split ~on:';' s) ~f:(fun chunk ->
      let key, raw_value =
        Option.value ~default:(chunk, "") (String.lsplit2 ~on:'=' chunk)
      in
      let chunk_values =
        match Hashtbl.find vcfm_info key with
        | Some (Info (_t, `flag_value, _description)) when String.equal raw_value "" ->
          Ok [ `flag key ]
        | Some (Info (n, t, _description)) ->
          coerce_n ~f:(coerce_to_vcf_info_type t) key n raw_value
        | None -> Error (`unknown_info key)
      in
      Result.map chunk_values ~f:(fun data -> Hashtbl.set values ~key ~data))
  and values = Hashtbl.Poly.create () in
  Result.(map (all_unit (go values)) ~f:(Fn.const values))
;;

let string_to_vcfr_filter { vcfm_filters; _ } s =
  match String.split ~on:';' s with
  | [ "PASS" ] -> Ok []
  | chunks -> (
    match
      List.find chunks ~f:(fun chunk ->
        not (List.Assoc.mem ~equal:String.equal vcfm_filters chunk))
    with
    | Some unknown_filter -> Error (`unknown_filter unknown_filter)
    | None -> Ok chunks)
;;

let string_to_vcfr_ids { vcfm_id_cache; _ } s =
  match String.split ~on:';' s with
  | [ "." ] -> Ok []
  | chunks ->
    let duplicate_ids = List.filter chunks ~f:(Set.mem vcfm_id_cache) in
    if List.is_empty duplicate_ids
    then Ok chunks
    else Error (`duplicate_ids duplicate_ids)
;;

let string_to_vcfr_alts { vcfm_alt; _ } s =
  match String.split ~on:',' s with
  | [ "." ] -> Ok []
  | chunks ->
    let res =
      List.map chunks ~f:(fun chunk ->
        let n = String.length chunk in
        match Char.(chunk.[0] = '<' && chunk.[n - 1] = '>', is_valid_dna chunk) with
        | true, _ ->
          if Hashtbl.mem vcfm_alt (String.sub ~pos:1 ~len:(n - 2) chunk)
          then Ok chunk
          else Error (`unknown_alt chunk)
        | false, true -> Ok chunk
        | false, false -> Error (`invalid_dna chunk))
      (* Impossible. *)
    in
    Result.all res
;;

let list_to_vcfr_samples { vcfm_format; vcfm_samples; _ } chunks =
  let open Result.Monad_infix in
  let samples = Hashtbl.Poly.create () in
  let go sample_keys id raw_sample =
    let sample_chunks = String.split ~on:':' raw_sample in
    if List.(length sample_keys <> length sample_chunks)
    then Error (`malformed_sample raw_sample)
    else (
      let res =
        List.map2_exn sample_keys sample_chunks ~f:(fun key raw_value ->
          match Hashtbl.find vcfm_format key with
          | Some (Format (n, t, _description)) ->
            coerce_n ~f:(coerce_to_vcf_format_type t) key n raw_value
            >>= fun value -> Ok (key, value)
          | None -> Error (`unknown_format key))
      in
      Result.(map (all res) ~f:(fun data -> Hashtbl.set samples ~key:id ~data)))
  in
  match chunks with
  | raw_sample_keys :: raw_samples ->
    let sample_keys = String.split ~on:':' raw_sample_keys in
    let res = List.map2_exn vcfm_samples raw_samples ~f:(go sample_keys) in
    Result.map (Result.all_unit res) ~f:(Fn.const samples)
  | [] -> Ok samples
;;

let list_to_vcf_row meta chunks =
  let open Result.Monad_infix in
  let n_chunks = List.length chunks
  and n_columns = List.(length meta.vcfm_header + length meta.vcfm_samples) in
  match chunks with
  | vcfr_chrom
    :: raw_pos
    :: raw_id
    :: raw_ref
    :: raw_alt
    :: raw_qual
    :: raw_filter
    :: raw_info
    :: raw_samples
    when n_chunks = n_columns ->
    Safe.int_of_string raw_pos
    >>= fun vcfr_pos ->
    string_to_vcfr_ids meta raw_id
    >>= fun vcfr_ids ->
    string_to_vcfr_ref raw_ref
    >>= fun vcfr_ref ->
    string_to_vcfr_alts meta raw_alt
    >>= fun vcfr_alts ->
    string_to_vcfr_info meta raw_info
    >>= fun vcfr_info ->
    string_to_vcfr_filter meta raw_filter
    >>= fun vcfr_filter ->
    (* FIXME(superbobry): actually, we need monadic 'when here',
         because if there's no samples, there's nothing to parse. *)
    list_to_vcfr_samples meta raw_samples
    >>= fun vcfr_samples ->
    let row =
      { vcfr_chrom
      ; vcfr_pos
      ; vcfr_ids
      ; vcfr_ref
      ; vcfr_alts
      ; vcfr_qual = Result.ok (Safe.float_of_string raw_qual)
      ; vcfr_filter
      ; vcfr_info
      ; vcfr_samples
      }
    in
    Ok row
  | _ -> Error (`invalid_row_length (n_chunks, n_columns))
;;

(** +-------------------+
    | Parser interface. |
    +-------------------+ *)

type item = vcf_row

module Transform = struct
  let next_vcf_header meta p =
    let open Lines.Buffer in
    let { vcfm_info; vcfm_format; _ } = meta in
    let l = Option.value_exn (next_line p :> string option) in
    let chunks = List.filter ~f:String.(fun s -> s <> "") (String.split ~on:'\t' l) in
    match chunks with
    | "#CHROM" :: "POS" :: "ID" :: "REF" :: "ALT" :: "QUAL" :: "FILTER" :: "INFO" :: rest
      -> (
      match rest with
      (* Note(superbobry): FORMAT only makes sense if we have at least
           a single sample. *)
      | "FORMAT" :: (_ :: _ as samples) | ([] as samples) ->
        let merge_with_reserved ~c =
          Hashtbl.merge ~f:(fun ~key:_ v ->
            match v with
            | `Left t -> Some (c Unknown t "<reserved>")
            | `Right parsed -> Some parsed
            | `Both (_t, parsed) -> Some parsed)
        in
        (* Note(superbobry): merge parsed INFO and FORMAT entries with
               predefined ones; a VCF file _may_ override any of the
               reserved fields, in that case, default definition won't be
               used. *)
        let vcfm_info =
          merge_with_reserved reserved_info vcfm_info ~c:(fun n t description ->
            Info (n, t, description))
        and vcfm_format =
          merge_with_reserved reserved_format vcfm_format ~c:(fun n t description ->
            Format (n, t, description))
        and vcfm_header, vcfm_samples =
          if List.is_empty samples
          then chunks, samples
          else List.split_n chunks List.(length chunks - length samples)
        in
        Ok (`complete { meta with vcfm_info; vcfm_format; vcfm_header; vcfm_samples })
      | _ :: _ -> Error (`malformed_header (current_position p, l)))
    | _ -> Error (`malformed_header (current_position p, l))
  ;;

  let next_vcf_meta meta p =
    let open Lines.Buffer in
    let { vcfm_info; vcfm_filters; vcfm_format; vcfm_alt; _ } = meta in
    match (peek_line p :> string option) with
    | Some l when String.is_prefix l ~prefix:"##" -> (
      let _l = next_line p in
      let s = String.suffix l (String.length l - 2) in
      match String.lsplit2 s ~on:'=' with
      | Some ("fileformat", v) -> Ok (`partial { meta with vcfm_version = v })
      | Some ("INFO", v) ->
        Scanf.sscanf
          v
          "<ID=%s@,Number=%s@,Type=%s@,Description=%S>"
          (fun id n t description ->
          let info_meta =
            Info (string_to_vcf_number n, string_to_vcf_info_type t, description)
          in
          Hashtbl.set vcfm_info ~key:id ~data:info_meta);
        Ok (`partial meta)
      | Some ("FILTER", v) ->
        Scanf.sscanf v "<ID=%s@,Description=%S>" (fun id description ->
          let filter_meta = Filter description in
          let meta = { meta with vcfm_filters = (id, filter_meta) :: vcfm_filters } in
          Ok (`partial meta))
      | Some ("FORMAT", v) ->
        Scanf.sscanf
          v
          "<ID=%s@,Number=%s@,Type=%s@,Description=%S>"
          (fun id n t description ->
          let format_meta =
            Format (string_to_vcf_number n, string_to_vcf_format_type t, description)
          in
          Hashtbl.set vcfm_format ~key:id ~data:format_meta);
        Ok (`partial meta)
      | Some ("ALT", v) ->
        Scanf.sscanf v "<ID=%s@,Description=%S>" (fun id description ->
          let alt_meta = Alt description in
          Hashtbl.set vcfm_alt ~key:id ~data:alt_meta);
        Ok (`partial meta)
      | Some (k, v) ->
        Hashtbl.set meta.vcfm_arbitrary ~key:k ~data:v;
        Ok (`partial meta)
      | None -> Error (`malformed_meta (current_position p, s)))
    | Some _l ->
      (* Note(superbobry): if the line *does not* start with '##' it
           must be a header. *)
      next_vcf_header meta p
    | None -> Error `not_ready
  ;;

  let next_vcf_row meta p =
    let open Lines.Buffer in
    match (next_line p :> string option) with
    | Some l when not (String.is_empty l) -> (
      let chunks = List.filter ~f:String.(fun s -> s <> "") (String.split ~on:'\t' l) in
      match list_to_vcf_row meta chunks with
      | Ok row -> `output (Ok row)
      | Error err -> `output (Error (`malformed_row (current_position p, err, l))))
    | Some _ | None -> `not_ready (* All done, boss! *)
  ;;

  let next meta_ref p =
    match !meta_ref with
    | `complete meta -> next_vcf_row meta p
    | `partial meta -> (
      match next_vcf_meta meta p with
      | Ok meta ->
        meta_ref := meta;
        `not_ready
      | Error `not_ready -> `not_ready
      | Error err -> `output (Error err))
  ;;

  let string_to_item ?filename () =
    let name = sprintf "vcf_parser:%s" Option.(value ~default:"<>" filename) in
    let meta_ref = ref (`partial default_meta) in
    Lines.Transform.make_merge_error ~name ?filename ~next:(next meta_ref) ()
  ;;
end

module Test = struct
  let make_stream name : (vcf_row, vcf_parse_error) result Stream.t =
    let filename = Filename.concat "../../etc/test_data" name in
    let t = Transform.string_to_item ~filename () in
    let ic = In_channel.create filename in
    Tfxm.in_channel_strings_to_stream ~buffer_size:10 ic t
  ;;

  let compare_rows r1 r2 =
    let open Poly in
    r1.vcfr_chrom = r2.vcfr_chrom
    && r1.vcfr_pos = r2.vcfr_pos
    && r1.vcfr_ids = r2.vcfr_ids
    && r1.vcfr_ref = r2.vcfr_ref
    && r1.vcfr_alts = r2.vcfr_alts
    && r1.vcfr_qual = r2.vcfr_qual
    && r1.vcfr_filter = r2.vcfr_filter
    && Hashtbl.equal ( = ) r1.vcfr_info r2.vcfr_info
    && Hashtbl.equal ( = ) r1.vcfr_samples r2.vcfr_samples
  ;;

  let make_row ~chrom ~pos ~ids ~ref ~alts ~qual ~filter ~info ~samples =
    let vcfr_info = Hashtbl.Poly.of_alist_exn info in
    let vcfr_samples = Hashtbl.Poly.of_alist_exn samples in
    { vcfr_chrom = chrom
    ; vcfr_pos = pos
    ; vcfr_ids = ids
    ; vcfr_ref = ref
    ; vcfr_alts = alts
    ; vcfr_qual = qual
    ; vcfr_filter = filter
    ; vcfr_info
    ; vcfr_samples
    }
  ;;

  let test_parse_vcf_generic filename rows =
    let s = make_stream filename in
    List.iter rows ~f:(fun row ->
      match CFStream.next s with
      | Some (Ok actual_row) -> printf "%b\n" (compare_rows row actual_row)
      | Some (Error err) ->
        let msg = parse_error_to_string err in
        printf "%s:row *not* parsed, reason: %s: %b\n" filename msg false
      | None -> printf "%s:row missing: %b\n" filename false)
  ;;

  let%expect_test "test_parse_vcf_header" =
    let s = make_stream "vcf_01_header_only.vcf" in
    (match CFStream.next s with
     | None -> printf "No rows to return.\n"
     | Some (Ok _) -> assert false (* Impossible. *)
     | Some (Error err) ->
       let msg = parse_error_to_string err in
       printf "test_parse_vcf_header, reason: %s: %b\n" msg false);
    [%expect {| No rows to return. |}]
  ;;

  let%expect_test "test_parse_vcf_simple" =
    test_parse_vcf_generic
      "vcf_02_simple.vcf"
      [ make_row
          ~chrom:"20"
          ~pos:14370
          ~ids:[ "rs6054257" ]
          ~ref:"G"
          ~alts:[ "A" ]
          ~qual:(Some 29.0)
          ~filter:[]
          ~info:
            [ "NS", [ `integer 3 ]
            ; "DP", [ `integer 14 ]
            ; "AF", [ `float 0.5 ]
            ; "DB", [ `flag "DB" ]
            ; "H2", [ `flag "H2" ]
            ]
          ~samples:[]
      ];
    [%expect {| true |}]
  ;;

  let%expect_test "test_parse_vcf_1000g" =
    test_parse_vcf_generic
      "vcf_03_1000g.vcf"
      [ make_row
          ~chrom:"20"
          ~pos:17330
          ~ids:[]
          ~ref:"T"
          ~alts:[ "A" ]
          ~qual:None
          ~filter:[ "q10" ]
          ~info:[ "NS", [ `integer 3 ]; "DP", [ `integer 11 ]; "AF", [ `float 0.017 ] ]
          ~samples:[]
      ; make_row
          ~chrom:"20"
          ~pos:1110696
          ~ids:[ "rs6040355" ]
          ~ref:"A"
          ~alts:[ "G"; "T" ]
          ~qual:(Some 67.0)
          ~filter:[]
          ~info:
            [ "NS", [ `integer 2 ]
            ; "DP", [ `integer 10 ]
            ; "AF", [ `float 0.333; `float 0.667 ]
            ; "AA", [ `string "T" ]
            ; "DB", [ `flag "DB" ]
            ]
          ~samples:[]
      ; make_row
          ~chrom:"20"
          ~pos:1230237
          ~ids:[]
          ~ref:"T"
          ~alts:[]
          ~qual:(Some 47.0)
          ~filter:[]
          ~info:[ "NS", [ `integer 3 ]; "DP", [ `integer 13 ]; "AA", [ `string "T" ] ]
          ~samples:[]
      ; make_row
          ~chrom:"20"
          ~pos:1234567
          ~ids:[ "microsat1" ]
          ~ref:"GTC"
          ~alts:[ "G"; "GTCT" ]
          ~qual:(Some 50.0)
          ~filter:[]
          ~info:[ "NS", [ `integer 3 ]; "DP", [ `integer 9 ]; "AA", [ `string "G" ] ]
          ~samples:[]
      ];
    [%expect {|
      true
      true
      true
      true
    |}]
  ;;

  let%expect_test "test_parse_vcf_reserved" =
    test_parse_vcf_generic
      "vcf_04_reserved.vcf"
      [ make_row
          ~chrom:"20"
          ~pos:14370
          ~ids:[ "rs6054257" ]
          ~ref:"G"
          ~alts:[ "A" ]
          ~qual:(Some 29.0)
          ~filter:[]
          ~info:
            [ "NS", [ `integer 3 ]
            ; "DP", [ `integer 14 ]
            ; "AF", [ `float 0.5 ]
            ; "DB", [ `flag "DB" ]
            ; "H2", [ `flag "H2" ]
            ]
          ~samples:[]
      ];
    [%expect {| true |}]
  ;;

  let%expect_test "test_parse_vcf_alt" =
    test_parse_vcf_generic
      "vcf_05_alt.vcf"
      [ make_row
          ~chrom:"2"
          ~pos:321682
          ~ids:[]
          ~ref:"T"
          ~alts:[ "<DEL>" ]
          ~qual:(Some 6.0)
          ~filter:[]
          ~info:
            [ "IMPRECISE", [ `flag "IMPRECISE" ]
            ; "SVTYPE", [ `string "DEL" ]
            ; "END", [ `integer 321887 ]
            ; "SVLEN", [ `integer (-105) ]
            ; "CIPOS", [ `integer (-56); `integer 20 ]
            ; "CIEND", [ `integer (-10); `integer 62 ]
            ]
          ~samples:[]
      ; make_row
          ~chrom:"2"
          ~pos:14477084
          ~ids:[]
          ~ref:"C"
          ~alts:[ "<DEL:ME:ALU>" ]
          ~qual:(Some 12.0)
          ~filter:[]
          ~info:
            [ "IMPRECISE", [ `flag "IMPRECISE" ]
            ; "SVTYPE", [ `string "DEL" ]
            ; "END", [ `integer 14477381 ]
            ; "SVLEN", [ `integer (-297) ]
            ; "MEINFO", [ `string "AluYa5"; `string "5"; `string "307"; `string "+" ]
            ; "CIPOS", [ `integer (-22); `integer 18 ]
            ; "CIEND", [ `integer (-12); `integer 32 ]
            ]
          ~samples:[]
      ];
    [%expect {|
      true
      true
    |}]
  ;;

  let%expect_test "test_parse_vcf_samples" =
    test_parse_vcf_generic
      "vcf_06_samples.vcf"
      [ make_row
          ~chrom:"20"
          ~pos:14370
          ~ids:[ "rs6054257" ]
          ~ref:"G"
          ~alts:[ "A" ]
          ~qual:(Some 29.0)
          ~filter:[]
          ~info:
            [ "NS", [ `integer 3 ]
            ; "DP", [ `integer 14 ]
            ; "AF", [ `float 0.5 ]
            ; "DB", [ `flag "DB" ]
            ; "H2", [ `flag "H2" ]
            ]
          ~samples:
            [ ( "NA00001"
              , [ "GT", [ `string "0|0" ]
                ; "GQ", [ `integer 48 ]
                ; "DP", [ `integer 1 ]
                ; "HQ", [ `integer 51; `integer 51 ]
                ] )
            ; ( "NA00002"
              , [ "GT", [ `string "1|0" ]
                ; "GQ", [ `integer 48 ]
                ; "DP", [ `integer 8 ]
                ; "HQ", [ `integer 51; `integer 51 ]
                ] )
            ; ( "NA00003"
              , [ "GT", [ `string "1/1" ]
                ; "GQ", [ `integer 43 ]
                ; "DP", [ `integer 5 ]
                ; "HQ", [ `missing; `missing ]
                ] )
            ]
      ];
    [%expect {| true |}]
  ;;
end
