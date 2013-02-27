open Biocaml_internal_pervasives
open Result

module Pos = Biocaml_pos

module Safe = struct
  type error = [ `invalid_int of string
               | `invalid_float of string
               ]

  let int_of_string s =
    try Ok (Int.of_string s)
    with Failure _ -> Error (`invalid_int s)

  let float_of_string s =
    try Ok (Float.of_string s)
    with Failure _ -> Error (`invalid_float s)
end

let is_valid_dna = String.for_all ~f:(String.contains "ACGTN")

type vcf_id = string
type vcf_description = string
type vcf_number =
  | Number of int
  | OnePerAllele
  | OnePerGenotype
  | Unknown

let string_to_vcf_number = function
  | "A" -> OnePerAllele
  | "G" -> OnePerGenotype
  | "." -> Unknown
  | arity -> Number (int_of_string arity)

type vcf_format_type = [ `integer_value
                       | `float_value
                       | `character_value
                       | `string_value
                       ]

let string_to_vcf_format_type s =
  match String.lowercase s with
  | "integer"   -> `integer_value
  | "float"     -> `float_value
  | "character" -> `character_value
  | "string"    -> `string_value
  | v           -> failwith ("string_to_vcf_format_type: invalid format: " ^ v)

let coerce_to_vcf_format_type t s =
  let open Result.Monad_infix in
  match t with
  | `integer_value ->
    Result.map (Safe.int_of_string s) ~f:(fun x -> `integer x)
  | `float_value   ->
    Result.map (Safe.float_of_string s) ~f:(fun x -> `float x)
  | `character_value when String.length s = 1 -> Ok (`character s.[0])
  | `string_value  -> return (`string s)
  | _ -> fail (`info_type_coersion_failure (t, s))

type vcf_info_type = [ vcf_format_type | `flag_value ]

let string_to_vcf_info_type s =
  match String.lowercase s with
  | "flag" -> `flag_value
  | s      -> string_to_vcf_format_type s

let coerce_to_vcf_info_type t s =
  match t with
  | `flag_value -> Ok (`flag s)
  | t           -> coerce_to_vcf_format_type t s

type vcf_alt_type =
  | Deletion
  | Insertion
  | Duplication
  | Inversion
  | CNV

type vcf_alt_subtype = string

type vcf_info_meta = Info of vcf_number * vcf_info_type * vcf_description
type vcf_filter_meta = Filter of vcf_description
type vcf_format_meta =
  Format of vcf_number * vcf_format_type * vcf_description
type vcf_alt_meta =
  Alt of vcf_alt_type * vcf_alt_subtype list * vcf_description

type vcf_meta = {
  vcfm_version : string;
  vcfm_id_cache: vcf_id Set.Poly.t;
  vcfm_info    : (vcf_id, vcf_info_meta) Hashtbl.t;
  vcfm_filters : (vcf_id * vcf_filter_meta) list;
  vcfm_format  : (vcf_id, vcf_format_meta) Hashtbl.t;
  vcfm_alt     : vcf_alt_meta list;
  vcfm_arbitrary : (string, string) Hashtbl.t;
  vcfm_header  : string list
}

let default_meta = {
  vcfm_version = "<unknown>";
  vcfm_id_cache = Set.Poly.empty;
  vcfm_info = Hashtbl.Poly.create ();
  vcfm_filters = [];
  vcfm_format = Hashtbl.Poly.create ();
  vcfm_alt = [];
  vcfm_arbitrary = Hashtbl.Poly.create ();
  vcfm_header = []
}

type vcf_format = [ `integer of int
                  | `float of float
                  | `character of char
                  | `string of string
                  ]
type vcf_info = [ vcf_format | `flag of string ]

type vcf_row = {
  vcfr_chrom : string; (* FIXME(superbobry): Biocaml_chrName.t *)
  vcfr_pos   : int;
  vcfr_ids    : string list;
  vcfr_ref   : string;
  vcfr_alts  : string list;
  vcfr_qual  : float option;
  vcfr_filter : vcf_id list;
  vcfr_info  : (vcf_id, vcf_info list) Hashtbl.t
}

type item = vcf_row

type vcf_parse_row_error =
  [  Safe.error
  | `info_type_coersion_failure of vcf_info_type * string
  | `invalid_dna of string
  | `unknown_info of vcf_id
  | `unknown_filter of vcf_id
  | `duplicate_ids of vcf_id list
  | `invalid_arguments_length of vcf_id * int * int
  | `arbitrary_width_rows_not_supported
  ]

type vcf_parse_error =
  [ `malformed_meta of Pos.t * string
  | `malformed_row of Pos.t * vcf_parse_row_error * string
  | `malformed_header of Pos.t * string
  | `alt_parsing_not_implemented of Pos.t
  | `arbitrary_width_rows_not_supported of Pos.t
  | `incomplete_input of Pos.t * Biocaml_lines.item list * string option
  | `not_ready
  ]

let string_to_vcfr_ref s =
  let open Result.Monad_infix in
  let s = String.uppercase s in
  if is_valid_dna s
  then return s
  else fail (`invalid_dna s)

let string_to_vcfr_info { vcfm_info } s =
  let rec string_to_t raw_value t =
    let res =
      List.map (String.split ~on:',' raw_value) ~f:(coerce_to_vcf_info_type t)
    in Result.all res
  and go values =
    List.map (String.split ~on:';' s) ~f:(fun chunk ->
      let (key, raw_value) =
        Option.value ~default:(chunk, "") (String.lsplit2 ~on:'=' chunk)
      in

      let open Result.Monad_infix in
      let chunk_values = match Hashtbl.find vcfm_info key with
      | Some (Info (Number 0, `flag_value, _description))
        when raw_value = "" -> return [`flag key]
      | Some (Info (Number n, t, _description)) ->
        string_to_t raw_value t >>= fun values ->
        if List.length values = n
        then return values
        else fail (`invalid_arguments_length (key, List.length values, n))
      | Some (Info (Unknown, t, _description)) ->
        string_to_t raw_value t
      | Some (Info (OnePerAllele, t, _description)) ->
        (** TODO(superbobry): how do we know the nr. of alleles? *)
        string_to_t raw_value t
      | Some (Info (OnePerGenotype, t, _description)) ->
        (** TODO(superbobry): how to make sure that we have exactly _one_
            value per genotype? *)
        string_to_t raw_value t
      | None -> fail (`unknown_info key)
      in Result.map chunk_values ~f:(fun data -> Hashtbl.set values ~key ~data))
  and values = Hashtbl.Poly.create ()
  in Result.(map (all (go values)) ~f:(Fn.const values))

let string_to_vcfr_filter { vcfm_filters } s =
  let open Result.Monad_infix in
  match String.split ~on:';' s with
  | ["PASS"] -> return []
  | chunks ->
    match List.find chunks
        ~f:(fun chunk -> not (List.Assoc.mem vcfm_filters chunk)) with
    | Some unknown_filter -> fail (`unknown_filter unknown_filter)
    | None -> return chunks

let string_to_vcfr_ids { vcfm_id_cache } s =
  let open Result.Monad_infix in
  match String.split ~on:';' s with
  | ["."] -> return []
  | chunks ->
    let duplicate_ids = List.filter chunks ~f:(Set.mem vcfm_id_cache)
    in if List.is_empty duplicate_ids
    then return chunks
    else fail (`duplicate_ids duplicate_ids)

let string_to_vcfr_alts s =
  let open Result.Monad_infix in
  match String.split ~on:',' s with
  | ["."]  -> return []
  | chunks ->
    match List.find chunks ~f:(fun chunk -> not (is_valid_dna chunk)) with
    | Some invalid -> fail (`invalid_dna invalid)
    | None -> return chunks

let list_to_vcf_row meta chunks =
  match chunks with
  | [vcfr_chrom; raw_pos; raw_id; raw_ref; raw_alt; raw_qual; raw_filter; raw_info]
    when List.length chunks = List.length meta.vcfm_header ->
    let open Result.Monad_infix in
    Safe.int_of_string raw_pos >>= fun vcfr_pos ->
    string_to_vcfr_ids meta raw_id >>= fun vcfr_ids ->
    string_to_vcfr_ref raw_ref >>= fun vcfr_ref ->
    string_to_vcfr_alts raw_alt >>= fun vcfr_alts ->
    string_to_vcfr_info meta raw_info >>= fun vcfr_info ->
    string_to_vcfr_filter meta raw_filter >>= fun vcfr_filter ->
    let row = {
      vcfr_chrom; vcfr_pos; vcfr_ids; vcfr_ref; vcfr_alts;
      vcfr_qual = Result.ok (Safe.float_of_string raw_qual);
      vcfr_filter; vcfr_info
    } in return row
  | c -> fail `arbitrary_width_rows_not_supported

let parse_row_error_to_string : vcf_parse_row_error -> string = function
| `invalid_int s -> sprintf "invalid_integer (%s)" s
| `invalid_float s -> sprintf "invalid_float (%s)" s
| `info_type_coersion_failure (t, s) ->
  let t = match t with
  | `integer_value -> "integer"
  | `float_value   -> "float"
  | `character_value -> "character"
  | `string_value -> "string"
  | `flag_value -> "flag"
  in sprintf "info_type_coersion_failure (%s, %S)" t s
| `invalid_dna s -> sprintf "invalid_dna (%s)" s
| `unknown_info s -> sprintf "unknown_info (%s)" s
| `unknown_filter f -> sprintf "unknown_filter (%s)" f
| `duplicate_ids ids ->
  sprintf "duplicate_ids (%s)" (String.concat ~sep:", " ids)
| `invalid_arguments_length (key, got, expected) ->
  sprintf "invalid_arguments_length (%s, %i, %i)" key got expected
| `arbitrary_width_rows_not_supported -> "arbitrary_width_rows_not_supported"

let parse_error_to_string : vcf_parse_error -> string =
  let pos () a = Pos.to_string a in function
  | `malformed_meta (p, s) -> sprintf "malformed_meta (%a, %S)" pos p s
  | `malformed_row (p, err, s) ->
    sprintf "malformed_row (%s, %a, %S)" (parse_row_error_to_string err) pos p s
  | `malformed_header (p, s) -> sprintf "malformed_header (%a, %s)" pos p s
  | `alt_parsing_not_implemented p ->
    sprintf "alt_parsing_not_implemented (%a)" pos p
  | `arbitrary_width_rows_not_supported p ->
    sprintf "arbitrary_width_rows_not_supported (%a)" pos p
  | _ -> sprintf "unknown_error"

module Transform = struct
  let next_vcf_meta meta p =
    let open Biocaml_line in
    let open Biocaml_lines.Buffer in
    let { vcfm_version; vcfm_info; vcfm_filters; vcfm_format; vcfm_alt; _ } = meta
    in match Option.map ~f:line_to_string (peek_line p) with
    | Some l when String.is_prefix l ~prefix:"##" ->
      let _l = next_line p in
      let s  = String.suffix l (String.length l - 2) in
      let open Result.Monad_infix in
      begin
        match String.lsplit2 s ~on:'=' with
        | Some ("fileformat", v) ->
          return (`partial { meta with vcfm_version = v })
        | Some ("INFO", v) ->
          Scanf.sscanf v "<ID=%s@,Number=%s@,Type=%s@,Description=%S>"
            (fun id n t description ->
              let info_meta = Info (string_to_vcf_number n,
                  string_to_vcf_info_type t,
                  description)
              in Hashtbl.set vcfm_info id info_meta);
          return (`partial meta)
        | Some ("FILTER", v) ->
          Scanf.sscanf v "<ID=%s@,Description=%S>"
            (fun id description ->
              let filter_meta = Filter description in
              let meta = { meta with
                           vcfm_filters = (id, filter_meta) :: vcfm_filters }
              in return (`partial meta))
        | Some ("FORMAT", v) ->
          Scanf.sscanf v "<ID=%s@,Number=%s@,Type=%s@,Description=%S>"
            (fun id n t description ->
              let format_meta = Format (string_to_vcf_number n,
                  string_to_vcf_format_type t,
                  description)
              in Hashtbl.set vcfm_format id format_meta);
          return (`partial meta)
        | Some ("ALT", v) ->
          fail (`alt_parsing_not_implemented (current_position p))
        | Some (k, v) -> begin
            Hashtbl.set meta.vcfm_arbitrary ~key:k ~data:v;
            return (`partial meta)
          end
        | None -> fail (`malformed_meta (current_position p, s))
      end
    | Some l ->
      let _l = next_line p in
      (** Note(superbobry): if the line *does not* start with '##' it
          must be a header. *)
      let chunks =
        List.filter ~f:(fun s -> s <> "") (String.split ~on:' ' l)
      in begin match chunks with
      | "#CHROM" :: "POS" :: "ID" :: "REF" :: "ALT" :: "QUAL" ::
          "FILTER" :: "INFO" :: rest ->
        begin match rest with
        | "FORMAT" :: _ ->
          fail (`arbitrary_width_rows_not_supported (current_position p))
        | _ :: _ -> fail (`malformed_header (current_position p, l))
        | [] -> return (`complete { meta with vcfm_header = chunks })
        end
      | _ -> fail (`malformed_header (current_position p, l))
      end
    | None -> fail `not_ready

  let next_vcf_row meta p =
    let open Biocaml_line in
    let open Biocaml_lines.Buffer in
    match Option.map ~f:line_to_string (next_line p) with
    | Some l when not (String.is_empty l) ->
      let chunks =
        List.filter ~f:(fun s -> s <> "") (String.split ~on:' ' l)
      in begin match list_to_vcf_row meta chunks with
      | Ok row -> output_ok row
      | Error err -> output_error (`malformed_row (current_position p, err, l))
      end
    | Some _ | None -> `not_ready  (* All done, boss! *)

  let rec next meta_ref p =
    match !meta_ref with
    | `complete meta -> next_vcf_row meta p
    | `partial meta  -> begin match next_vcf_meta meta p with
      | Ok meta -> meta_ref := meta; `not_ready
      | Error `not_ready -> `not_ready
      | Error err -> output_error err
      end

  let string_to_item ?filename () =
    let name = sprintf "vcf_parser:%s" Option.(value ~default:"<>" filename) in
    let meta_ref = ref (`partial default_meta) in
    Biocaml_lines.Transform.make_merge_error ~name ?filename ~next:(next meta_ref) ()
end
