open Biocaml_internal_pervasives
open Result

module Pos = Biocaml_pos

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
  | n   -> Number (int_of_string n)

type vcf_format_type = [ `integer
                       | `float
                       | `character
                       | `string
                       ]

let string_to_vcf_format_type s =
  match String.lowercase s with
  | "integer"   -> `integer
  | "float"     -> `float
  | "character" -> `character
  | "string"    -> `string
  | v           -> failwith ("string_to_vcf_format_type: invalid format: " ^ v)

type vcf_info_type = [ vcf_format_type | `flag ]

let string_to_vcf_info_type = function
  | "Flag" -> `flag
  | t      -> string_to_vcf_format_type t

type vcf_alt_type =
  | Deletion
  | Insertion
  | Duplication
  | Inversion
  | CNV

type vcf_alt_subtype = string

type vcf_info_meta =
  Info of vcf_id * vcf_number * vcf_info_type * vcf_description
type vcf_filter_meta =
  Filter of vcf_id * vcf_description
type vcf_format_meta =
  Format of vcf_id * vcf_number * vcf_format_type * vcf_description
type vcf_alt_meta =
  Alt of vcf_alt_type * vcf_alt_subtype list * vcf_description

type vcf_meta = {
  vcfm_version : string;
  vcfm_info    : vcf_info_meta list;
  vcfm_filter  : vcf_filter_meta list;
  vcfm_format  : vcf_format_meta list;
  vcfm_alt     : vcf_alt_meta list;
  vcfm_arbitrary : (string, string) Hashtbl.t;
  vcfm_header  : string list
}

let default_meta = {
  vcfm_version = "unknown";
  vcfm_info = []; vcfm_filter = [];
  vcfm_format = []; vcfm_alt = [];
  vcfm_arbitrary = Hashtbl.Poly.create ();
  vcfm_header = []
}

type vcf_row = {
  vcfr_chrom : string; (* FIXME(superbobry): Biocaml_chrName.t *)
  vcfr_pos   : int;
  vcfr_id    : string list;
  vcfr_ref   : string;
  vcfr_alt   : string list;
  vcfr_qual  : float option;
  vcfr_filter : vcf_id list;
  vcfr_info  : (string, string) Hashtbl.t  (* FIXME(superbobry): proper typing *)
}

let default_row = {
  vcfr_chrom = "";
  vcfr_pos   = -1;
  vcfr_id    = [];
  vcfr_ref   = "N";
  vcfr_alt   = [];
  vcfr_qual  = None;
  vcfr_filter = [];
  vcfr_info  = Hashtbl.Poly.create ()
}

let rec list_to_vcf_row meta chunks =
  match chunks with
  | [vcfr_chrom; raw_pos; raw_id; raw_ref; raw_alt; raw_qual; raw_filter; raw_info]
    when List.length chunks = List.length meta.vcfm_header ->
    let open Option.Monad_infix in
    string_to_vcfr_ref raw_ref >>= fun vcfr_ref ->
    (** TODO(superbobry): parse INFO properly. *)
    let row = {
      default_row with
      vcfr_chrom;
      vcfr_pos  = Int.of_string raw_pos;
      vcfr_id   = String.split ~on:';' raw_id;  (** Ensure uniqueness. *)
      vcfr_ref;
      vcfr_alt  = String.split ~on:',' raw_alt; (** ACGT, <ID>. *)
      vcfr_qual = Some (Float.of_string raw_qual);
      vcfr_filter = String.split ~on:';' raw_filter;  (** Proper typing. *)
    } in Some row
  | c ->

    (** Note(superbobry): arbitary-width rows aren't supported yet. *)
    None

and string_to_vcfr_ref s =
  let s = String.uppercase s in
  if String.fold s
      ~f:(fun acc ch -> acc && String.contains "ACGTN" ch)
      ~init:true
  then Some s
  else None

type item = vcf_row

type vcf_parse_error =
  [ `malformed_meta of Pos.t * string
  | `malformed_row of Pos.t * string
  | `missing_header of Pos.t
  | `incomplete_input of Pos.t * Biocaml_lines.item list * string option
  | `not_ready
  ]

let parse_error_to_string =
  let pos () a = Pos.to_string a in
  function
  | `malformed_meta (p, s) -> sprintf "malformed_meta (%a, %s)" pos p s
  | `malformed_row (p, s) -> sprintf "malformed_row (%a, %s)" pos p s
  | `missing_header p -> sprintf "missing_header (%a)" pos p
  | _ -> sprintf "unknown error"

module Transform = struct
  let next_vcf_meta p =
    let open Biocaml_line in
    let open Biocaml_lines.Buffer in
    let rec go meta =
      let {vcfm_version; vcfm_info; vcfm_filter; vcfm_format; vcfm_alt; _} = meta
      in match Option.map ~f:line_to_string (peek_line p) with
      | Some l when String.is_prefix l ~prefix:"##" ->
        let _l = next_line p in
        let s  = String.suffix l (String.length l - 2) in
        begin
          match String.lsplit2 s ~on:'=' with
          | Some ("fileformat", v) -> go { meta with vcfm_version = v }
          | Some ("INFO", v) ->
            Scanf.sscanf v "<ID=%s@,Number=%s@,Type=%s@,Description=%S>"
              (fun id n t description ->
                let info_meta = Info (id,
                    string_to_vcf_number n,
                    string_to_vcf_info_type t,
                    description)
                in go { meta with vcfm_info = info_meta :: vcfm_info })
          | Some ("FILTER", v) ->
            Scanf.sscanf v "<ID=%s@,Description=%S>"
              (fun id description ->
                let filter_meta = Filter (id, description) in
                go { meta with vcfm_filter = filter_meta :: vcfm_filter })
          | Some ("FORMAT", v) ->
            Scanf.sscanf v "<ID=%s@,Number=%s@,Type=%s@,Description=%S>"
              (fun id n t description ->
                let format_meta = Format (id,
                    string_to_vcf_number n,
                    string_to_vcf_format_type t,
                    description)
                in go { meta with vcfm_format = format_meta :: vcfm_format })
          | Some ("ALT", v) -> failwith "not implemented"
          | Some (k, v) -> begin
              Hashtbl.set meta.vcfm_arbitrary ~key:k ~data:v;
              go meta
            end
          | None -> Error (`malformed_meta (current_position p, s))
        end
      | Some l ->
        let _l = next_line p in
        (** Note(superbobry): if the line *does not* start with '##' it
            must be a header. *)
        let chunks =
          List.filter ~f:(fun s -> s <> "") (String.split ~on:' ' l)
        in begin match chunks with
        | "#CHROM" :: "POS" :: "ID" :: "REF" :: "ALT" :: "QUAL" ::
          "FILTER" :: "INFO" :: "FORMAT" :: _
        | ["#CHROM"; "POS"; "ID"; "REF"; "ALT"; "QUAL"; "FILTER"; "INFO"] ->
          Ok { meta with vcfm_header = chunks }
        | _ -> Error (`missing_header (current_position p))
        end
      | None -> Error `not_ready
    in go default_meta

  let next_vcf_row meta p =
    let open Biocaml_line in
    let open Biocaml_lines.Buffer in
    match Option.map ~f:line_to_string (next_line p) with
    | Some l when not (String.is_empty l) ->
      let chunks =
        List.filter ~f:(fun s -> s <> "") (String.split ~on:' ' l)
      in begin match list_to_vcf_row meta chunks with
      | Some row -> output_ok row
      | None -> output_error (`malformed_row (current_position p, l))
      end
    | Some _ | None -> `not_ready  (* All done, boss! *)

  let rec next meta_ref p =
    match !meta_ref with
    | Some meta -> next_vcf_row meta p
    | None -> begin match next_vcf_meta p with
      | Ok meta ->
        (** Note(superbobry): parse meta and demand more input. *)
        meta_ref := Some meta; `not_ready
      | Error `not_ready -> `not_ready
      | Error err -> output_error err
      end

  let string_to_item ?filename () =
    let name = sprintf "vcf_parser:%s" Option.(value ~default:"<>" filename) in
    let meta_ref = ref None in
    Biocaml_lines.Transform.make_merge_error ~name ?filename ~next:(next meta_ref) ()
end