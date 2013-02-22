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

let string_to_vcf_format_type = function
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

type vcf_meta =
  | Version of string
  | Info of vcf_id * vcf_number * vcf_info_type * vcf_description
  | Filter of vcf_id * vcf_description
  | Format of vcf_id * vcf_number * vcf_format_type * vcf_description
  | Alt of vcf_alt_type * vcf_alt_subtype list * vcf_description
  | Arbitrary of string * string

type vcf_row = {
  chrom : int;
  pos   : int;
  id    : string list;
  ref   : string list;
  alt   : string list;
  qual  : float option;
  filter : vcf_id list;
  info  : (string, string) Hashtbl.t  (* FIXME: proper typing *)
}

type t = [`meta of vcf_meta | `row of vcf_row]

type vcf_parse_error =
  [ `empty_line of Pos.t
  | `malformed_meta of Pos.t * string
  | `incomplete_input of Pos.t * string list * string option
  ]

let parse_error_to_string =
  let pos () a = Pos.to_string a in
  function
    | `empty_line p -> sprintf "empty_line (%a)" pos p
    | `malformed_meta (p, s) -> sprintf "malformed_meta (%a, %s)" pos p s

module Transform = struct
  let rec next p =
    (* Note(superbobry): this only parses the header for now. *)
    let open Biocaml_transform.Line_oriented in
        match next_line p with
          | Some "" -> output_error (`empty_line (current_position p))
          | Some l when String.is_prefix l ~prefix:"##" ->
            let s = String.suffix l 2 in
            begin match String.lsplit2 s ~on:'=' with
              | Some ("fileformat", v) -> output_ok (`meta (Version v))
              | Some ("INFO", v) ->
                Scanf.sscanf v "<ID=%s,Number=%s,Type=%s,Description=%s>"
                (fun id n t description ->
                  output_ok (`meta (Info (id,
                                          string_to_vcf_number n,
                                          string_to_vcf_info_type t,
                                          description))))
              | Some ("FILTER", v) ->
                Scanf.sscanf v "<ID=%s,Description=%s>"
                (fun id description -> output_ok (`meta (Filter (id, description))))
              | Some ("FORMAT", v) ->
                Scanf.sscanf v "<ID=%s,Number=%s,Type=%s,Description=%s>"
                (fun id n t description ->
                  output_ok (`meta (Format (id,
                                            string_to_vcf_number n,
                                            string_to_vcf_format_type t,
                                            description))))
              | Some ("ALT", v) -> failwith "not implemented"
              | Some (k, v) -> output_ok (`meta (Arbitrary (k, v)))
              | None -> output_error (`malformed_meta (current_position p, s))
            end
          | Some l ->
            failwith "next: row parsing is not implemented yet"
          | None -> `not_ready

  let string_to_t ?filename () =
    let name = sprintf "vcf_parser:%s" Option.(value ~default:"<>" filename) in
    Biocaml_transform.Line_oriented.make_merge_error ~name ?filename ~next ()
end
