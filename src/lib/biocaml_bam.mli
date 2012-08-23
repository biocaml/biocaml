
type raw_alignment = {
  qname : string;
  flag : int;
  ref_id: int;
  pos : int;
  mapq : int;
  bin: int;
  cigar : string;
  next_ref_id : int;
  pnext : int;
  tlen : int;
  seq : string;
  qual : int array;
  optional : string;
}

type raw_item =
[ `alignment of raw_alignment
| `header of string
| `reference_information of (string * int) array ]
  
type raw_parsing_error = [
| `read_name_not_null_terminated of string
| `reference_information_name_not_null_terminated of string
| `wrong_magic_number of string
]

val string_of_raw_parsing_error : raw_parsing_error -> string

val raw_parser:
  ?zlib_buffer_size:int ->
  unit ->
  (string, raw_item,
   [`unzip of Biocaml_zip.unzip_error | `bam of raw_parsing_error ] )
    Biocaml_transform.t

type parse_optional_error = [
| `wrong_auxiliary_data of
      [ `array_size of int
      | `null_terminated_hexarray
      | `null_terminated_string
      | `out_of_bounds
      | `unknown_type of char ] * string
]
val parse_optional: ?pos:int -> ?len:int -> string ->
  (Biocaml_sam.optional_content, parse_optional_error) Core.Result.t
    
type parse_cigar_error = [
| `wrong_cigar of string
| `wrong_cigar_length of int ]

val parse_cigar: ?pos:int -> ?len:int -> string ->
  (Biocaml_sam.cigar_op array, parse_cigar_error) Core.Result.t
     

    
val item_parser :
  unit ->
  (raw_item, Biocaml_sam.item,
   [> `header_line_not_first of int
   | `header_line_without_version of (string * string) list
   | `header_line_wrong_sorting of string
   | `invalid_header_tag of int * string
   | `invalid_tag_value_list of int * string list
   | `reference_sequence_not_found of raw_alignment
   | parse_optional_error
   | parse_cigar_error
   | `wrong_flag of raw_alignment
   | `wrong_mapq of raw_alignment
   | `wrong_pnext of raw_alignment
   | `wrong_pos of raw_alignment
   | `wrong_qname of raw_alignment
   | `wrong_tlen of raw_alignment ]) Biocaml_transform.t

val downgrader :
  unit ->
  (Biocaml_sam.item, raw_item,
   [> `cannot_get_sequence of Biocaml_sam.alignment
   | `header_line_not_first of string
   | `reference_name_not_found of Biocaml_sam.alignment * string ])
    Biocaml_transform.t
