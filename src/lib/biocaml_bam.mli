
type raw_alignment = {
  qname : string;
  flag : int;
  (* rname : string; *)
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
  
type bam_parse_error = [
| `read_name_not_null_terminated of string
| `reference_information_name_not_null_terminated of string
| `wrong_magic_number of string
]

val string_of_bam_parse_error : bam_parse_error -> string

val parser:
  ?zlib_buffer_size:int ->
  unit ->
  (string, raw_item,
   [`unzip of Biocaml_zip.unzip_error | `bam of bam_parse_error ] )
    Biocaml_transform.t

val debug : bool ref

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
     
