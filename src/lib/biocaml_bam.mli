
type optional_content = [
| `array of optional_content array
| `char of char
| `float of float
| `int of int
| `string of string ]

type alignment = {
  qname : string;
  flag : int;
  (* rname : string; *)
  pos : int;
  mapq : int;
  cigar : (char * int) array;
  next_ref_id : int;
  pnext : int;
  tlen : int;
  seq : string;
  qual : int array;
  optional : (string * char * optional_content) list
}

type stream_item =
[ `alignment of alignment
| `header of string
| `reference_information of (string * int) array ]
  
type bam_parse_error = [
| `read_name_not_null_terminated of string
| `reference_information_name_not_null_terminated of string
| `wrong_auxiliary_data of
    [ `array_size of int
    | `null_terminated_hexarray
    | `null_terminated_string
    | `out_of_bounds
    | `unknown_type of char ] * string
| `wrong_cigar of string
| `wrong_magic_number of string
]

val string_of_bam_parse_error : bam_parse_error -> string

val parser:
  ?zlib_buffer_size:int ->
  unit ->
  (string, stream_item,
   [`unzip of Biocaml_zip.unzip_error | `bam of bam_parse_error ] )
    Biocaml_transform.t

val debug : bool ref
