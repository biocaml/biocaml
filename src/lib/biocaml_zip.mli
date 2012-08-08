
type unzip_error =
[ `garbage_at_end_of_compressed_data of string
| `wrong_gzip_header of
    [ `compression_method | `flags | `magic_number ] * int ]

val unzip: 
  ?format:[ `gzip | `raw ] ->
  ?zlib_buffer_size:int ->
  unit ->
  (string, string, unzip_error) Biocaml_transform.t
