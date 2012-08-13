(** Streaming interface to the Zlib library. *)

(**

   The module deals with compressed streams.
   - The DEFLATE compression algorithm is defined in the
   {{:http://www.ietf.org/rfc/rfc1951.txt}RFC 1951}.
   - GZIP file-format is defined in the
   {{:http://www.ietf.org/rfc/rfc1952.txt}RFC 1952}
   (c.f. also
   {{:http://en.wikipedia.org/wiki/Gzip}wikipedia:Gzip}).

*)


type unzip_error =
[ `garbage_at_end_of_compressed_data of string
| `wrong_gzip_header of
    [ `compression_method | `flags | `magic_number ] * int ]
(** The possible unzipping errors. *)

val unzip: 
  ?format:[ `gzip | `raw ] ->
  ?zlib_buffer_size:int ->
  unit ->
  (string, string, unzip_error) Biocaml_transform.t
(** Create a transform that uncompresses a stream.
    The default [format] is [`raw] (i.e. only apply the "deflate"
    algorithm to the stream); [`gzip] means that the transform must first
    skip a gzip header. *)
