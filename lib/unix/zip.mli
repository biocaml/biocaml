(** Streaming interface to the Zlib library. *)
open Core_kernel.Std

(**

   The module deals with compressed streams.
   - The DEFLATE compression algorithm is defined in the
   {{:http://www.ietf.org/rfc/rfc1951.txt}RFC 1951}.
   - GZIP file-format is defined in the
   {{:http://www.ietf.org/rfc/rfc1952.txt}RFC 1952}
   (c.f. also
   {{:http://en.wikipedia.org/wiki/Gzip}wikipedia:Gzip}).

*)

(** {2 Default Parameters} *)

module Default: sig
  (** This module contains the default values for most optional
     parameters of the module. *)

  (** The default size of the internal buffer used by the ZLib library.
     It's value is 4096. *)
  val zlib_buffer_size : int

  (** The default compression level used in [Transform.zip]. It's
     value is 3, bigger values (up to 9) decrease performance while
     slightly improving the compression.  *)
  val level : int

end

(** {2 Error Types} *)

module Error: sig

  type unzip =
  [ `garbage_at_end_of_compressed_data of string
  | `zlib of string
  | `wrong_gzip_header of
      [ `compression_method | `flags | `magic_number ] * int ]
  (** The possible unzipping errors. *)

  type t = unzip
  (** The union of the errors. *)


  val unzip_of_sexp: Sexplib.Sexp.t -> unzip
  val sexp_of_unzip: unzip -> Sexplib.Sexp.t
  val t_of_sexp: Sexplib.Sexp.t -> t
  val sexp_of_t: t -> Sexplib.Sexp.t
end

(** {2 [In_chanel.t] Functions} *)


val unzip_in_channel :
  ?format:[ `gzip | `raw ] -> ?zlib_buffer_size:int ->
  ?buffer_size:int -> in_channel ->
  (string, [> Error.t]) Result.t Stream.t
(** Decompress an Input Channel. *)

val zip_in_channel :
  ?format:[ `gzip | `raw ] -> ?zlib_buffer_size:int -> ?level:int ->
  ?buffer_size:int -> in_channel ->
  string Stream.t
(** Compress an Input Channel. *)

exception Error of Error.unzip
(** The exception raise by the [*_exn] functions of this module. *)

val unzip_in_channel_exn :
  ?format:[ `gzip | `raw ] -> ?zlib_buffer_size:int ->
  ?buffer_size:int -> in_channel ->
  string Stream.t
(** Like [unzip_in_channel] but calls to [Stream.next] may raise
    [Error e] exceptions. *)

(** {2 [Transform.t] Implementations} *)

module Transform: sig

  val unzip:
    ?format:[ `gzip | `raw ] ->
    ?zlib_buffer_size:int ->
    unit ->
    (string, (string, [> Error.unzip]) Result.t) Tfxm.t
  (** Create a transform that uncompresses a stream.
      The default [format] is [`raw] (i.e. only apply the "deflate"
      algorithm to the stream); [`gzip] means that the transform must first
      skip a gzip header. *)

  val zip :
    ?format:[ `gzip | `raw ] ->
    ?level:int ->
    ?zlib_buffer_size:int ->
    unit ->
    (string, string) Tfxm.t
  (** Create a transform that writes compressed data. *)


end
