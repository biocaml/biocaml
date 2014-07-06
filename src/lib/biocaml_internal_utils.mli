(** Internal utility functions that are commonly needed in many
    places. Within Biocaml, all modules start with "open
    Core.Std". When needed, we can also open this module to get some
    extra functions, but it should not be opened unless it is
    specifically needed.
*)
open Core.Std

module Line : module type of Biocaml_line with type t = Biocaml_line.t
module Pos : module type of Biocaml_pos with type t = Biocaml_pos.t

module Stream : module type of CFStream_stream

val ( |? ) : 'a option -> 'a -> 'a

module Array : sig
  include module type of Core.Std.Array

  (** [range xs] is the stream of all valid indices in [xs] *)
  val range : 'a t -> int Stream.t
end

module Result : sig

  include module type of Core.Std.Result

  module List : sig

    (** Map the function [f] over the list, stopping on the first
        error encountered. *)
    val mapi: 'a list -> f:(int -> 'a -> ('b, 'e) t) -> ('b list, 'e) t
    val map: 'a list -> f:('a -> ('b, 'err) t) -> ('b list, 'err) t
  end

end

val try_finally_exn : fend:('a -> unit) -> ('a -> 'b) -> 'a -> 'b
  (** [try_finally_exn fend f a] will run [x = f a], then run [fend
      a], and finally return [x]. If [f a] raised an exception that
      exception will be returned even if [f x] raises an exception too. If
      [f a] successfully produces [x], then it is possible to get instead
      an exception raised by [fend a]. *)

val open_out_safe : string -> out_channel
  (** Like [open_out] but will not overwrite existing file. *)

(** Operations on URL-style encodings. *)
module Url : sig

  val escape: string -> string
  (** Convert non-alphanumeric characters to their ["%HX"]
      URL-escaping format. *)

  val unescape: string -> error:(string -> 'error) -> (string, 'error) Result.t
  (** Convert a string containing ["%HX"] escaped characters to a normal
      string. In case of error, the string is passed to the [~error] parameter
      and the function returns its result. *)

end

module Debug: sig

  val enable: string -> unit
  val disable: string -> unit

  val make : string -> ('a, unit, string, unit) format4 -> 'a

end
