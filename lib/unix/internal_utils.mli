(** Internal utility functions that are commonly needed in many
    places. Within Biocaml, all modules start with "open
    Core.Std". When needed, we can also open this module to get some
    extra functions, but it should not be opened unless it is
    specifically needed.
*)
open Core.Std

module Stream : module type of CFStream_stream

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
