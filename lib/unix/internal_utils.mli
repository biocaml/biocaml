(** Internal utility functions that are commonly needed in many
    places. Within Biocaml, all modules start with "open
    Core.Std". When needed, we can also open this module to get some
    extra functions, but it should not be opened unless it is
    specifically needed.
*)
open Core.Std

module Stream : module type of CFStream_stream

module Debug: sig

  val enable: string -> unit
  val disable: string -> unit

  val make : string -> ('a, unit, string, unit) format4 -> 'a

end
