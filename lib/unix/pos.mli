(** File positions. A position within a file is defined by:

    [source] - Name of the file or other kind of source such as a URL.

    [line] - Line number within the [source]. The first line is
    numbered 1. Set to [None] for binary files where the concept of a
    line isn't applicable.

    [offset] - If a [line] number is given, this is the position from
    the start of the line. The first position is 1. If no line number
    is given, this is the offset from the beginning of [source]. The
    exact semantics of [offset] depends on the type of [source]. For
    example, for Unicode text files, the offset might be the character
    position instead of a byte position.

    It is valid to omit any field. Omitting all fields denotes a dummy
    or unknown position. Omitting [source] while providing [line] or
    [offset] is probably not sensible but isn't disallowed. Even if
    the source is an unnamed entity, some descriptive text should be
    provided, e.g. "stdin" is better than saying None. Negative values
    for [line] and [offset] also shouldn't be used, but we do not
    bother disallowing it.
*)
open Core_kernel

type t = {
  source : string option;
  line : int option;
  offset : int option;
} [@@deriving sexp]

val make: ?source:string -> ?line:int -> ?offset:int -> unit -> t

(** Position with all fields set to None. *)
val unknown : t

val incr_line : ?n:int -> t -> t
(** [incr_line ?n pos] increments the line number of [pos] by
    [n]. Default: [n = 1]. If [pos.line = None], it is treated as
    zero, i.e. the returned line number is set to [n]. *)

(** Print string in a human legible format. No particular format is
    guaranteed. *)
val to_string : t -> string
