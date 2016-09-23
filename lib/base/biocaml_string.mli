(** Extension to the stdlib's String module

    Most parts of this module interface/implementation are taken from
    Janestreet's base libray
 *)

include module type of BytesLabels

(** [slice s start stop] gets a slice of [s] between [start] and [stop].
    [start] and [stop] will be normalized before the access. *)
val slice : t -> int -> int -> t

val split : t -> on:char -> t list

(** [rsplit2 line ~on] optionally returns [line] split into two strings around the
    first appearance of [on] from the right *)
val rsplit2 : t -> on:char -> (t * t) option
