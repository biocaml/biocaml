(** Color values. *)

type t
    (** Type of a color. *)

val of_rgb : int -> int -> int -> t
  (** Construct color from given RGB values. Raise [Failure] if invalid values given. *)
  
val to_rgb_string : t -> string
  (** Return string representing the color as an RGB value. For example, [to_rgb_string black] returns "0,0,0". *)
  
val black : t

val yale_blue : t
