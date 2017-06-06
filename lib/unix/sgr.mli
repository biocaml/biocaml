(** Sequence Graph (SGR) files. *)

type t

exception Bad of string

val of_channel : ?chr_map:(string -> string) -> ?increment_bp:int -> in_channel -> t
val of_file : ?chr_map:(string -> string) -> ?increment_bp:int -> string -> t
val of_list : (string * int * float) list -> t
val of_chr_lists : (string * (int * float) list) list -> t

val to_channel : ?chr_map:(string -> string) -> ?increment_bp:int -> t -> out_channel -> unit
val to_file : ?chr_map:(string -> string) -> ?increment_bp:int -> t -> string -> unit
  (** Items will be printed in ascending order by [(chr,coord)]. *)

val to_list : t -> (string * int * float) list
  (** Items will be returned in ascending order by [(chr,coord)]. *)

val to_chr_lists : t -> (string * (int * float) list) list
  (** Outer list will be in ascending order by [chr], and inner lists will be in ascending order by [coord]. *)
