(** Sequence Graph (SGR) files. *)

type t

exception Bad of string

val of_file : string -> t option
val of_file_exn : string -> t
val of_list : (string * int * float) list -> t
val of_chr_lists : (string * (int * float) list) list -> t
  
val to_file : string -> t -> unit
  (** Items will be printed in ascending order by [(chr,coord)]. *)
  
val to_list : t -> (string * int * float) list
  (** Items will be returned in ascending order by [(chr,coord)]. *)
  
val to_chr_lists : t -> (string * (int * float) list) list
  (** Outer list will be in ascending order by [chr], and inner lists will be in ascending order by [coord]. *)
