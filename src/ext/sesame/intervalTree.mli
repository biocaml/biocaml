type t
val empty : t
val create : (int * int) list -> t
val within : t -> int * int -> (int * int) list
val within_pt : t -> int -> (int * int) list
