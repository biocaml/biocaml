type count_matrix = int array array
type background = float array
type t = private float array array

val flat_background : unit -> background
val background_of_sequence : string -> float -> background

val make : count_matrix -> background -> t

val tandem : 
  ?orientation:[`direct | `inverted | `everted] ->
  spacer:int ->
  count_matrix -> count_matrix -> background -> t

val reverse_complement : t -> t

val scan : t -> string -> float -> (int * float) list

val stub_scan : t -> string -> float -> (int * float) list