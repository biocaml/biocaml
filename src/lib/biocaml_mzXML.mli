(** mzXML files (mass spectrometry data format).
 *)

val decode : precision:int -> string -> float array * float array
