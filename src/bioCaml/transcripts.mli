
(* This module uses zero based, half-open intervals *)

type 'a transcript = 
    { 
      exons : (int * int) list; 
      lo : int; 
      hi : int;
      chr : string;
      info : 'a
    }

type 'a t = 'a transcript list

val of_composite_file : ?chr_map:(string -> string) -> ?increment_lo_hi:(int * int) -> string -> string t

val of_bed_file : ?chr_map:(string -> string) -> ?increment_lo_hi:(int * int) ->string -> string t

val all_probes_in : 'a t -> (string * int * int * float) list -> ('a * float array) t

val all_points_in : 'a t -> (string * int * float) list -> ('a * float array) t
