(** BED data *)

type pt = string * int * int
    (** A data point is a triple [(chr,lo,hi)] defining an interval [\[lo, hi\]] on chromosome [chr]. *)
    
type t
    (** Type of BED data. Can be thought of as a collection of [pt]'s. Intervals cannot overlap for each chromosome. *)
    
exception Bad of string
  
val get_chr : string -> t -> (int * int) list

val of_file : string -> t
  (** Parse given file. Raise [Bad] if any errors. *)
  
val of_channel : in_channel -> t
  (** Parse given channel. Raise [Bad] if any errors. *)

val of_list: pt list -> t
  (** Construct data set from given [pt]'s. Data can be in any order, but [Bad] will be raised if data is ill-formed. *)
  
val to_list : t -> pt list
  (** Extract data as a flat list. *)
  
val to_lists : t -> (string * (int * int) list) list
  (** Extract data as a list of intervals for each chromosome. *)
  
val of_line : string -> pt  

val to_file : t -> string -> unit
val to_channel : t -> out_channel -> unit


(** {6 Dynamic Data Set Construction} *)
  
type s
val empty : s
val singleton : pt -> s
val append : s -> pt -> s
val complete : s -> t
