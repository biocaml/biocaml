(** BED data. A BED file is in the format shown below, where columns must be separted by a tab character. The order of the lines does not matter. The definition is that intervals are zero based and half-open. So by default the line "chrA   lo   hi" is parsed to the interval [\[lo + 1, hi\]] on chromosome [chrA]. Similarly, when printing, the default is to print [\[lo - 1, hi\]]. The optional argument [increment_lo_hi] allows changing this behavior for non-conformant files. In addition, the optional argument [chr_map] is a [string -> string] function that allows changing of the chromosome name to a specified format, and defaults to [identity]. 

    {v
    chrA   lo1   hi1
    chrA   lo2   hi2
     .      .     .
     .      .     .
     .      .     .
    chrB   lo1   hi1
    chrB   lo2   hi2
     .      .     .
     .      .     .
     .      .     .
    v}

    Some tools require that given intervals do not overlap within each chromosome. This is not enforced, but you can use [any_overlap] to verify this property when needed.
*)

type pt = string * int * int
    (** A data point is a triple [(chr,lo,hi)] defining an interval [\[lo, hi\]] on chromosome [chr]. All methods taking a [pt] as an input argument verify that [lo <= hi] and raise [Bad] if it is not. *)
    
type t
    (** Type of BED data. Can be thought of as a collection of [pt]'s. *)
    
exception Bad of string

val empty : t
  (** The empty data set. *)

val insert : pt -> t -> t
  (** [insert pt t] inserts [pt] into [t]. Does nothing if [pt] was already in [t]. *)

val mem : pt -> t -> bool
  (** [mem pt t] returns true if [pt] is in [t]. *)

val any_overlap : t -> bool
  (** Returns true if any intervals overlap within the same chromosome. *)

val diff : t -> t -> t
  (** Returns the difference in base pairs from one bed to another. In other words, does an RSet.diff on each chromosome. *)
  
val union : t -> t -> t
  (** Returns the union in base pairs from one bed file to another. In other words, does an RSet.union on each chromosome. *)
  

(** {6 Constructors and Extractors} *)

val of_list: pt list -> t
  (** Construct data set from given [pt]'s. If there are duplicate points, just one is inserted. *)
  
val to_list : t -> pt list
  (** Extract data as a flat list. *)
  
val get_chr : string -> t -> (int * int) list
  (** [get_chr chr t] returns list of intervals on chromosome [chr]. Returns empty list if no data for given chromosome. *)

val to_lists : t -> (string * (int * int) list) list
  (** Extract data as a list of intervals for each chromosome. *)
  

(** {6 Parsers and Printers} *)
  
val of_file : ?chr_map:(string -> string) -> ?increment_lo_hi:(int * int) -> string -> t
  (** Parse given file. Default value for [increment_lo_hi] is [(1,0)]. Raise [Bad] if any errors. *)
  
val of_channel : ?chr_map:(string -> string) -> ?increment_lo_hi:(int * int) -> in_channel -> t
  (** Like [of_file]. *)

val to_file : ?chr_map:(string -> string) -> ?increment_lo_hi:(int * int) -> t -> string -> unit
  (** [to_file t file] prints [t] to [file] in standard BED format. Default value for [increment_lo_hi] is [(-1,0)]. *)
  
val to_channel : ?chr_map:(string -> string) -> ?increment_lo_hi:(int * int) -> t -> out_channel -> unit
  (** Like [to_file]. *)
