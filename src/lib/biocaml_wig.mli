(** WIG data.
    
    Internal representation of coordinates always assumes the first
    position on a chromosome is numbered 1. Also, integer ranges are
    always closed; the range [\[1, 10\]] is the set of integers from 1
    to 10 inclusive of 1 and 10. WIG data can be in three
    formats---bed, variable-step, or fixed-step---and unfortunately
    each has different conventions as follows:
    - Bed format requires half-open intervals [\[low, high\)] and
      numbers the first base as 0. Thus 1 is added to the low value
      when parsing. The line ["chrI 0 10 3.14"] is parsed to [("chrI",
      1, 10, 3.14)].
    - Variable-step format numbers the first position 1 and uses
      closed intervals. Thus no change is required. The line ["1
      3.14"] is parsed to [(1, 3.14)].
    - Fixed-step format numbers the first position 1 and uses closed
      intervals. Thus no change is required. The header line
      ["fixedStep chrom=chrI start=1 step=100 span=30"] is parsed to
      [("chrI", 1, 100, 30)].
    
    The inverse is done for printing routines. You are freed from
    these details if you always use this module to parse and print.
    
    All parsers allow columns (fields) on a line to be separated by
    any combination of space, tab, or carriage return
    characters. Printers always separate columns with a single
    tab. Tag-value pairs must be in the form "tag=value" with no space
    around the '='.
*)

type pt = string * int * int * float
    (** A data point is a 4-tuple [(chr,lo,hi,x)], where [x] is the value assigned to interval [\[lo, hi\]], inclusive of end-points, on chromosome [chr]. *)
    
type t
    (** Type of WIG data. Can be thought of as a collection of [pt]'s. Coordinates of data points are not allowed to overlap, for each chromosome. *)
    
type format = Bed | VariableStep | FixedStep
    (** The three formats in which WIG data can be specified. *)

exception Bad of string

val of_list : pt list -> t
  (** Construct WIG data from given [pt]'s. Raise [Bad] if any [pt]'s are invalid. *)

val to_list : t -> pt list
  (** Extract data as a flat list. *)
  
val iter : (pt -> unit) -> t -> unit
val fold : ('a -> pt -> 'a) -> 'a -> t -> 'a

val to_file : ?fmt:format -> t -> string -> unit
  (** [to_file ~fmt t file] prints [t] to [file]. Printing is in most efficient format possible by default. Optional [fmt] argument forces printing in a specific format. Requesting [VariableStep] or [FixedStep] may raise [Failure] if given data cannot be represented in those formats. *)

val to_channel : ?fmt:format -> t -> out_channel -> unit
  (** Like [to_file] but print to channel. *)

val of_channel : ?fmt:format -> ?chr_map:(string -> string) -> ?header:bool -> ?increment_lo_hi:(int * int) -> in_channel -> t
val of_file : ?fmt:format -> ?chr_map:(string -> string) -> ?header:bool -> ?increment_lo_hi:(int * int) -> string -> t
