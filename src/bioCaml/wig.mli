(** WIG data.
    
    Internal representation of coordinates always assumes the first position on a chromosome is numbered 0. Also, integer ranges are always closed; the range [\[0, 10\]] is the set of integers from 0 to 10 inclusive of 0 and 10. WIG data can be in three formats, bed, variable step, or fixed step, and unfortunately each has different conventions as follows:
    - Bed format requires half-open intervals [\[low, high)]. Thus 1 is subtracted from the high value when parsing. The line ["chrI 0 10 3.14"] is parsed to [("chrI", 0, 9, 3.14)].
    - Variable step format numbers the first position 1. Thus 1 is subtracted from the low value when parsing. The line ["1 3.14"] is parsed to [(0, 3.14)].
    - Fixed step format numbers the first position 1. Thus 1 is subtracted from the start coordinate given in header lines. The header line ["fixedStep chrom=chrI start=1 step=100 span=30"] is parsed to [("chrI", 0, 100, 30)].
    
    The inverse is done for printing routines. You are freed from these details if you always use this module to parse and print.
    
    All parsers allow columns (fields) on a line to be separated by any combination of space, tab, or carriage return characters. Printers always separate columns with a single tab. Tag-value pairs must be in the form "tag=value" with no space around the '='.
*)

type pt = string * int * int * float
    (** A data point is a 4-tuple [(chr,lo,hi,x)], where [x] is the value assigned to interval [\[lo, hi\]], inclusive of end-points, on chromosome [chr]. *)
    
type t
    (** Type of WIG data. Can be thought of as a collection of [pt]'s. Coordinates of data points are not allowed to overlap, for each chromosome. *)
    
exception Bad of string
  
val of_bed_list : pt list -> t
  (** Construct WIG data from given [pt]'s. Raise [Bad] if any errors. *)
  
val to_bed_list : t -> pt list
  (** Extract data as a flat list. Guaranteed to be in order by chromosome name, and then coordinate. *)
  
val iter : (pt -> unit) -> t -> unit
val fold : ('a -> pt -> 'a) -> 'a -> t -> 'a
  (** [iter] and [fold] will be more efficient than converting to list and using list's iter or fold when internal representation is fixed or variable step. *)

  
module B : sig
  type datum = string * int * int * float
      (** chromosome name, low and high values of coordinate range, data value *)
      
  type s
      (** Representation of bed formatted WIG data that supports efficient appending. *)
      
  val datum_of_string : string -> datum
    (** Raise [Bad] if any errors. E.g. ["chrI 0 10 3.14"] is parsed to [("chrI", 0, 9, 3.14)]. *)
    
  val empty : s
    (** The empty data set. *)
    
  val singleton : datum -> s
    (** Return the data set with the single given data point. *)
    
  val append_datum : s -> datum -> s
    (** [append_datum s l] appends [l] to the end of [s]. Raise [Bad] if [l] cannot be added. *)

  val complete : s -> t
    
  val datum_to_string : datum -> string
    (** E.g. [("chrI", 0, 9, 3.14)] is converted to ["chrI\t0\t10\t3.14"]. *)
end
  
module V : sig
  type header = string * int
      (** Header line provides a chromosome name and span. *)

  type datum = int * float
      (** Each data line provides a low coordinate and data value. *)

  type s
      (** Type of WIG data in variable step format. *)

  val header_of_string : string -> header
    (** Raise [Bad] if any errors. *)
    
  val datum_of_string : string -> datum
    (** Raise [Bad] if any errors. E.g. ["1 3.14"] is parsed to [(0, 3.14)]. *)
    
  val empty : header -> s
    (** Return empty data set, ready to have data added under given header. *)
    
  val set_header : s -> header -> s
    (** Make data set ready for adding data under given header. Raise [Bad] if this cannot be done. *)
    
  val append_datum : s -> datum -> s
    (** Append given data point. Raise [Bad] if this cannot be done. *)

  val complete : s -> t
    (** Raise [Bad] if it does not make sense to be done adding data. This is possible for example if a new header was just set but no datum were appened. *)
    
  val header_to_string : header -> string

  val datum_to_string : datum -> string
    (** E.g. [(0, 3.14)] is converted to ["1\t3.14"]. *)

end

module F : sig
  type header = string * int * int * int
      (** Header line provides a chromosome name, start coordinate, step size, and span. *)
      
  type datum = float
      (** Each data line provides just a float data value. *)
      
  type s
      (** Type of WIG data in fixed step format. *)

  val header_of_string : string -> header
    (** Raise [Bad] if any errors. E.g. ["fixedStep chrom=chrI start=1 step=100 span=30"] is parsed to [("chrI", 0, 100, 30)]. *)
    
  val datum_of_string : string -> datum
    (** Raise [Bad] if any errors. *)
    
  val empty : header -> s
    (** Return empty data set, ready to have data added under given header. *)
    
  val set_header : s -> header -> s
    (** Make data set ready for adding data under given header. Raise [Bad] if this cannot be done. *)
    
  val append_datum : s -> datum -> s
    (** Append given data point. Raise [Bad] if this cannot be done. *)
    
  val complete : s -> t
    (** Raise [Bad] if it does not make sense to be done adding data. This is possible for example if a new header was just set but no datum were appened. *)
    
  val header_to_string : header -> string
    (** E.g. [("chrI", 0, 100, 30)] is converted to ["fixedStep\tchrom=chrI\tstart=1\tstep=100\tspan=30"]. *)
    
  val datum_to_string : datum -> string
end
