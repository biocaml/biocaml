(** GFF files. Parser silently skips comment lines, blank lines, and pragmas.

    Versions 2 and 3 are supported. The only difference is the delimiter used for tag-value pairs in the attribute list: [3] uses an equal sign, and [2] uses a space. Version [3] also has additional requirements, e.g. the [feature] must be a sequence ontology term, but these are not checked. *)

open TylesBase

exception Bad of string
  (** Raised if there are any parse errrors. *)

type strand =
    | Sense
    | Antisense
    | Unknown (** strand is relevant but unknown *)
    | Unstranded (** strand is irrelevant *)
        
type attribute =
    | TagValue of string * string (** name and value of an attribute *)
    | Something of string  (** attributes not in tag-value format *)
        
(** Type of information on a row. *)
type row = {
  chr : string;    (** chromosome name *)
  source : string; (** where annotation came from, e.g. a database or algorithm *)
  feature : string; (** the feature type, e.g. exon or gene *)
  pos : (int * int); (** position *)
  score : float option; (** possible score, semantics not defined *)
  strand : strand; (** strand *)
  phase: int option ; (** phase, should be 0,1, or 2 if feature = "CDS" or None otherwise but this is not checked *)
  attributes : attribute list; (** the attributes *)
}
    
type t
    (** The type representing a GFF file. *)
    
val of_file : ?version:int -> ?strict:bool -> string -> t
  (** [of_file file] parses [file]. If [strict=true], the default, [Bad] is raise on any errors. If [strict=false], errors are silently skipped. Default [version] is 3, but you can also specify 2. *)
  
val fold : ('a -> row -> 'a) -> 'a -> t -> 'a
val iter : (row -> unit) -> t -> unit

val fold_file : ?version:int -> ?strict:bool -> ('a -> row -> 'a) -> 'a -> string -> 'a
  (** [fold_file f init file] accumulates the result of applying [f] to each row of [file]. Optional arguments [version] and [strict] are as in [of_file]. *)
  
val iter_file : ?version:int -> ?strict:bool -> (row -> unit) -> string -> unit

val to_list : t -> row list

val to_map : t -> row list StringMap.t
  (** Partitions annotations by chromosome. *)

val map_of_file : ?version:int -> ?strict:bool -> string -> row list StringMap.t

val get_attribute : row -> string -> string
  (** [get_attribute r x] returns the value of attribute [x]. Enclosing quotes if any are stripped off. Raise [Failure] if [x] is not defined exactly once. *)

val get_attributel : row -> string -> string list
  (** [get_attributel r x] returns the values of the attribute named [x] in row [r]. A list is returned in case the same attribute is multiply defined. An empty list indicates that the requested attribute is undefined. See also [get_attribute]. *)
  
val has_attribute : row -> string -> bool
  (** [has_attribute r x] returns true if attribute [x] is defined in [r]. *)

val add_attribute : string -> string -> row -> row
  (** [add_attribute x y r] adds attribute [x] with value [y] in [r]. Any previous value of [x] is left unaltered. Use [delete_attribute] first if desired. See also [set_attribute]. *)

val set_attribute : string -> string -> row -> row
  (** [set_attribute x y r] sets attribute [x] to [y] in [r], deleting any previous values. *)
  
val delete_attribute : string -> row -> row
  (** [delete_attribute x r] deletes all occurrences of attribute [x] in [r]. *)
  
val row_to_string : ?version:int -> row -> string
val to_channel : ?version:int -> t -> out_channel -> unit
val to_file : ?version:int -> t -> string -> unit
