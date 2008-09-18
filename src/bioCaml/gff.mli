(** GFF files. *)

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
        
(** Type of information on a row. This particular type represents all information provided in GFF files, but note that type ['a t] allows alternative types of annotation information to be retained. *)
type row = {
  chr : string;    (** chromosome name *)
  source : string; (** where annotation came from, e.g. a database or algorithm *)
  feature : string; (** the feature type *)
  pos : Range.t; (** position *)
  score : float option; (** possible score, semantics not defined *)
  strand : strand; (** strand *)
  phase: int option ; (** phase, should be 0,1, or 2 if feature = "CDS" or None otherwise but this is not checked *)
  attributes : attribute list; (** the attributes *)
}
    
type 'a t
    (** The type representing a GFF file. Can be thought of as a collection of annotations of type ['a]. *)
    
val of_file : (row -> 'a option) -> string -> 'a t
  (** [of_file f file] parses [file] using [f] to convert each row into the desired type of information. Function [f] should return None to omit the row entirely. *)

val fold_file : ('a -> row -> 'a) -> 'a -> string -> 'a
  (** [fold_file f init file] accumulates the result of applying [f] to each row of [file]. This is useful for large files because it avoids loading the entire file first. *)
  
val to_list : 'a t -> 'a list

(*
val of_array : annt array -> t
  (** [of_array] behaves similarly to [of_file]. *)

val file_to_array : string -> annt array
  (** [file_to_array file] returns the contents of [file] as a plain array of annotations. *)


val get_attr : annt -> string -> string
  (** [get_attr annt attr] returns the value of [attr] in attribute list of [annt]. Raise [Failure] if [attr] does not occur exactly once in attribute list. *)
*)
