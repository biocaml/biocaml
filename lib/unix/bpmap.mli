(** Affymetrix's BPMAP files. Only text format supported. Binary BPMAP files must first be converted to text using Affymetrix's probe exporter tool. *)
open Core_kernel

type probe = {
  org_name: string;  (** name of organism on which probe is based *)
  version : string;  (** genome build version on which probe is based *) 
  chr_name : string; (** name of chromosome on which probe is based *)
  start_pos : int;   (** start position of probe on given chromosome *)
  sequence : Seq.t;  (** sequence of the perfect match probe *)
}

type row = {
  pmcoord : int * int; (** x,y-coordinates of perfect match probe. *)
  mmcoord : int * int; (** x,y-coordinates of mis-match probe *)
  probe : probe
}
    (** Type of information on one data row. *)
    
type t
    (** Type of a BPMAP file. *)

exception Bad of string
  (** Raised when encountering ill-formed BPMAP. *)
  
val num_probes : t -> int
  (** Number of PM/MM probe pairs in given BPMAP. The number of total probes is twice this value. *)
  
val col_names : string list
  (** Names of columns in BPMAP file, in the order required by specification. *)
  
val iter : (row -> unit) -> t -> unit
val fold : ('a -> row -> 'a) -> 'a -> t -> 'a
val to_list : t -> row list
  

(** {6 I/O} *)

val of_file : ?chr_map:(string -> string) -> string -> t
  (** [of_file file] parses [file]. If given, [chr_map] is applied to every chromosome name. Raise [Bad] if there is a parse error. *)
  
val row_to_string : row -> string
  (** String representation of row in same format as required by specification. *)
  
val to_file : string -> t -> unit
  (** [to_file file t] prints [t] to [file] in format required by specification. *)
