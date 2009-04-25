(** Operations on FASTA files. The format for a FASTA file is shown below, but see NCBI for futher details. It consists of a set of sequence headers each followed by a sequence. See module {!Seq} for details on the format of sequences. There is no standard format for the header. Often it is the name of the sequence, but many fasta files include additional content in the header. This module treats the entire string after the initial > as the header.

    {v
    >header1
    ACGT...
    
    >header2
    TGAC...
    
    ...
    v}
*)

type header = string

type t
    (** Type of a FASTA file *)

exception Bad of string

val of_file : string -> t
  (** Parse given file. Raise [Bad] if there are any parse errors. *)

val fold : (header -> Seq.t -> 'b -> 'b) -> t -> 'b -> 'b
  (** [fold f t init] folds over the sequences in [t]. *)
  
val iter : (header -> Seq.t -> unit) -> t -> unit
  
val headers : t -> header list
  (** Return all headers. *)
  
val get_seq : t -> header -> Seq.t
  (** [get_seq t x] returns the sequence with header [x] in [t]. Raise [Failure] if no such sequence. *)
