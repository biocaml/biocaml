(** Transcripts are integer intervals containing a list of
    exons. Exons are themselves defined as a list of integer intervals. *)
open Core_kernel

(*
type 'a transcript = {
  exons : (int * int) list;
  lo : int;
  hi : int;
  chr : string;
  info : 'a
}

type 'a t = 'a transcript list

val of_composite_file :
  ?chr_map:(string -> string) -> ?increment_lo_hi:(int * int) ->
  string -> (string * int) t
(** Parse given composite file. Exons belonging to same transcript, as
    defined by transcript name, are merged into one transcript
    object. Info on return type is a string * int, the name of the
    transcript and the total length of exons within that transcript. The
    strand is ignored. *)

val of_bed_file :
  ?chr_map:(string -> string) -> ?increment_lo_hi:(int * int) ->
  string -> (string * int) t
(** Parse given BED file. Each line is treated as a unique transcript
    with just a single exon in it, which are the coordinates of the
    transcript itself. Info of type string * int in answer are as in
    [of_composite_file] but the name is always the empty string. *)

val of_gff : (Gff.row -> string option) -> Gff.t -> string t
(** [of_gff f gff] converts [gff] to a list of [transcript]s. Function
    [f] will be applied to each row in [gff]. If the row is an exon it
    should return the name of the transcript to which that exon belongs,
    which will be used to group exons. It should return None if the row is
    not an exon or if you want to skip that row for any other reason. The
    [info] in the answer is the name of the transcript. Raise [Failure] if
    [f] does anything erroneous such as map exons on different chromosomes
    to the same transcript. *)

val all_probes_in : 'a t -> (string * int * int * 'b) list -> ('a * 'b array) t
(** [all_probes_in transcripts probes] bins [probes] into
    [transcripts]. Each probe in [probes] has a genomic location and value
    associated with that location. Each transcript in the returned answer
    contains an array of these probe values, for those probes that fall
    within that transcript. *)

val all_points_in : 'a t -> (string * int * 'b) list -> ('a * 'b array) t
(** Like [all_probes_in] but the given "probes" are defined on single
    base pairs. *)
*)
