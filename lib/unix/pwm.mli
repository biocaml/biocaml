(** Position-weight matrix

    This module can be used to create position-weight matrices (PWM)
    to describe a DNA motif. Such matrices can then be searched on 
    a DNA sequence, given a threshold for alignment score.
 *)
open Core_kernel

type count_matrix = int array array
  (** Type to represent gap-free alignments. First dimension is 
      the sequence position, second dimension is for the alphabet.
      Only DNA alphabet (A, C, G, T) is supported to rows should be
      of length exactly four. *)

type background = private float array
  (** Probability distribution over an alphabet *)

val flat_background : unit -> background
  (** Uniform distribution over A, C, G, T *)

val background_of_sequence : string -> float -> background
  (** [background_of_sequence seq pc] estimates the base
      frequency in [seq] using [pc] as pseudo-counts. Typical
      value for [pc] is [0.1]. *)

type t = private float array array
  (** Representation of a PWM *)

val make : count_matrix -> background -> t
  (** Builds a PWM from a count_matrix and a background *)

val tandem : 
  ?orientation:[`direct | `inverted | `everted] ->
  spacer:int ->
  count_matrix -> count_matrix -> background -> t
  (** [tandem orientation spacer cm1 cm2 bg] builds a PWM by constructing 
      a composite motif: it builds [mat1] the PWM from [cm1] under background [bg]
      (resp. [mat2] from [cm2] under [bg]), then concatenates [mat1] and [mat2] with
      [spacer] non scoring columns in between *)

val reverse_complement : t -> t
  (** Reverse complement of a PWM *)

val scan : t -> string -> float -> (int * float) list
  (** [scan mat seq tol] returns the list of positions (with
      corresponding scores) such that the alignment score
      of [mat] is superior to [tol] *)

val fast_scan : t -> string -> float -> (int * float) list
  (** Identical to [scan] but directly implemented in C *)

val best_hit : t -> string -> int * float
(** [best_hit mat seq] returns the position and score of the best
    alignment found in [seq] for the motif [mat]. Raise [Invalid_arg]
    if [seq] is shorter than [mat] *)










