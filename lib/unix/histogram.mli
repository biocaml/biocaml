(** Histograms with polymorphic bin types.

    A histogram is a list of bins, each with a count. Bin [i] is
    defined by a lower limit [lo(i)] and an upper limit [hi(i)]. It is
    inclusive of the lower limit and exclusive of the upper limit. For
    all [i], [hi(i) = lo(i+1)]. By convention the first bin is
    numbered 0. The count of a bin is a floating point number,
    allowing fractional values if necessary.
*)
open Core_kernel

type 'a t
    (** The type of a histogram whose bin limits are of type ['a]. *)

val make : ('a -> 'a -> int) -> 'a list -> 'a t option
(** [make cmp bins] returns a new histogram from the given [bins],
    all initialized to a count of 0.0. The [bins] must be provided as a
    list of the boundaries dividing them. The list [\[v0; v1; ...; vn\]]
    of length [n+1] represents the [n] bins [\[v0, v1)], [\[v1, v2)],
    ..., [\[vn-1, vn)], where [cmp] is used as the comparison
    function. Resturns [None] if [bins] are not monotonically
    increasing, or if length of [bins] is less than 2. *)

val to_list : 'a t -> (('a * 'a) * float) list
(** Return a list of all bin/count pairs. Answer will be in
    ascending order by the bin limits. *)

val copy : 'a t -> 'a t
(** Copy histogram. *)

val bin : 'a t -> int -> ('a * 'a) option
(** [bin hist i] returns the [i]th bin of [hist]. *)

val bin_exn : 'a t -> int -> 'a * 'a
(** [bin hist i] returns the [i]th bin of [hist]. Raise
    [Invalid_argument] if an invalid bin number is requested. *)

val count : 'a t -> int -> float option
(** [count hist i] returns the count the [i]th bin. *)

val count_exn : 'a t -> int -> float
(** [count hist i] returns the count the [i]th bin. Raise
    [Invalid_argument] if an invalid bin number is requested. *)

val num_bins : 'a t -> int
  (** Number of bins. *)

val minimum : 'a t -> 'a
  (** Lower limit of the minimum bin. *)

val maximum : 'a t -> 'a
  (** Upper limit of the maximum bin. *)

val increment : ?delt:float -> 'a t -> 'a -> 'a t
(** [increment delt hist x] increments the count of the bin containing
    [x] by [delt] (default is 1.0). The histogram is unaltered if [x] not
    in any bin. This is not considered an error because it is often
    necessary to calculate a histogram for a subset of a larger data
    set. *)

val reset : 'a t -> 'a t
(** Return histogram with same bins but all counts reset to 0.0. *)

val find_bin_index : 'a t -> 'a -> int option
(** [find_bin_index hist x] returns the index of the bin in [hist]
    containing [x]. Return None if [x] is outside the histogram's
    range. *)

val in_range : 'a t -> 'a -> bool
(** [in_range hist x] is true if [x] greater than or equal to [minimum
    hist] and strictly less than [maximum hist]. *)


(** {6 Histograms With Float Bins} *)

val make_uniform : float -> float -> int -> (float t, string) Result.t
(** [make_uniform min max n] returns a histogram with [n] bins
    uniformly dividing up the range from [min] to [max]. Bins will be
    inclusive of the lower limit and exclusive of the upper limit,
    i.e. value of [min] will fall into lowest bin and value of [max] will
    fall outside the range of the histogram. Raise [Failure] if [min] not
    strictly less than [max] or if [n] not greater than 0. *)
