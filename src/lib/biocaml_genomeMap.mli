(**
   Data structures to represent sets of (possibly annotated) genomic regions

   This module is useful to deal with sets of genomic regions. It
   provides set operations like union, intersection, difference or
   membership tests. Specific data types are also provided when the
   regions are annotated with some value.

   Genomic regions are represented as a pair formed by a range and an
   abstract representation of a sequence/chromosome identifiers. The
   data structures implemented here are parameterized over this
   abstract type, but the module provides an implementation for the
   most common case where chromosomes are identified with a string.

   The functor [Make] provides four datatypes, which corresponds to
   variants where:

   - the regions in the set can overlap or not

   - the regions are annotated with some values
*)

(** An abstract representation of a chromosome identifier *)
module type Chromosome = sig
  type t
  val compare : t -> t -> int
end

module Make(Chromosome : Chromosome) : sig
  type range = Biocaml_range.t
  type location = Chromosome.t * range

  (** A collection of non-overlapping regions *)
  module Selection : sig
    type t

    val inter : t -> t -> t
    val union : t -> t -> t
    val diff : t -> t -> t
    val size : t -> int

    val intersects : t -> location -> bool
    (** [intersects loc sel] returns [true] if [loc] has a non-empty
        intersection with [sel], and [false] otherwise. *)

    val overlap : t -> location -> int

    val to_stream : t -> location Stream.t

    val of_stream : location Stream.t -> t
    (** [of_stream e] computes a selection (i.e. a set of non
        overlapping locations) as the union of the locations contained
        in [e] *)

  end

  (** Partial function over the genome

      This module implements a partial function over the genome: each
      base may be associated to a value. Alternatively, this can be
      seen as a collection of non-overlapping regions each labeled
      with a value *)
  module type Signal = sig
    type 'a t

    val eval : 'a t -> default:'a -> Chromosome.t -> int -> 'a
    (** function evaluation at some point in the genome *)

    val fold : 'a t -> init:'c -> f:('c -> location -> 'b -> 'c) -> 'c
    (** folds on constant intervals of the function, in increasing order *)

    val to_stream : 'a t -> (location * 'a) Stream.t
    (** stream over all constant intervals of the function, in
        increasing order *)

    val of_stream : ('a -> 'a -> 'a) -> (location * 'a) Stream.t -> 'a t
    (** [of_stream f ls] builds a signal from a collection of
    annotated locations. [f] is used when two locations intersect, to
    compute the annotation on their intersection. *Beware*, [f]
    should be associative and commutative since when many locations
    in [ls] intersect, there is no guarantee on the order followed to
    aggregate them and their annotation. *)

  end

  (** A set of locations *)
  module LSet : sig
    type t

    val to_stream : t -> location Stream.t
    val of_stream : location Stream.t -> t


    val intersects : t -> location -> bool
    (** [intersects lset loc] returns [true] if [loc] has a non-empty
        intersection with one of the locations in [lset], and returns
        [false] otherwise *)

    val closest : t -> location -> (location * int) option
    (** [closest lset loc] returns the location in [lset] that is the
        closest to [loc], along with the actual (minimal)
        distance. Returns [None] if there is no location in [lset]
        that comes from the same chromosome than [loc]. *)

    val intersecting_elems : t -> location -> location Stream.t
    (** [intersecting_elems lset loc] returns a stream of all
        locations in [lset] that intersect [loc]. *)

  end

  (** A set of locations with an attached value on each of them *)
  module LMap : sig
    type 'a t

    val to_stream : 'a t -> (location * 'a) Stream.t
    val of_stream : (location * 'a) Stream.t -> 'a t

    val intersects : 'a t -> location -> bool
    (** [intersects lmap loc] returns [true] if [loc] has a non-empty
        intersection with one of the locations in [lmap], and returns
        [false] otherwise *)

    val closest : 'a t -> location -> (location * 'a * int) option
    (** [closest lmap loc] returns the location in [lmap] that is the
        closest to [loc], along with its annotation and the actual (minimal)
        distance. Returns [None] if there is no location in [lmap]
        that comes from the same chromosome than [loc]. *)

    val intersecting_elems : 'a t -> location -> (location * 'a) Stream.t
    (** [intersecting_elems lmap loc] returns a stream of elements
        in [lmap] whose location intersects with [loc]. *)

  end
end

