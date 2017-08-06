(** Range on a sequence, where the sequence is represented by an identifier.

    This datatype can be used to represent genomic regions with a
    triplet (chromosome, start, end) as can be found in say, BED
    files. The {! Seq_range.Make} functor offers some genericity for the choice
    of the type representing a sequence identifier, while the module
    offers the most frequent case when the identifier is a simple
    string.

    Most operations on {! Seq_range.t} are simple extensions
    of operations on {! Range.t}.
*)

open Core_kernel

module type Identifier = sig
  include Comparable
  include Sexpable with type t := t
  val to_string : t -> string
end

module Make(S : Identifier) : sig
  type t = S.t * Range.t
  [@@deriving compare, sexp]

  val make : S.t -> int -> int -> t Or_error.t
  val seq : t -> S.t

  val size : t -> int

  val to_string : t -> string
  (** String representation of a sequence range, as <seq>:<start>-<end> *)
end

include module type of Make(String)

val of_string : string -> t Or_error.t
(** Parses a string representation of a sequence range, as
    <seq>:<start>-<end> *)
