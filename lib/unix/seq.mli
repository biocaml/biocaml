(** Nucleic acid sequences. A nucleic acid code is any of A, C, G, T,
    U, R, Y, K, M, S, W, B, D, H, V, N, or X. See IUB/IUPAC standards for
    further information. Gaps are not supported. Internal representation
    uses uppercase, but constructors are case-insensitive. By convention
    the first nucleic acid in a sequence is numbered 1. *)
open Core_kernel

type t
    (** Type of a sequence *)

exception Bad of string
  (** Raised to indicate ill-formed sequence. *)

val of_string : string -> t
  (** Make sequence from string. Raise [Bad] if unsuccessful. *)

val of_buffer : Buffer.t -> t
  (** Make sequence from buffer. Raise [Bad] if unsuccessful. *)

val nth : t -> int -> char
(** [nth t i] returns the [i]th nucleic acid in sequence [t]. Raise
    [Failure] if [i] is out of range. *)

val length : t -> int
(** Length of sequence. *)

val to_string : t -> string
(** Return string representation of sequence. Answer can be
    successfully converted back using [of_string]. *)

val is_nucleic_acid : char -> bool
(** True if given character represents one of the allowed nucleic acid
    codes, case-insensitive. *)

val slice : int -> int -> t -> t
(** [slice first last t] returns the sub-sequence, or slice, of [t]
    starting from index [first] to [last] (inclusive). Raise [Failure] if
    indices out of range or if [first] > [last]. *)

val fold_left : ('a -> char -> 'a) -> 'a -> t -> 'a
(** see String.fold_left. *)

val fold_lefti : ('a -> int -> char -> 'a) -> 'a -> t -> 'a
(** String.fold_lefti *)

(** {6 Unsafe Sequences} *)

val of_buffer_unsafe : Buffer.t -> t
val of_string_unsafe : string -> t
(** Make a sequence from given buffer or string. Every character in
    input should satisfy {!is_nucleic_acid}, else returned sequence is
    ill-formed and behavior of operations on it are undefined. *)
