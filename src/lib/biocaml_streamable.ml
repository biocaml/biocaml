(** A signature for data structures that may be converted to and from
    a [Stream.t].

    If you create a new data structure, you should make it compatible
    with [Streamable] if possible.
*)
module type S = sig

  (** Type of the datastructure. *)
  type 'a t

  (** Return a stream containing all elements of given data
      structure. Exact semantics depend on implementation. For
      example, elements in stream may or may not be ordered. *)
  val to_stream : 'a t -> 'a Stream.t

  (** Return a data structure containing all elements in given stream,
      fully consuming the stream. Exact semantics depend on
      implementation. For example, duplicate elements in input may be
      ignored if the data structure is a set. *)
  val of_stream : 'a Stream.t -> 'a t

end

module type S2 = sig

  type ('a,'b) t
  val to_stream : ('a,'b) t -> ('a * 'b) Stream.t
  val of_stream : ('a * 'b) Stream.t -> ('a,'b) t

end
