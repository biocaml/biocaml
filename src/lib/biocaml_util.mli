(** Miscellaneous functions that don't fit elsewhere. *)

open Batteries_uni

val err_enum : ?source:string -> 'a Enum.t -> 'a Enum.t
  (** Returned enum behaves identically to given one except that its
      use will raise [PosError] when accessing its element raises an
      exception [e]. First value carried by [PosError] will be
      [source] (default: ""), second will be the item number where the
      exception occurred, third argument will be -1, and fourth will
      be the exception raised. *)
