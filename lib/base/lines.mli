(** Primitives for line-oriented file formats *)

(** A parser for lines

    This module provides building blocks for a line parser, especially
    a {!step} function, that given the state of the parser and a chunk
    of bytes returns a list of lines and a new state. This API is
    meant to be usable from various contexts, including async or lwt.
*)
module Parser : sig

  (** Parser state *)
  type state

  (** The initial state to be fed to the {!step} function. *)
  val initial_state : state

  (** Number of lines seen so far. We have [line_number initial_state
      = 0], and [line_number (fst (step _ (Some _))) > 0] *)
  val line_number : state -> int

  (** [step st i] parses an input. If [i = None], the caller indicates
      that there is no more input; in that case the returned state is
      terminal: all inputs read from this state will be ignored. *)
  val step : state -> string option -> state * Line.t list
end
