(** Consistent printing of errors, warnings, and bugs. An error is a user mistake that prevents continuing program execution, a warning is a milder problem that the program continues to execute through, and a bug is a mistake in the software. *)

val err : ?pos:Biocaml.Pos.t -> string -> string
val warn : ?pos:Biocaml.Pos.t -> string -> string

(** Create a string communicating an error, warning, or bug. First optional argument is position where problem occurred. Second argument is a string explaining the problem. *)
val bug : ?pos:Biocaml.Pos.t -> string -> string

val print_err : ?pos:Biocaml.Pos.t -> string -> unit
val print_warn : ?pos:Biocaml.Pos.t -> string -> unit

(** Print an error, warning, or bug. First optional argument is position where problem occurred. Second argument is a string explaining the problem. *)
val print_bug : ?pos:Biocaml.Pos.t -> string -> unit

(** String explaining OCaml's array length limitation on 32-bit machines. *)
val max_array_length_error : string

(** Message tree for more complex messages. *)
module Tree : sig
  type t =
    | T of string * t list
        (** A tree of messages. The tree [(msg,sub_msgs)] is interpreted as meaning that [sub_msgs] are the various possible explanations for [msg]. *)

  val leaf : string -> t

  (** [add_child t x] inserts [x] as the right-most child of [t]. *)
  val add_child : t -> t -> t

  val to_string : t -> string
end
