(** Internal "standard" library. *)

(**
   See
   {{:https://bitbucket.org/yminsky/ocaml-core/src/8808e3a2571f/base/core/lib/std.ml}std.ml}.
*)
include module type of Core.Std

(** {{:http://erratique.ch/software/xmlm}Xmlm 1.0.2} (to be deleted). *)
module Xmlm: module type of Biocaml_internal_xmlm

(** A more core-styled version of
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Stream.html}the stdlib's [Stream] module}. *)
module Stream: sig
  include module type of Stream with type 'a t = 'a Stream.t
  val next: 'a t -> 'a option
  val next_exn: 'a t -> 'a
  val lines_of_chars : char t -> string t
  val keep_whilei : (int -> 'a -> bool) -> 'a t -> 'a t
  val keep_while : ('a -> bool) -> 'a t -> 'a t
  val truncate : int -> 'a t -> 'a t
  val skip_whilei : (int -> 'a -> bool) -> 'a t -> unit
  val skip_while : ('a -> bool) -> 'a t -> unit
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val to_list : 'a t -> 'a list
  val map : ('a option -> 'b) -> 'a Stream.t -> 'b t
  val is_empty : 'a t -> bool
  val lines_of_channel : in_channel -> string Stream.t
end
  
