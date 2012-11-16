(** Internal "standard" library. *)

(**
   See
   {{:https://bitbucket.org/yminsky/ocaml-core/src/8808e3a2571f/base/core/lib/std.ml}std.ml}.
*)
include module type of Core.Std

val try_finally_exn : fend:('a -> unit) -> ('a -> 'b) -> 'a -> 'b
  (** [try_finally_exn fend f a] will run [x = f a], then run [fend
      a], and finally return [x]. If [f a] raised an exception that
      exception will be returned even if [f x] raises an exception too. If
      [f a] successfully produces [x], then it is possible to get instead
      an exception raised by [fend a]. *)

val open_out_safe : string -> out_channel
  (** Like [open_out] but will not overwrite existing file. *)

val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)
  (** [flip f] returns a function [g] that takes its arguments in the
      opposite order of [f], i.e. [f x y = g y x]. *)

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

  val iter2_exn : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
    (** [iter2_exn a b ~f] calls in turn [f a1 b1; ...; f an
        bn]. @raise Invalid_argument if the two streams have different
        lengths, and no guarantee about which elements were
        consumed. *)

  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val to_list : 'a t -> 'a list
  val map : ('a -> 'b) -> 'a Stream.t -> 'b t
  val filter : 'a t -> f:('a -> bool) -> 'a t
  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
  val is_empty : 'a t -> bool
  val lines_of_channel : in_channel -> string Stream.t

  val result_to_exn :
    ('output, 'error) Result.t t ->
    error_to_exn:('error -> exn) ->
    'output t
      (** Convert exception-less stream to exception-ful
          stream. Resulting stream raises exception at first error
          seen. *)

  module Infix : sig
    val ( /@ ) : 'a t -> ('a -> 'b) -> 'b t
      (** [s /@ f] is equivalent to [map f s] *)

    val ( // ) : 'a t -> ('a -> bool) -> 'a t
      (** [s // f] is equivalent to [filter f s] *)

    val ( //@ ) : 'a t -> ('a -> 'b option) -> 'b t
      (** [s //@ f] is equivalent to [filter_map f s] *)
  end

end

module Lines : sig
  exception Error of (Biocaml_pos.t * string)
  val fold_stream' : ?file:string -> ?strict:bool -> ('a -> string -> 'a) -> 'a -> char Stream.t -> 'a
  val fold_stream : ?strict:bool -> ('a -> string -> 'a) -> 'a -> char Stream.t -> 'a
  val fold_channel' : ?file:string -> ?strict:bool -> ('a -> string -> 'a) -> 'a -> in_channel -> 'a
  val fold_channel : ?strict:bool -> ('a -> string -> 'a) -> 'a -> in_channel -> 'a
  val fold_file : ?strict:bool -> ('a -> string -> 'a) -> 'a -> string -> 'a
  val iter_file : ?strict:bool -> (string -> unit) -> string -> unit
  val of_stream : ?strict:bool -> (string -> 'a) -> char Stream.t -> 'a List.t
  val of_channel : ?strict:bool -> (string -> 'a) -> in_channel -> 'a List.t
end

(** Operations on URL-style encodings. *)
module Url : sig

  val escape: string -> string
  (** Convert non-alphanumeric characters to their ["%HX"]
      URL-escaping format. *)
    
  val unescape: string -> error:(string -> 'error) -> (string, 'error) Result.t
  (** Convert a string containing ["%HX"] escaped characters to a normal
      string. In case of error, the string is passed to the [~error] parameter
      and the function returns its result. *)

end

(** Utility functions to write manual parsers. *)
module Parse : sig

  val escapable_string: string -> stop_before:char list ->
    (string * char option * string)
  (** Parse a string potentially escaped with OCaml string
      conventions, or stop at [stop_before] character if it is not
      escaped.  Examples: {[
      (* Does not stop: *)
      escapable_string ~stop_before:\['='; '@'\]  "sdf\tsd\000 sdf fdsaf";;
      = ("sdf\tsd\000 sdf fdsaf", None, "")
      (* Reads an escaped string; *)
      escapable_string ~stop_before:\['='; '@'\]  "\"sdf\\tsd\\000\" sdf fdsaf";;
      = ("sdf\tsd\000", None, " sdf fdsa")
      escapable_string ~stop_before:\['='; '@'\]  "\"sdf\\tsd\\000\" s=df \@fdsaf";;
      = ("sdf\tsd\000", None, " s=df \@fdsa")
      escapable_string ~stop_before:\['='; '@'\]  "\"sdf\\tsd\\000\"\@ s=df \@fdsaf";;
      = ("sdf\tsd\000", Some '\@', " s=df \@fdsa")
      (* Stops at '=' or '\@' *)
      escapable_string ~stop_before:\['='; '@'\]  "sdf\tsd\000 s=df \@fdsaf";;
      = ("sdf\tsd\000 s", Some '=', "df \@fdsa")
      escapable_string ~stop_before:\['='; '@'\]  "sdf\tsd\000 sdf \@fdsaf";;
      = ("sdf\tsd\000 sdf ", Some '\@', "fdsa")
      ]} *)

end

(** More operations for the [Result.t] monad. *)
module With_result: sig

  include module type of Result

  val while_ok: 'a list -> f:(int -> 'a -> ('b, 'e) Result.t) ->
    ('b list, 'e) Result.t
  (** Map the function [f] on the list until the first error is met. *)

  val output_result : 'a -> [> `output of 'a ]
  val output_ok : 'a -> [> `output of ('a, 'b) t ]
  val output_error : 'a -> [> `output of ('b, 'a) t ]

end

module Order : sig

  val compose :
    ('a -> 'b -> int option) ->
    ('a -> 'b -> int option) -> 'a -> 'b -> int
  val reverse : ('a -> 'b -> int) -> 'a -> 'b -> int
  val reversep :
    ('a -> 'b -> int Option.t) -> 'a -> 'b -> int Option.t
  val totalify : ('a -> 'b -> 'c option) -> 'a -> 'b -> 'c

end

module Debug: sig

  val enable: string -> unit
  val disable: string -> unit

  val make : string -> ('a, unit, string, unit) format4 -> 'a

end
