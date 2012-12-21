(** Internal "standard" library. *)

(**
   See
   {{:https://bitbucket.org/yminsky/ocaml-core/src/8808e3a2571f/base/core/lib/std.ml}std.ml}.

   Semantics: (1) functions that return a stream return a "fresh"
   stream, meaning that their count is set to 0. (2) indexed variants
   of HOF use the internal count of the stream
*)

module Stream : module type of Biocaml_stream

include module type of Core.Common
module List : sig
  include module type of Core.Std.List
  include Stream.Streamable
end
module Arg : module type of Core.Std.Arg
module Array : sig
  include module type of Core.Std.Array
  include Stream.Streamable
end
include module type of Array.Infix
module Backtrace : module type of Core.Std.Backtrace
module Bag : module type of Core.Std.Bag
module Big_int : module type of Core.Std.Big_int
module Bigbuffer : module type of Core.Std.Bigbuffer
module Bigstring : module type of Core.Std.Bigstring
module Bigsubstring : module type of Core.Std.Bigsubstring
module Bin_prot : module type of Core.Std.Bin_prot
module Binary_packing : module type of Core.Std.Binary_packing
module Bool : module type of Core.Std.Bool
module Caml : module type of Core.Std.Caml
module Char : module type of Core.Std.Char
module Command : module type of Core.Std.Command
module Dequeue : module type of Core.Std.Dequeue
module Exn : module type of Core.Std.Exn
module Filename : module type of Core.Std.Filename
module Float : module type of Core.Std.Float
module Fn : module type of Core.Std.Fn
module Hashtbl : module type of Core.Std.Hashtbl
module Int : module type of Core.Std.Int
include module type of Int.Infix
module In_channel : module type of Core.Std.In_channel
module Int32 : module type of Core.Std.Int32
module Int63 : module type of Core.Std.Int63
module Int64 : module type of Core.Std.Int64
module Interfaces : module type of Core.Std.Interfaces
include module type of Interfaces
module Interval : module type of Core.Std.Interval
module Lazy : module type of Core.Std.Lazy
include module type of List.Infix
module Map : module type of Core.Std.Map
module Monad : module type of Core.Std.Monad
module Nat : module type of Core.Std.Nat
module Nativeint : module type of Core.Std.Nativeint
module Num : module type of Core.Std.Num
module Option : module type of Core.Std.Option
module Out_channel : module type of Core.Std.Out_channel
module Printexc : module type of Core.Std.Printexc
module Printf : module type of Core.Std.Printf
include module type of Printf
module Queue : module type of Core.Std.Queue
module Random : module type of Core.Std.Random
module Ratio : module type of Core.Std.Ratio
module Result : module type of Core.Std.Result
include module type of Result.Export
module Set : module type of Core.Std.Set
include module type of Sexplib.Conv
module Stack : module type of Core.Std.Stack
module String : module type of Core.Std.String
include module type of String.Infix
module Sys : module type of Core.Std.Sys
module Time : module type of Core.Std.Time

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
