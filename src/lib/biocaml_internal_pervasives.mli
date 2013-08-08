(** Internal "standard" library. This module is not part of the
    Biocaml API and subject to change at any time. Biocaml uses Core, and
    for the most part, this module simply includes Core modules, sometimes
    with a few functions added. A few modules are completely new.
*)

module Stream : module type of CFStream_stream
module Streamable : module type of CFStream_streamable

include module type of Core.Common
val ( |? ) : 'a option -> 'a -> 'a
module List : module type of Core.Std.List
module Arg : module type of Core.Std.Arg
module Array : sig
  include module type of Core.Std.Array

  (** [range xs] is the stream of all valid indices in [xs] *)
  val range : 'a t -> int Stream.t
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
module Buffer : module type of Core.Std.Caml.Buffer
module Caml : module type of Core.Std.Caml
module Char : module type of Core.Std.Char
module Command : module type of Core.Std.Command
module Dequeue : module type of Core.Std.Dequeue
module Exn : module type of Core.Std.Exn
module Filename : sig
  include module type of Core.Std.Filename

  module Infix : sig
    (** [p1/p2] is equivalent to [concat p1 p2]. *)
    val (/) : string -> string -> string
  end
end
module Float : module type of Core.Std.Float
module Fn : module type of Core.Std.Fn
module Hashtbl : sig
  include module type of Core.Std.Hashtbl
  include Streamable.S2 with type ('a,'b) t := ('a,'b) t
end
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
module Map : sig
  include module type of Core.Std.Map
  val to_stream : ('a, 'b, 'c) t -> ('a * 'b) Stream.t
  val of_stream : ('a * 'b) Stream.t -> ('a, 'b) Poly.t
end
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
module Result : sig

  include module type of Core.Std.Result

  (** Map the function [f] on the list until the first error is
      met. *)
  val while_ok: 'a list -> f:(int -> 'a -> ('b, 'e) t) ->
    ('b list, 'e) t

  val output_result : 'a -> [> `output of 'a ]
  val output_ok : 'a -> [> `output of ('a, 'b) t ]
  val output_error : 'a -> [> `output of ('b, 'a) t ]

end
include module type of Result.Export
module Set : sig
  include module type of Core.Std.Set
  val to_stream : ('a, 'b) t -> 'a Stream.t
  val of_stream : 'a Stream.t -> 'a Poly.t
end
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
