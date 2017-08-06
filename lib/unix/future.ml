open Core_kernel

module type S = sig

  type how = [ `Parallel | `Sequential | `Max_concurrent_jobs of int ]
  (** [`Max_concurrent_jobs] supported only for Async
  implementation. The Lwt implementation treats this the same as
  [`Parallel]. Blocking implementation treats all as [`Sequential]. *)

  module Deferred : sig
    include Monad.S

    val unit : unit t

    module Result : Monad.S2
      with type ('a, 'b) t = ('a, 'b) Result.t t

    module List : sig
      val fold : 'a list -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t
      val iter : ?how:how -> 'a list -> f:('a -> unit t) -> unit t
      val map : ?how:how -> 'a list -> f:('a -> 'b t) -> 'b list t
      val filter : ?how:how -> 'a list -> f:('a -> bool t) -> 'a list t
    end

    module Or_error : sig
      module List : sig

        val map
          :  ?how:how
          -> 'a list
          -> f:('a -> 'b Or_error.t t)
          -> 'b list Or_error.t t

        val iter
          :  ?how:how
          -> 'a list
          -> f:('a -> unit Or_error.t t)
          -> unit Or_error.t t

      end
    end

  end

  val return : 'a -> 'a Deferred.t
  val (>>=) : 'a Deferred.t -> ('a -> 'b Deferred.t) -> 'b Deferred.t
  val (>>|) : 'a Deferred.t -> ('a -> 'b) -> 'b Deferred.t

  val (>>=?) :
    ('a, 'b) Deferred.Result.t ->
    ('a -> ('c, 'b) Deferred.Result.t) ->
    ('c, 'b) Deferred.Result.t

  val (>>|?) :
    ('a, 'b) Deferred.Result.t ->
    ('a -> 'c) ->
    ('c, 'b) Deferred.Result.t

  (** Difference from Async: Use [fail] instead of [raise]. *)
  val fail : exn -> 'a Deferred.t
  val raise : [> `Use_fail_instead ]

  (** Async supports several extra parameters to this function, which
      we do not currently support. *)
  val try_with : (unit -> 'a Deferred.t) -> ('a, exn) Result.t Deferred.t


  module In_thread : sig
    val run : (unit -> 'a) -> 'a Deferred.t
  end

  module Pipe : sig
    module Reader : sig
      type 'a t
    end

    val read : 'a Reader.t -> [ `Eof | `Ok of 'a ] Deferred.t

    (** Discard one item from the pipe. Do nothing if pipe is already
        fully consumed. Difference from Async: This function is not
        defined. *)
    val junk : 'a Reader.t -> unit Deferred.t

    (** Like [read] but doesn't consume the item. Difference from
        Async: This function is not defined. We don't call this
        function [peek] because that is already another function in
        Async, which has different semantics. *)
    val peek_deferred : 'a Reader.t -> [ `Eof | `Ok of 'a ] Deferred.t

    val map : 'a Reader.t -> f:('a -> 'b) -> 'b Reader.t

    val fold :
      'a Reader.t ->
      init:'accum ->
      f:('accum -> 'a -> 'accum Deferred.t) ->
      'accum Deferred.t

    val iter :
      'a Reader.t ->
      f:('a -> unit Deferred.t) ->
      unit Deferred.t

  end

  module Reader : sig
    module Read_result : sig
      type 'a t = [ `Eof | `Ok of 'a ]
    end

    type t

    (** Difference from Async: implementations should try to use
        [buf_len] but are not required to. *)
    val open_file : ?buf_len:int -> string -> t Deferred.t

    val close : t -> unit Deferred.t

    (** Difference from Async: implementations should try to use
        [buf_len] but are not required to. *)
    val with_file :
      ?buf_len:int ->
      string ->
      f:(t -> 'a Deferred.t) ->
      'a Deferred.t

    val read_line : t -> string Read_result.t Deferred.t
    val read_all : t -> (t -> 'a Read_result.t Deferred.t) -> 'a Pipe.Reader.t
    val lines : t -> string Pipe.Reader.t
    val contents : t -> string Deferred.t
    val file_contents : string -> string Deferred.t
    val file_lines : string -> string list Deferred.t
  end

  module Writer : sig
    type t

    val with_file
      : ?perm:int
      -> ?append:bool
      -> string
      -> f:(t -> 'a Deferred.t)
      -> 'a Deferred.t

    (** Following functions returned a Deferred.t, while in Async they
        return unit. *)
    val write : t -> string -> unit Deferred.t
    val write_char : t -> char -> unit Deferred.t
    val write_line : t -> string -> unit Deferred.t

  end

  module Unix : sig
    type file_perm = Unix.file_perm
    val getcwd : unit -> string Deferred.t
    (* val rename : src:string -> dst:string -> unit Deferred.t *)
    (* val getpid : unit -> Pid.t *)
  end

end
