open Core
open Async

type how = Monad_sequence.how

module Deferred = Deferred

let return = return
let (>>=) = (>>=)
let (>>|) = (>>|)
let (>>=?) = (>>=?)
let (>>|?) = (>>|?)
let fail = raise
let raise = `Use_fail_instead
let try_with f = try_with f

module In_thread = struct
  include In_thread
  let run f = run f
end


module Pipe = struct
  include Pipe

  let read r = read r

  let junk r =
    read r >>= function
    | `Ok _ | `Eof -> Deferred.unit

  (* Author: Stephen Weeks. See here:
     https://groups.google.com/forum/#!topic/ocaml-core/6vskwLlFnS0 *)
  let rec peek_deferred r =
    match peek r with
    | Some x -> return (`Ok x)
    | None ->
      values_available r
      >>= function
      | `Eof -> return `Eof
      | `Ok -> peek_deferred r

  let map = map
  let fold r ~init ~f = fold r ~init ~f
  let iter r ~f = iter r ~f
end

module Reader = struct
  include Reader
  let open_file ?buf_len file = open_file ?buf_len file
  let with_file ?buf_len file ~f = with_file ?buf_len file ~f
end

module Writer = struct
  include Writer
  let with_file ?perm ?append file ~f = with_file ?perm ?append file ~f
  let write t x = write t x; Deferred.unit
  let write_char t x = write_char t x; Deferred.unit
  let write_line t x = write_line t x; Deferred.unit
end

module Sys = Sys

module Unix = struct
  type file_perm = Unix.file_perm
  let getcwd = Unix.getcwd
  (* let rename = Unix.rename *)
  (* let getpid = Unix.getpid *)

  module Stats = struct
    type t = Core.Unix.stats = {
      st_dev   : int;
      st_ino   : int;
      st_kind  : Core.Unix.file_kind;
      st_perm  : file_perm;
      st_nlink : int;
      st_uid   : int;
      st_gid   : int;
      st_rdev  : int;
      st_size  : int64;
      st_atime : float;
      st_mtime : float;
      st_ctime : float;
    }
  end

  (* let stat x = In_thread.run (fun () -> Core.Unix.stat x) *)
  (* let lstat x = In_thread.run (fun () -> Core.Unix.lstat x) *)

end
