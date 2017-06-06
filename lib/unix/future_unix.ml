open Core_kernel
open CFStream

type how = [ `Parallel | `Sequential | `Max_concurrent_jobs of int ]

module Deferred = struct
  type 'a t = 'a

  include Monad.Make(struct
    type 'a t = 'a
    let return x = x
    let bind m ~f = f m
    let map = `Custom (fun m ~f -> f m)
  end)

  let unit = ()

  module Result = struct
    type ('a, 'b) t = ('a, 'b) Result.t

    include Monad.Make2(struct
      type ('a, 'b) t = ('a, 'b) Result.t
      let return = Result.return
      let bind = Result.bind
      let map = `Custom Result.map
    end)
  end

  module List = struct
    let fold = List.fold
    let iter ?how:_ l ~f = List.iter l ~f
    let map ?how:_ l ~f = List.map l ~f
    let filter ?how:_ l ~f = List.filter l ~f
  end

  module Or_error = struct
    module List = struct

      let map ?(how = `Sequential) l ~f =
        let () = ignore how in
        let module M = struct
          exception E of Error.t
          let helper () = List.map l ~f:(fun x -> match f x with
            | Ok x -> x
            | Error e -> raise (E e)
          )
        end in
        try Ok (M.helper())
        with M.E e -> Error e

      let iter ?(how = `Sequential) l ~f =
        let () = ignore how in
        let module M = struct
          exception E of Error.t
          let helper () = List.iter l ~f:(fun x -> match f x with
            | Ok () -> ()
            | Error e -> raise (E e)
          )
        end in
        try Ok (M.helper())
        with M.E e -> Error e

    end
  end

end

let return = Deferred.return
let (>>=) x f = Deferred.bind x ~f
let (>>|) = Deferred.(>>|)
let (>>=?) = Deferred.Result.(>>=)
let (>>|?) = Deferred.Result.(>>|)
let fail = raise
let raise = `Use_fail_instead

let try_with f =
  try Ok (f ())
  with exn -> Error exn


module In_thread = struct
  let run f = f ()
end

module Pipe = struct
  module Reader = struct
    type 'a t = 'a Stream.t
  end

  let read r = match Stream.next r with
    | Some x -> `Ok x
    | None -> `Eof

  let junk = Stream.junk

  let peek_deferred r = match Stream.peek r with
    | Some x -> `Ok x
    | None -> `Eof

  let map = Stream.map
  let fold = Stream.fold
  let iter = Stream.iter

end

module Reader = struct
  module Read_result = struct
    type 'a t = [ `Eof | `Ok of 'a ]
  end

  type t = In_channel.t

  let open_file ?buf_len:_ file =
    In_channel.create file

  let close = In_channel.close

  let with_file ?buf_len file ~f =
    match buf_len with
    | None | Some _ -> In_channel.with_file file ~f

  let read_line ic =
    match In_channel.input_line ~fix_win_eol:true ic with
    | Some x -> `Ok x
    | None -> `Eof

  let read_all ic read_one =
    Stream.from (fun _ -> match read_one ic with
    | `Ok x -> Some x
    | `Eof -> In_channel.close ic; None
    )

  let lines ic = read_all ic read_line
  let contents = In_channel.input_all
  let file_contents = In_channel.read_all
  let file_lines = In_channel.read_lines

end

module Writer = struct
  type t = Out_channel.t

  let with_file ?perm ?append file ~f =
    Out_channel.with_file ?perm ?append file ~f

  let write = Out_channel.output_string
  let write_char = Out_channel.output_char
  let write_line t s = Out_channel.output_string t s; Out_channel.newline t
end

module Unix = Unix
