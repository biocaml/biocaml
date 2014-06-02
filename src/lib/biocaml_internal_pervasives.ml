module Line = Biocaml_line
module Pos = Biocaml_pos

module Stream = CFStream_stream

include Core.Common
let ( |? ) x default = Core.Option.value ~default x
module List = Core.Std.List
module Array = struct
  include Core.Std.Array
  let range xs = Stream.Infix.(0 --^ (length xs))
end
include Array.Infix
module Arg = Core.Std.Arg
module Backtrace = Core.Std.Backtrace
module Bag = Core.Std.Bag
module Bigbuffer = Core.Std.Bigbuffer
module Bigstring = Core.Std.Bigstring
module Bigsubstring = Core.Std.Bigsubstring
module Bin_prot = Core.Std.Bin_prot
module Binary_packing = Core.Std.Binary_packing
module Bool = Core.Std.Bool
module Buffer = Core.Std.Caml.Buffer
module Caml = Core.Std.Caml
module Char = Core.Std.Char
module Command = Core.Std.Command
module Dequeue = Core.Std.Dequeue
module Error = Core.Std.Error
module Exn = Core.Std.Exn
module Filename = struct
  include Core.Std.Filename

  module Infix = struct
    let (/) = concat
  end
end
module Float = Core.Std.Float
module Fn = Core.Std.Fn
module Hashtbl = Core.Std.Hashtbl
module Int = Core.Std.Int
module In_channel = Core.Std.In_channel
module Int32 = Core.Std.Int32
module Int63 = Core.Std.Int63
module Int64 = Core.Std.Int64
module Interfaces = Core.Std.Interfaces
include Interfaces
module Interval = Core.Std.Interval
module Lazy = Core.Std.Lazy
include List.Infix
module Map = Core.Std.Map
module Monad = Core.Std.Monad
module Nativeint = Core.Std.Nativeint
module Option = Core.Std.Option
module Or_error = Core.Std.Or_error
module Out_channel = Core.Std.Out_channel
module Printexc = Core.Std.Printexc
module Printf = Core.Std.Printf
include Printf
module Queue = Core.Std.Queue
module Random = Core.Std.Random
module Result = struct

  include Core.Std.Result

  let while_ok (type error) l ~(f:(int -> 'a -> ('b, error) t)) =
    let module M = struct
      exception E of error
      let the_fun () =
        let run () =
          List.mapi l (fun i x ->
            match f i x with
            | Ok o -> o
            | Error e -> raise (E e))
        in
        try Ok (run ())
        with
        | E e -> Error e
    end in
    M.the_fun ()

  let output_result r = `output r
  let output_ok o = `output (Ok o)
  let output_error e = `output (Error e)

end
include Result.Export
module Set = Core.Std.Set
module Sexp = Core.Std.Sexp
module Sexpable = Core.Std.Sexpable
include Sexplib.Conv
module Stack = Core.Std.Stack
module String = Core.Std.String
include String.Infix
module Sys = Core.Std.Sys
module Time = Core.Std.Time

let try_finally_exn ~fend f x =
  match try `V (f x) with e -> `E e with
    | `V f_x -> fend x; f_x
    | `E e -> (try fend x with _ -> ()); raise e

let open_out_safe = open_out_gen [Open_wronly; Open_creat; Open_excl; Open_text] 0o666

module Url = struct

  let escape s =
    let b = Buffer.create (String.length s) in
    String.iter s (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' as c -> Buffer.add_char b c
    | anyother -> Buffer.add_string b (sprintf "%%%02X" (Char.to_int anyother)));
    Buffer.contents b

  let unescape s ~error =
    (* ?(error= fun s -> `wrong_url_string s) s =; *)
    let buf = Buffer.create (String.length s) in
    let rec loop pos =
      match String.lfindi s ~pos ~f:(fun _ c -> (=) '%' c) with
      | None ->
        Buffer.add_substring buf s pos String.(length s - pos)
      | Some idx ->
        if String.length s >= idx + 2 then (
          let char = Scanf.sscanf (String.sub s (idx + 1) 2) "%x" ident in
          Buffer.add_substring buf s pos String.(idx - pos);
          Buffer.add_char buf (Char.of_int_exn char);
          loop (idx + 3)
        ) else (
          failwith "A"
        )
  in
    try loop 0; Ok (Buffer.contents buf) with
    | e -> Error (error s)

end

module Parse = struct

  let escapable_string s ~stop_before =
    let try_escaped s =
      try Some (Scanf.sscanf s "%S%n" (fun s n -> (s,n))) with e -> None in
    let lgth_s = String.length s in
    begin match try_escaped s with
    | Some (found, chars_read) ->
      if chars_read < lgth_s then (
        if List.exists stop_before ((=) s.[chars_read]) then
          (found, Some s.[chars_read],
           String.slice s (chars_read + 1) (String.length s))
        else
          (found, None, String.slice s chars_read (String.length s))
      ) else
        (found, None, "")
    | None ->
      begin match String.lfindi s ~f:(fun _ c -> List.exists stop_before ((=) c)) with
      | Some idx ->
        (String.sub s 0 idx, Some s.[idx],
         String.slice s (idx + 1) (String.length s))
      | None -> (s, None, "")
      end
    end

end

module Debug = struct

  let debugged = ref []

  let enable s =
    debugged := s :: !debugged
  let disable s =
    debugged := List.filter !debugged ((<>) s)

  let is_enabled s =
    List.mem !debugged s

  let make prefix fmt =
    ksprintf (fun s ->
      if is_enabled prefix then (
        eprintf "%s: " prefix;
        String.iter s (function
        | '\n' -> eprintf "\n    "
        | c -> eprintf "%c" c);
        eprintf "\n%!"
      )
    ) fmt


end
