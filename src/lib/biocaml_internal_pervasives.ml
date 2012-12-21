module Arg = Core.Std.Arg
module Array = Core.Std.Array
include Array.Infix
module Backtrace = Core.Std.Backtrace
module Bag = Core.Std.Bag
module Big_int = Core.Std.Big_int
module Bigbuffer = Core.Std.Bigbuffer
module Bigstring = Core.Std.Bigstring
module Bigsubstring = Core.Std.Bigsubstring
module Bin_prot = Core.Std.Bin_prot
module Binary_packing = Core.Std.Binary_packing
module Bool = Core.Std.Bool
module Caml = Core.Std.Caml
module Char = Core.Std.Char
module Command = Core.Std.Command
include Core.Common
module Dequeue = Core.Std.Dequeue
module Exn = Core.Std.Exn
module Filename = Core.Std.Filename
module Float = Core.Std.Float
module Fn = Core.Std.Fn
module Hashtbl = Core.Std.Hashtbl
module Int = Core.Std.Int
include Int.Infix
module In_channel = Core.Std.In_channel
module Int32 = Core.Std.Int32
module Int63 = Core.Std.Int63
module Int64 = Core.Std.Int64
module Interfaces = Core.Std.Interfaces
include Interfaces
module Interval = Core.Std.Interval
module Lazy = Core.Std.Lazy
module List = Core.Std.List
include List.Infix
module Map = Core.Std.Map
module Monad = Core.Std.Monad
module Nat = Core.Std.Nat
module Nativeint = Core.Std.Nativeint
module Num = Core.Std.Num
module Option = Core.Std.Option
module Out_channel = Core.Std.Out_channel
module Printexc = Core.Std.Printexc
module Printf = Core.Std.Printf
include Printf
module Queue = Core.Std.Queue
module Random = Core.Std.Random
module Ratio = Core.Std.Ratio
module Result = Core.Std.Result
include Result.Export
module Set = Core.Std.Set
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

let flip f x y = f y x

module Stream = Biocaml_stream

module Lines = struct
  module Pos = Biocaml_pos

  exception Error of (Pos.t * string)
  let raise_error p m = raise (Error(p,m))

  let fold_stream' ?(file="") ?(strict=true) f init cstr =
    let lines = Stream.lines_of_chars cstr in
    let f accum s =
      try f accum s
      with Failure msg ->
        let n = Stream.count lines in
        let pos = if file = "" then Pos.l n else Pos.fl file n in
        if strict then raise_error pos msg else accum
    in
    Stream.fold ~f ~init lines

  let fold_stream ?(strict=true) f init cstr =
    fold_stream' ~strict f init cstr

  let fold_channel' ?(file="") ?(strict=true) f init cin =
    fold_stream' ~file ~strict f init (Stream.of_channel cin)

  let fold_channel ?(strict=true) f init cin =
    fold_stream ~strict f init (Stream.of_channel cin)

  let fold_file ?(strict=true) f init file =
    try
      try_finally_exn (fold_channel' ~file ~strict f init)
        ~fend:In_channel.close (open_in file)
    with Error (p,m) -> raise_error (Pos.set_file p file) m

  let iter_file ?(strict=true) f file =
    fold_file ~strict (fun _ x -> f x) () file

  let of_stream ?(strict=true) f (cstr : char Stream.t) =
    let lines = Stream.lines_of_chars cstr in
    let g ans s =
      try (f s)::ans
      with Failure m ->
        if strict
        then raise_error (Pos.l (Stream.count lines)) m
        else ans
    in List.rev (Stream.fold ~f:g ~init:[] lines)

  let of_channel ?(strict=true) f cin =
    of_stream ~strict f (Stream.of_channel cin)

end

module With_result = struct

  include Result
    
  let while_ok (type error) l ~(f:(int -> 'a -> ('b, error) Result.t)) =
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


module Order = struct
  let compose cmp1 cmp2 a b =
    let c1 = cmp1 a b in
    let c2 = cmp2 a b in
    let num_true = List.fold_left ~f:(fun cnt b -> if b then cnt+1 else cnt) ~init:0 in
    let bl = [c1 = Some (-1); c1 = Some 1; c2 = Some (-1); c2 = Some 1] in
    assert((num_true bl <= 1) || (c1 = Some 0 && c2 = Some 0));
    match c1 with
    | Some c1 -> c1
    | None ->
      match c2 with
      | Some c2 -> c2
      | None -> invalid_arg "neither partial order given relates given values"
        
  let reverse cmp a b = -(cmp a b)
  let reversep cmp a b = Option.map ~f:(~-) (cmp a b)
    
  let totalify cmp =
    fun a b ->
      match cmp a b with
      | Some c -> c
      | None -> failwith "order relation not defined for given elements"
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
