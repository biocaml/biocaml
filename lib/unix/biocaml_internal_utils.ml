open Core.Std

module Line = Biocaml_line
module Pos = Biocaml_pos

module Stream = CFStream_stream

let ( |? ) x default = Core.Option.value ~default x

module Array = struct
  include Core.Std.Array
  let range xs = Stream.Infix.(0 --^ (length xs))
end

module Result = struct

  include Core.Std.Result

  module List = struct

    let mapi (type error) l ~(f:(int -> 'a -> ('b, error) t)) =
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

    let map l ~f = mapi l ~f:(fun _ x -> f x)

  end

end

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

let compare_of_list ?(equal = (=)) l =
  fun a b ->
    let i,_ = Option.value_exn (List.findi l ~f:(fun _ a' -> equal a a')) in
    let j,_ = Option.value_exn (List.findi l ~f:(fun _ b' -> equal b b')) in
    Int.compare i j
