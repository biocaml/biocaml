open Core.Std
open Flow
open Biocaml_app_common
open Biocaml


module Genlex = struct
  include Genlex
  let sexp_of_token = function
    | Kwd s ->    Sexp.Atom (sprintf "[Kwd %S]" s)
    | Ident s ->  Sexp.Atom (sprintf "[Ident %S]" s)
    | Int s ->    Sexp.Atom (sprintf "[Int %d]" s)
    | Float s ->  Sexp.Atom (sprintf "[Float %f]" s)
    | String s -> Sexp.Atom (sprintf "[String %S]" s)
    | Char s ->   Sexp.Atom (sprintf "[Char %S]" (String.of_char s))

end
let spec_lexer = Genlex.make_lexer [":"; "with"; "without"; "="; "("; ")"]


let parse_spec_aux s =
  let open Genlex in
  let parse_meta_int = function
    | Int i :: t -> return (`int i, t)
    | Ident "random" :: Int m :: Int n :: t -> return (`random (m, n), t)
    | l -> error (`not_a_meta_int l)
  in
  let char_stream = String.to_list (s ^ " special_end") |! Stream.of_list in
  begin try
    let tokens = Biocaml_stream.to_list (spec_lexer char_stream) in
    match List.last tokens with
    | Some (Ident "special_end") ->
      return tokens
    | None -> error (`lexical "Nothing parsed")
    | Some i -> error (`lexical "Unfinished Input")
    with
    | Stream.Error e -> error (`lexical e)
  end
  >>= begin function
  | Ident f :: t when String.lowercase f = "fastq" ->
    let rec parse_fastq acc =  function
      | Ident "special_end" :: []
      | [] -> return acc
      | Kwd "with" :: Ident "N" :: t ->
        parse_fastq (`with_n :: acc) t
      | Kwd "without" :: Ident "N" :: t ->
        parse_fastq (`without_n :: acc) t
      | Kwd "with" :: Ident "read" :: Ident "length" :: Kwd "=" :: some ->
        parse_meta_int some
        >>= fun (meta_int, rest) ->
        parse_fastq (`read_length meta_int :: acc) rest
      | l -> error (`expecting_with_out_but_got l)
    in
    parse_fastq [] t
    >>= fun spec ->
    return (`fastq spec)
  | tokens ->
    error (`uknown_specification tokens)
  end

let parse_spec s =
  parse_spec_aux s
  >>< begin function
  | Ok o -> return o
  | Error e -> error (`parse_specification e)
  end


let io_with_out_channel out ?buffer_size ~f =
  begin match out with
  | `stdout -> return Lwt_io.stdout
  | `strerr -> return Lwt_io.stderr
  | `file file ->
    wrap_io (Lwt_io.open_file ~mode:Lwt_io.output ?buffer_size) file
  end
  >>= fun outchan ->
  begin
    f outchan
    >>< begin function
    | Ok o ->
      wrap_io Lwt_io.close outchan
      >>= fun () ->
      return o
    | Error e ->
      begin match out with
      | `file _ ->
        wrap_io Lwt_io.close outchan
        >>= fun _ ->
        error e
      | _ -> error e
      end
    end
  end

let io_fprintf out fmt =
  ksprintf (fun s -> wrap_io (Lwt_io.fprint out) s) fmt

let random_fastq_transform ~args () =
  let todo = ref 0 in
  let seq_num = ref 0 in
  let read_length_spec =
    List.find_map args (function `read_length m -> Some m | _ -> None)
    |! Option.value ~default:(`int 50) in
  let with_n =
    match List.find args (function `without_n -> true | _ -> false) with
    | Some _ -> false
    | _ -> true in
  let make_read () =
    let length =
      match read_length_spec with
      | `int n -> n
      | `random (n, m) -> n + Random.int m
    in
    String.init length (fun _ ->
      match Random.int (if with_n then 5 else 4) with
      | 0 -> 'A'
      | 1 -> 'C'
      | 2 -> 'G'
      | 3 -> 'T'
      | _ -> 'N')
  in
  let make_qualities read =
    String.map read (fun _ ->
      Phred_score.(of_int_exn (Random.int (126 - 33)) |! to_ascii_exn))
  in
  Transform.make ()
    ~next:(fun stopped ->
      match !todo, stopped with
      | 0, true -> `end_of_stream
      | 0, false -> `not_ready
      | n, _  when n < 0 -> assert false
      | n, _ ->
        decr todo;
        incr seq_num;
        let sequence = make_read () in
        let qualities = make_qualities sequence in
        `output (Fastq.(
          { name = sprintf "Random sequence: %d" !seq_num;
            sequence;
            comment = "";
            qualities } )))
    ~feed:(fun () -> incr todo)


let do_random ~output_file ~gzip ~nb_items spec =
  let zlib_buffer_size  = 4200 in
  let output_meta_channel =
    match output_file with
    | None -> `stdout
    | Some f -> `file f in
  parse_spec (String.concat ~sep:" " spec)
  >>= fun spec ->
  let tags, args =
    match spec with
    | `fastq args -> if gzip then (`gzip `fastq, args) else (`fastq, args)
  in
  output_transform_of_tags ~zlib_buffer_size tags
  >>= begin function
  | `to_fastq tr ->
    let transform = Transform.compose (random_fastq_transform ~args ()) tr in
    io_with_out_channel output_meta_channel (fun out ->
      let rec loop = function
        | 0 -> io_fprintf out "%!"
        | n ->
          Transform.feed transform ();
          begin match Transform.next transform with
          | `not_ready | `end_of_stream -> assert false
          | `output s ->
            io_fprintf out "%s" s
            >>= fun () ->
            loop (n - 1)
          end
      in
      loop nb_items)
    >>= fun () ->
    return ()
  | _ ->
    io_with_out_channel `stdout (fun out ->
      io_fprintf out "not to-fastq!\n%!")
    >>= fun () ->
    return ()
  end
  >>= fun () ->


  return ()




let stringify m =
  m >>< begin function
  | Ok o -> return o
  | Error e ->
    error (<:sexp_of< [
    | `io_exn of exn
    | `parse_specification of
        [ `expecting_with_out_but_got of Genlex.token list
        | `lexical of string
        | `not_a_meta_int of Genlex.token list
        | `uknown_specification of Genlex.token list ]
    ] >> e |! Sexp.to_string_hum)
  end

let do_random ~output_file ~gzip ~nb_items spec =
  stringify (do_random ~output_file ~gzip ~nb_items  spec)

let command =
  let open Command_line in
  let spec =
    let open Spec in
    empty
    ++ step (fun k v -> k ~output_file:v)
    +> flag "output-file" ~aliases:["o"] (optional string)
      ~doc:"<filename> output to a file"
    ++ step (fun k v -> k ~gzip:v)
    +> flag "gzip" ~aliases:["gz"] no_arg
      ~doc:" GZip the output"
    ++ step (fun k v -> k ~nb_items:v)
    +> flag "items" (optional_with_default 42 int)
      ~doc:"<nb> Number of records/items to produce"
    +> anon (sequence ("specification" %: string))
    ++ uses_lwt ()
  in
  basic ~summary:"Generate random files" spec do_random
