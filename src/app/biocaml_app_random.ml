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
let spec_lexer = Genlex.make_lexer [":"; "with"; "without"; "="; "("; ")"; "and"]


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
    match List.rev tokens with
    | (Ident "special_end") :: more ->
      return (List.rev more)
    | [] -> error (`lexical "Nothing parsed")
    | i -> error (`lexical "Unfinished Input")
    with
    | Stream.Error e -> error (`lexical e)
  end
  >>= begin function
  | Ident f :: t when String.lowercase f = "fastq" ->
    let rec parse_fastq acc =  function
      | [] -> return acc
      | Kwd "and" :: [] -> error (`expecting_spec_but_got [])
      | Kwd "and" :: q :: t when acc <> [] ->
        parse_fastq acc (q :: t)
      | Kwd "with" :: Ident "N" :: t ->
        parse_fastq (`with_n :: acc) t
      | Kwd "without" :: Ident "N" :: t ->
        parse_fastq (`without_n :: acc) t
      | Kwd "with" :: Ident "read" :: Ident "length" :: Kwd "=" :: some ->
        parse_meta_int some
        >>= fun (meta_int, rest) ->
        parse_fastq (`read_length meta_int :: acc) rest
      | l -> error (`expecting_spec_but_got l)
    in
    parse_fastq [] t
    >>= fun spec ->
    return (`fastq spec)
  | Ident f :: t when String.lowercase f = "bed" ->
    let rec get_columns acc = function
      | []
      | Kwd "and" :: _ as l -> return (acc, l)
      | Ident "int" :: l -> get_columns (`int :: acc) l
      | Ident "float" :: l -> get_columns (`float :: acc) l
      | l -> error (`expecting_column_type_or_closing_parenthesis l)
    in
    let rec parse_bed acc = function
      | [] -> return acc
      | Kwd "and" :: [] -> error (`expecting_spec_but_got [])
      | Kwd "and" :: q :: t when acc <> [] ->
        parse_bed acc (q :: t)
      | Kwd "with" :: Ident "columns" :: t ->
        get_columns [] t
        >>= fun (cols, rest) ->
        parse_bed (`columns (List.rev cols) :: acc) rest
      | l -> error (`expecting_spec_but_got l)
    in
    parse_bed [] t
    >>= fun args ->
    return (`bed args)
  | tokens ->
    error (`uknown_specification tokens)
  end

let parse_spec s =
  parse_spec_aux s
  >>< begin function
  | Ok o -> return o
  | Error e -> error (`parse_specification e)
  end


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

let random_bed_transform ~args () =
  let columns =
    List.find_map args (function `columns m -> Some m | _ -> None)
    |! Option.value ~default:[] in
  let todo = ref 0 in
  let seq_num = ref 0 in
  let make_item seqnum =
    let span = Random.int 4242 in
    let start = Random.int 3 *  seqnum in
    let rest =
      List.map columns (function
      | `int -> `Int (Random.int 42)
      | `float -> `Float (Random.float 1000.)) in
    (sprintf "chr%d" seqnum, start, start + span, rest)
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
        `output (make_item !seq_num))
    ~feed:(fun () -> incr todo)

let do_output output_meta_channel transform nb_items =
  IO.with_out_channel output_meta_channel (fun out ->
    let rec loop = function
      | 0 -> return ()
      | n ->
        Transform.feed transform ();
        begin match Transform.next transform with
        | `not_ready | `end_of_stream -> assert false
        | `output s ->
          IO.write out s
          >>= fun () ->
          loop (n - 1)
        end
    in
    loop nb_items)

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
    | `bed args -> if gzip then (`gzip `bed, args) else (`bed, args)
  in
  output_transform_of_tags ~zlib_buffer_size tags
  >>= begin function
  | `to_fastq tr ->
    let transform = Transform.compose (random_fastq_transform ~args ()) tr in
    do_output output_meta_channel transform nb_items
  | `to_bed tr ->
    let transform = Transform.compose (random_bed_transform ~args ()) tr in
    do_output output_meta_channel transform nb_items

  | _ -> error (`not_implemented)
  end
  >>= fun () ->


  return ()




let stringify m =
  m >>< begin function
  | Ok o -> return o
  | Error e ->
    error (<:sexp_of< [
    | `io_exn of exn
    | `not_implemented
    | `parse_specification of
        [ `expecting_spec_but_got of Genlex.token list
        | `expecting_column_type_or_closing_parenthesis of Genlex.token list
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
