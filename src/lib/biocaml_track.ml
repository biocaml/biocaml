open Biocaml_internal_pervasives

type t = [
| `track of (string * string) list
| `comment of string
| `browser of
    [ `position of string * int * int | `hide of [`all] | `unknown of string ]
]
type 'a content = [
| `content of 'a
]
type track = t
type parse_error =
[ `incomplete_input of Biocaml_pos.t * string list * string option
| `wrong_browser_position of Biocaml_pos.t * string
| `wrong_key_value_format of (string * string) list * string * string ]

open Result

(*
  browser position chr19:49304200-49310700
  browser hide all
*)
let parse_chormpos position s =
  try begin match String.rindex s ':' with
  | Some colon ->
    let name = String.slice s 0 colon in
    begin match String.rindex s '-' with
    | Some dash ->
      let start = String.slice s (colon + 1) dash |! Int.of_string in
      let stop =  String.slice s (dash + 1) (String.length s) |! Int.of_string in
      return (`browser (`position (name, start, stop)))
    | None -> failwith "A"
    end
  | None -> failwith "B"
  end
  with
    e -> fail (`wrong_browser_position (position, s))

let parse_browser position line =
  let tokens =
    String.chop_prefix ~prefix:"browser " line
    |! Option.value ~default:""
    |! String.split_on_chars ~on:[' '; '\t'; '\r']
    |! List.filter ~f:((<>) "") in
  begin match tokens with
  | "position" :: pos :: [] -> parse_chormpos position pos
  | "hide" :: "all" :: [] -> return (`browser (`hide `all))
  | any -> return (`browser (`unknown line))
  end

let parse_track position stripped =
  let rec loop s acc =
    match Parse.escapable_string s ~stop_before:['='] with
    | (tag, Some '=', rest) ->
      begin match Parse.escapable_string rest ~stop_before:[' '; '\t'] with
      | (value, _, rest) ->
        let str = String.strip rest in
        if str = "" then return ((tag, value) :: acc)
        else loop str ((tag, value) :: acc)
      end
    | (str, _, rest) -> fail (`wrong_key_value_format (List.rev acc, str, rest))
  in
  loop stripped []
  >>= fun kv ->
  return (`track (List.rev kv))

let rec next p =
  let open Biocaml_transform.Line_oriented in
  let output_result = function
    | Ok o -> `output o
    | Error e -> `error e in
  match next_line p with
  | None -> `not_ready
  | Some "" -> `not_ready
  | Some l when String.(is_prefix (strip l) ~prefix:"#") ->
    `output (`comment String.(sub l ~pos:1 ~len:(length l - 1)))
  | Some l when String.strip l = "track"-> `output (`track [])
  | Some l when String.strip l = "browser" -> `output (`browser (`unknown l))
  | Some l when String.(is_prefix (strip l) ~prefix:"track ") ->
    parse_track (current_position p)
      (String.chop_prefix_exn ~prefix:"track " l |! String.strip)
    |! output_result
  | Some l when String.(is_prefix (strip l) ~prefix:"browser ") ->
    parse_browser (current_position p) l |! output_result
  | Some l -> `output (`content l)

let parser ?filename () =
  let name = sprintf "track_parser:%s" Option.(value ~default:"<>" filename) in
  let module LOP =  Biocaml_transform.Line_oriented  in
  let lo_parser = LOP.parser ?filename () in
  Biocaml_transform.make_stoppable ~name ()
    ~feed:(LOP.feed_string lo_parser)
    ~next:(fun stopped ->
      match next lo_parser with
      | `output r -> `output r
      | `error e -> `error e
      | `not_ready ->
        if stopped then (
          match LOP.finish lo_parser with
          | `ok -> `end_of_stream
          | `error (l, o) ->
            `error (`incomplete_input (LOP.current_position lo_parser, l, o))
        ) else
          `not_ready)

let needs_escaping s =
  String.exists s
    ~f:(function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> false | _ -> true)
let potentially_escape s =
  if needs_escaping s then sprintf "%S" s else s
    
let printer ?(add_content_new_line=true) () =
  let module PQ = Biocaml_transform.Printer_queue in
  let printer =
    PQ.make ~to_string:(function
    | `comment c -> sprintf "#%s\n" c
    | `track l ->
      sprintf "track %s\n"
        (List.map l (fun (k,v) ->
         sprintf "%s=%s" (potentially_escape k) (potentially_escape v))
         |! String.concat ~sep:" ")
    | `browser (`hide `all) ->
      "browser hide all\n"
    | `browser (`position (n, s, e)) ->
      sprintf "browser position %s:%d-%d\n" n s e
    | `browser (`unknown s) -> sprintf "browser %s\n" s
    | `content s ->
      if add_content_new_line then s ^ "\n" else s) () in
  Biocaml_transform.make_stoppable ~name:"string_track_printer" ()
    ~feed:(fun r -> PQ.feed printer r)
    ~next:(fun stopped ->
      match (PQ.flush printer) with
      | "" -> if stopped then `end_of_stream else `not_ready
      | s -> `output s)

let embed_parser embedded ?filename ~coerce_error ~coerce_output =
  let track_parser = parser ?filename () in
  Biocaml_transform.(
    on_error ~f:coerce_error
      (partially_compose
         track_parser
         ~destruct:(function
         | `content s -> `Yes (s ^ "\n")
         | `track _ | `browser _ | `comment _ as n -> `No n)
         embedded
         ~reconstruct:(function
         | `Filtered f -> f
         | `Done d -> coerce_output d)))

  
type wig_parser_error = [ parse_error | Biocaml_wig.parse_error ]
type wig_t = [ track | Biocaml_wig.t]
let wig_parser ?filename () =
  let wig_parser =
    Biocaml_wig.parser ~pedantic:false ~sharp_comments:true ?filename () in
  embed_parser wig_parser ?filename 
    ~coerce_error:(function
    | `left l -> (l :> wig_parser_error)
    | `right r ->  (r :> wig_parser_error))
    ~coerce_output:(fun o -> (o :> wig_t))

type gff_parse_error = [parse_error | Biocaml_gff.parse_error]
type gff_t = [track | Biocaml_gff.stream_item]
let gff_parser ?filename ?version () =
  let gff = Biocaml_gff.parser ?filename ?version () in
  embed_parser gff ?filename
    ~coerce_error: (function
    | `left l -> (l :> gff_parse_error)
    | `right r -> (r :> gff_parse_error))
    ~coerce_output:(fun o -> (o :> gff_t))

type bed_parse_error = [parse_error| Biocaml_bed.parse_error]
type bed_t = [track |  Biocaml_bed.t content ]
let bed_parser ?filename  ?more_columns  () =
  let bed = Biocaml_bed.parser ?filename ?more_columns () in
  embed_parser bed ?filename
    ~coerce_error: (function
    | `left l -> (l :> bed_parse_error)
    | `right r -> (r :> bed_parse_error))
    ~coerce_output:(fun o -> `content o)


let make_printer p ~split () =
  let track = printer ~add_content_new_line:false () in
  Biocaml_transform.(
    (on_error ~f:(function `left x -> x | `right x -> x)
       (compose
          (on_error ~f:(function `left x -> x | `right x -> x)
             (split_and_merge (identity ()) p
                ~merge:(function `left s -> s | `right r -> `content r)
                ~split))
          track))
  )
let wig_printer () =
  let wig = Biocaml_wig.printer () in
  make_printer wig ()
    ~split:(function
    | `comment _ | `track _ | `browser _ as x -> `left x
    | #Biocaml_wig.t as y -> `right y)
    
let gff_printer ?version () =
  let gff = Biocaml_gff.printer ?version () in
  make_printer gff ()
    ~split:(function
    | `comment _ | `track _ | `browser _ as x -> `left x
    | #Biocaml_gff.stream_item as y -> `right y)
    
let bed_printer () =
  let bed = Biocaml_bed.printer () in
  make_printer bed ()
    ~split:(function
    | `comment _ | `track _ | `browser _ as x -> `left x
    | `content y -> `right y)
 
