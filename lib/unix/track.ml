open Core_kernel

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

module Error = struct
  type parsing =
    [ `incomplete_input of Pos.t * string list * string option
    | `wrong_browser_position of Pos.t * string
    | `wrong_key_value_format of (string * string) list * string * string ]
  [@@deriving sexp]

  type t = parsing [@@deriving sexp]

end


module Transform = struct

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
        let start = String.slice s (colon + 1) dash |> Int.of_string in
        let stop =  String.slice s (dash + 1) (String.length s) |> Int.of_string in
        Ok (`browser (`position (name, start, stop)))
      | None -> failwith "A"
      end
    | None -> failwith "B"
    end
    with
      _ -> Error (`wrong_browser_position (position, s))

  let parse_browser position line =
    let tokens =
      String.chop_prefix ~prefix:"browser " line
      |> Option.value ~default:""
      |> String.split_on_chars ~on:[' '; '\t'; '\r']
      |> List.filter ~f:((<>) "") in
    begin match tokens with
    | "position" :: pos :: [] -> parse_chormpos position pos
    | "hide" :: "all" :: [] -> Ok (`browser (`hide `all))
    | _ -> Ok (`browser (`unknown line))
    end

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
  let escapable_string
      (s:string)
      ~(stop_before : char list)
      : (string * char option * string)
      =
    let try_escaped s =
      try Some (Scanf.sscanf s "%S%n" (fun s n -> (s,n))) with _ -> None in
    let lgth_s = String.length s in
    begin match try_escaped s with
    | Some (found, chars_read) ->
      if chars_read < lgth_s then (
        if List.exists stop_before ~f:((=) s.[chars_read]) then
          (found, Some s.[chars_read],
           String.slice s (chars_read + 1) (String.length s))
        else
          (found, None, String.slice s chars_read (String.length s))
      ) else
        (found, None, "")
    | None ->
      begin match String.lfindi s ~f:(fun _ c -> List.exists stop_before ~f:((=) c)) with
      | Some idx ->
        (String.sub s ~pos:0 ~len:idx, Some s.[idx],
         String.slice s (idx + 1) (String.length s))
      | None -> (s, None, "")
      end
    end

  let parse_track stripped =
    let open Result.Monad_infix in
    let rec loop s acc =
      match escapable_string s ~stop_before:['='] with
      | (tag, Some '=', rest) ->
        begin match escapable_string rest ~stop_before:[' '; '\t'] with
        | (value, _, rest) ->
          let str = String.strip rest in
          if str = "" then Ok ((tag, value) :: acc)
          else loop str ((tag, value) :: acc)
        end
      | (str, _, rest) -> Error (`wrong_key_value_format (List.rev acc, str, rest))
    in
    loop stripped []
    >>= fun kv ->
    Ok (`track (List.rev kv))

  let rec next p =
    let open Lines.Buffer in
    match (next_line p :> string option) with
    | None -> `not_ready
    | Some "" -> next p
    | Some l when String.(is_prefix (strip l) ~prefix:"#") ->
      `output (Ok (`comment String.(sub l ~pos:1 ~len:(length l - 1))))
    | Some l when String.strip l = "track"-> `output (Ok (`track []))
    | Some l when String.strip l = "browser" -> `output (Ok (`browser (`unknown l)))
    | Some l when String.(is_prefix (strip l) ~prefix:"track ") ->
      parse_track (String.chop_prefix_exn ~prefix:"track " l |> String.strip)
      |> (fun x -> `output x)
    | Some l when String.(is_prefix (strip l) ~prefix:"browser ") ->
      parse_browser (current_position p) l |> (fun x -> `output x)
    | Some l -> `output (Ok (`content l))

  let string_to_string_content ?filename () =
    let name = sprintf "track_parser:%s" Option.(value ~default:"<>" filename) in
    Lines.Transform.make_merge_error ~name ?filename ~next ()

  let needs_escaping s =
    String.exists s
      ~f:(function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> false | _ -> true)
  let potentially_escape s =
    if needs_escaping s then sprintf "%S" s else s

  let string_content_to_string ?(add_content_new_line=true) () =
    let to_string = function
      | `comment c -> sprintf "#%s\n" c
      | `track l ->
        sprintf "track %s\n"
          (List.map l ~f:(fun (k,v) ->
            sprintf "%s=%s" (potentially_escape k) (potentially_escape v))
            |> String.concat ~sep:" ")
      | `browser (`hide `all) ->
        "browser hide all\n"
      | `browser (`position (n, s, e)) ->
        sprintf "browser position %s:%d-%d\n" n s e
      | `browser (`unknown s) -> sprintf "browser %s\n" s
      | `content s ->
        if add_content_new_line then s ^ "\n" else s in
    Tfxm.of_function ~name:"track_to_string" to_string

  let embed_parser ?filename =
    let track_parser = string_to_string_content ?filename () in
    Tfxm.filter_compose
      track_parser
      ~destruct:(function
      | Ok (`content s) -> `transform (s ^ "\n")
      | Ok (`track _) | Ok (`browser _) | Ok (`comment _)
      | Error _ as n -> `bypass n)

  type wig_parser_error = [ Error.parsing | Wig.Error.parsing ]
  type wig_t = [ track | Wig.item]

  let string_to_wig ?filename () =
    let wig_parser =
      Wig.Transform.string_to_item ?filename () in
    embed_parser ?filename
      (*
    let track_parser = string_to_string_content ?filename () in
    Tfxm.filter_compose
      track_parser
      ~destruct:(function
      | Ok (`content s) -> `Yes (s ^ "\n")
      | Ok (`track _) | Ok (`browser _) | Ok (`comment _)
      | Error _ as n -> `No n) *)
      wig_parser
      ~reconstruct:(function
      | `bypassed (Ok f) -> Ok (f :> wig_t)
      | `bypassed (Error f) -> Error (f :> [> wig_parser_error])
      | `transformed (Ok o) -> Ok (o :> wig_t)
      | `transformed (Error e) -> Error (e :> [> wig_parser_error]))

  type gff_parse_error = [Error.parsing | Gff.Error.parsing]
  type gff_t = [track | Gff.item]
  let string_to_gff ?filename ~tags () =
    let gff = Gff.Transform.string_to_item ?filename () in
    embed_parser  ?filename (gff ~tags)
      ~reconstruct:(function
      | `bypassed (Ok f) -> Ok (f :> gff_t)
      | `bypassed (Error f) -> Error (f :> [> gff_parse_error])
      | `transformed (Ok o) -> Ok (o :> gff_t)
      | `transformed (Error e) -> Error (e :> [> gff_parse_error]))

  type bed_parse_error = [Error.parsing| Bed.Error.parsing]
  type bed_t = [track |  Bed.item content ]

  let string_to_bed ?filename  ?more_columns  () =
    let bed = Bed.Transform.string_to_item ?more_columns () in
    embed_parser  ?filename bed
      ~reconstruct:(function
      | `bypassed (Ok f) -> Ok (f :> bed_t)
      | `bypassed (Error f) -> Error (f :> [> bed_parse_error])
      | `transformed (Ok o) -> Ok (`content o :> bed_t)
      | `transformed (Error e) -> Error (e :> [> bed_parse_error]))


  let make_printer p ~split () =
    let track = string_content_to_string ~add_content_new_line:false () in
    Tfxm.(
      compose
        (split_and_merge (identity ()) p
           ~merge:(function `left s -> s | `right r -> `content r)
           ~split)
        track)

  let wig_to_string () =
    let wig = Wig.Transform.item_to_string () in
    make_printer wig ()
      ~split:(function
      | `comment _ | `track _ | `browser _ as x -> `left x
      | #Wig.item as y -> `right y)

  let gff_to_string ~tags () =
    let gff = Gff.Transform.item_to_string ~tags () in
    make_printer gff ()
      ~split:(function
      | `comment _ | `track _ | `browser _ as x -> `left x
      | #Gff.item as y -> `right y)

  let bed_to_string () =
    let bed = Bed.Transform.item_to_string () in
    make_printer bed ()
      ~split:(function
      | `comment _ | `track _ | `browser _ as x -> `left x
      | `content y -> `right y)

end
