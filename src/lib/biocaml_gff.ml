open Biocaml_internal_pervasives

(*
  Version 2:
  http://www.sanger.ac.uk/resources/software/gff/spec.html
  http://gmod.org/wiki/GFF2
  
  Version 3:
  http://www.sequenceontology.org/gff3.shtml
  http://gmod.org/wiki/GFF3
*)

type t = {
  seqname: string;
  source: string option;
  feature: string option;
  pos: int * int;

  score: float option;
  strand: [`plus | `minus | `not_applicable | `unknown ];
  phase: int option;
  attributes: (string * string list) list;
}

type stream_item = [ `comment of string | `record of t ]

type tag = [ `version of [`two | `three] | `pedantic ] with sexp
let default_tags = [`version `three; `pedantic]

type parse_error = 
[ `cannot_parse_float of Biocaml_pos.t * string
| `cannot_parse_int of Biocaml_pos.t * string
| `cannot_parse_strand of Biocaml_pos.t * string
| `cannot_parse_string of Biocaml_pos.t * string
| `empty_line of Biocaml_pos.t
| `incomplete_input of
    Biocaml_pos.t * string list * string option
| `wrong_attributes of Biocaml_pos.t * string
| `wrong_row of Biocaml_pos.t * string
| `wrong_url_escaping of Biocaml_pos.t * string ]

module Transform = struct
  open With_result

    
  let parse_string msg pos i =
    begin try Ok (Scanf.sscanf i "%S " ident) with
    | e ->
      begin match (Scanf.sscanf i "%s " ident) with
      | "" -> Error (`cannot_parse_string (pos, msg))
      | s -> Url.unescape ~error:(fun s -> `wrong_url_escaping (pos, s)) s
      end
    end
  let parse_string_opt m pos i =
    parse_string m pos i >>= fun s ->
    begin match s with
    | "." -> return None
    | s -> return (Some s)
    end

  let parse_int msg pos i =
    parse_string msg pos i >>= fun s ->
    (try return (Int.of_string s)
     with e -> fail (`cannot_parse_int (pos, msg)))
      
  let parse_float_opt msg pos i =
    parse_string_opt msg pos i >>= function
    | Some s ->
      (try return (Some (Float.of_string s))
       with e -> fail (`cannot_parse_float (pos, msg)))
    | None -> return None
      
  let parse_int_opt msg pos i =
    parse_string_opt msg pos i >>= function
    | Some s ->
      (try return (Some (Int.of_string s))
       with e -> fail (`cannot_parse_int (pos, msg)))
    | None -> return None
      
  let parse_attributes_version_3 position i =
    let whole_thing = String.concat ~sep:"\t" i in
  (*   let b = Buffer.create 42 in *)
  (*   String.iter (String.concat ~sep:"\t" i) (function *)
  (*   | ' ' -> Buffer.add_string b "%20" *)
  (*   | c -> Buffer.add_char b c); *)
  (*   Buffer.contents b *)
  (* in *)
    let get_csv s =
      List.map (String.split ~on:',' s)
        (fun s -> parse_string "value" position String.(strip s))
      |! List.partition_map ~f:Result.ok_fst
      |! (function
        | (ok, []) -> return ok
        | (_, notok :: _) -> fail notok) in
    let rec loop pos acc =
      begin match String.lfindi whole_thing ~pos ~f:(fun _ c -> c = '=') with
      | Some equal ->
        parse_string "tag" position (String.slice whole_thing pos equal)
        >>= fun tag ->
        let pos = equal + 1 in
        begin match String.lfindi whole_thing ~pos ~f:(fun _ c -> c = ';') with
        | Some semicolon ->
          let delimited = String.slice whole_thing pos semicolon in
          get_csv delimited
          >>= fun values ->
          loop (semicolon + 1) ((tag, values) :: acc)
        | None ->
          let delimited = String.(sub whole_thing pos (length whole_thing - pos)) in
          get_csv delimited
          >>= fun values ->
          return ((tag, values) :: acc)
        end
      | None ->
        if pos >= String.length whole_thing then
          return acc
        else
          fail (`wrong_attributes (position, whole_thing))
      end
    in
    (try loop 0 [] with e -> fail (`wrong_attributes (position, whole_thing)))
    >>| List.rev

  let parse_attributes_version_2 position l =
    let whole_thing = String.(concat ~sep:"\t" l |! strip) in
    let parse_string i =
      begin try Some (Scanf.bscanf i "%S " ident) with
      | e ->
        begin match (Scanf.bscanf i "%s " ident) with
        | "" -> None
        | s -> Some s
        end
      end
    in
    let inch = Scanf.Scanning.from_string whole_thing in
    let tokens = Stream.(from (fun _ -> parse_string inch) |! npeek max_int) in
    let rec go_3_by_3 acc = function
      | k  :: v :: ";" :: rest -> go_3_by_3 ((k, [v]) :: acc) rest
      | [] | [";"] -> return (List.rev acc)
      | problem -> fail (`wrong_attributes (position, whole_thing))
    in
    go_3_by_3 [] tokens

      
  let parse_row ~version pos s =
    let fields = String.split ~on:'\t' s in
    begin match fields with
    | seqname :: source :: feature :: start :: stop :: score :: strand :: phase
      :: rest ->
      let result =
        parse_string "Sequence name" pos seqname >>= fun seqname ->
        parse_string_opt "Source" pos source >>= fun source ->
        parse_string_opt "Feature" pos feature >>= fun feature ->
        parse_int "Start Position" pos start >>= fun start ->
        parse_int "Stop Position" pos stop >>= fun stop ->
        parse_float_opt "Score" pos score >>= fun score ->
        parse_string_opt "Strand" pos strand
        >>= (function
        | Some "+" -> return `plus
        | None -> return `not_applicable
        | Some "-" -> return `minus
        | Some "?" -> return `unknown
        | Some s -> fail (`cannot_parse_strand (pos, s)))
        >>= fun strand ->
        parse_int_opt "Phase/Frame" pos phase >>= fun phase ->
        begin match version with
        | `two -> parse_attributes_version_2 pos rest
        | `three -> parse_attributes_version_3 pos rest
        end
        >>= fun attributes ->
        return (`record {seqname; source; feature; pos = (start, stop); score;
                         strand; phase; attributes})
      in
      output_result result

    | other ->
      output_error (`wrong_row (pos, s))
    end
      
  let rec next ?(pedantic=true) ?(sharp_comments=true) ?(version=`three) p =
    let open Biocaml_transform.Line_oriented in
    let open Result in
    match next_line p with
    | None -> `not_ready
    | Some "" ->
      if pedantic
      then output_error (`empty_line (current_position p))
      else next ~pedantic ~sharp_comments ~version p
    | Some l when sharp_comments && String.(is_prefix (strip l) ~prefix:"#") ->
      output_ok (`comment String.(sub l ~pos:1 ~len:(length l - 1)))
    | Some l -> parse_row ~version (current_position p) l

  let string_to_item ?filename ?(tags=default_tags) () =
    let name = sprintf "gff_parser:%s" Option.(value ~default:"<>" filename) in
    let pedantic = List.mem tags  `pedantic in
    let version =
      List.find_map tags (function `version v -> Some v | _ -> None) in
    let next = next ~pedantic ?version in
    Biocaml_transform.Line_oriented.make_stoppable_merge_error
      ~name ?filename ~next ()
      
      
  let item_to_string ?(tags=default_tags) () =
    let module PQ = Biocaml_transform.Printer_queue in
    let version =
      List.find_map tags (function `version v -> Some v | _ -> None)
      |! Option.value ~default:`three in
    let printer =
      PQ.make () ~to_string:(function
      | `comment c -> sprintf "#%s\n" c
      | `record t ->
        let escape =
          match version with | `three -> Url.escape | `two -> sprintf "%S" in
        let optescape  o =  Option.value_map ~default:"." o ~f:escape in
        String.concat ~sep:"\t" [
          escape t.seqname;
          optescape t.source;
          optescape t.feature;
          sprintf "%d" (fst t.pos);
          sprintf "%d" (snd t.pos);
          Option.value_map ~default:"." ~f:(sprintf "%g") t.score;
          (match t.strand with`plus -> "+" | `minus -> "-"
          | `not_applicable -> "." | `unknown -> "?");
          Option.value_map ~default:"." ~f:(sprintf "%d") t.phase;
        String.concat ~sep:";"
          (List.map t.attributes (fun (k,v) ->
            match version with
            | `three ->
              sprintf "%s=%s" (Url.escape k)
                (List.map v Url.escape |! String.concat ~sep:",")
            | `two ->
              sprintf "%S %s" k
                (List.map v escape |! String.concat ~sep:",")
           ));
      ] ^ "\n"
      ) in
    Biocaml_transform.make_stoppable ~name:"gff_printer" ()
      ~feed:(fun r -> PQ.feed printer r)
      ~next:(fun stopped ->
        match (PQ.flush printer) with
        | "" -> if stopped then `end_of_stream else `not_ready
        | s -> `output s)
end

