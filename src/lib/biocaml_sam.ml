open Biocaml_internal_pervasives


type raw_alignment = {
  qname : string;
  flag : int;
  rname : string;
  pos : int;
  mapq : int;
  cigar : string;
  rnext : string;
  pnext : int;
  tlen : int;
  seq : string;
  qual : string;
  optional : (string * char * string) list
}

type raw_item = [
| `comment of string
| `header of string * (string * string) list
| `alignment of raw_alignment
]
type raw_parsing_error = [
| `incomplete_input of Biocaml_pos.t * string list * string option
| `invalid_header_tag of Biocaml_pos.t * string
| `invalid_tag_value_list of Biocaml_pos.t * string list
| `not_an_int of Biocaml_pos.t * string * string
| `wrong_alignment of Biocaml_pos.t * string
| `wrong_optional_field of Biocaml_pos.t * string
]
open Result


let is_tag_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
  | _ -> false
let is_valid_tag s =
  if String.length s = 2 then (is_tag_char s.[0] && is_tag_char s.[1])
  else false

let parse_header_line position line =
  match String.(split ~on:'\t' (chop_prefix_exn line ~prefix:"@")) with
  | [] -> assert false
  | "CO" :: rest -> return (`comment (String.concat ~sep:"\t" rest))
  | tag :: values ->
    if is_valid_tag tag then (
      let tag_values () =
        List.map values (fun v ->
          match String.split ~on:':' v with
          | tag :: value :: [] ->
            if is_valid_tag tag then (tag, value)
            else failwith "A"
          | other ->  failwith "A") in
      begin try return (tag_values ()) with
      | Failure _ -> fail (`invalid_tag_value_list (position, values))
      end
      >>= fun tv ->
      return (`header (tag, tv))
    ) else
      fail (`invalid_header_tag (position, tag))

let parse_optional_field position s =
  match String.split s ~on:':' with
  | [tag; typ; value] ->
    if is_valid_tag tag then
      begin match typ with
      | "A" | "i" | "f" | "Z" | "H" | "B" ->
        return (tag, typ.[0], value)
      | _ ->
        fail (`wrong_optional_field (position, s))
      end
    else
      fail (`wrong_optional_field (position, s))
  | _ ->
    fail (`wrong_optional_field (position, s))

let parse_alignment position s  =
  let int field x =
    try return (int_of_string x)
    with Failure _ -> fail (`not_an_int (position, field, x)) in
  match String.split s ~on:'\t' with
  | qname  ::  flag :: rname :: pos :: mapq :: cigar :: rnext :: pnext
    :: tlen :: seq :: qual :: optional ->
    begin
      int "flag" flag >>= fun flag ->
      int "pos" pos >>= fun pos ->
      int "mapq" mapq >>= fun mapq ->
      int "pnext" pnext >>= fun pnext ->
      int "tlen" tlen >>= fun tlen ->
      Result_list.while_ok optional (fun _ -> parse_optional_field position)
      >>= fun optional ->
      return (`alignment {
        qname;  flag; rname; pos; mapq; cigar; rnext;
        pnext; tlen; seq; qual; optional })
    end
  | _ ->
    fail (`wrong_alignment (position, s))
  
let rec next p =
  let open Biocaml_transform.Line_oriented in
  let output_result = function
    | Ok o -> `output o
    | Error e -> `error e in
  match next_line p with
  | None -> `not_ready
  | Some "" -> next p
  | Some l when String.(is_prefix (strip l) ~prefix:"@") ->
    parse_header_line (current_position p) l |! output_result
  | Some l ->
    parse_alignment (current_position p) l |! output_result
  
let raw_parser ?filename () =
  let name = sprintf "sam_raw_parser:%s" Option.(value ~default:"<>" filename) in
  Biocaml_transform.Line_oriented.stoppable_parser ~name ?filename ~next ()

type reference_sequence = {
  ref_name: string;
  ref_length: int;
  ref_assembly_identifier: string option;
  ref_checksum: string option;
  ref_species: string option;
  ref_uri: string option;
  ref_unknown: (string * string) list;
}
let reference_sequence
    ?assembly_identifier ?checksum ?species ?uri ?(unknown_data=[]) name length =
  { 
    ref_name                = name               ;
    ref_length              = length             ;
    ref_assembly_identifier = assembly_identifier;
    ref_checksum            = checksum           ;
    ref_species             = species            ;
    ref_uri                 = uri                ;
    ref_unknown             = unknown_data ;
  }
let reference_sequence_to_header rs =
  ("SN", rs.ref_name)
  :: ("LN", Int.to_string rs.ref_length)
  :: (List.filter_opt [
    Option.map rs.ref_assembly_identifier (fun s -> ("AS", s));
    Option.map rs.ref_checksum (fun s -> ("M5", s));
    Option.map rs.ref_species (fun s -> ("SP", s));
    Option.map rs.ref_uri (fun s -> ("UR", s)); ]
      @ rs.ref_unknown)
    
    
module Flags = struct 
  type t = int

  let of_int = ident
    
  let flag_is_set s f = (f land s) <> 0
    
  let has_multiple_segments            = flag_is_set 0x1
  let each_segment_properly_aligned    = flag_is_set 0x2
  let segment_unmapped                 = flag_is_set 0x4
  let next_segment_unmapped            = flag_is_set 0x8
  let seq_is_reverse_complemented      = flag_is_set 0x10
  let next_seq_is_reverse_complemented = flag_is_set 0x20
  let first_segment                    = flag_is_set 0x40
  let last_segment                     = flag_is_set 0x80
  let secondary_alignment              = flag_is_set 0x100
  let not_passing_quality_controls     = flag_is_set 0x200
  let pcr_or_optical_duplicate         = flag_is_set 0x400
end
  
type cigar_op = [
| `D of int
| `Eq of int
| `H of int
| `I of int
| `M of int
| `N of int
| `P of int
| `S of int
| `X of int ]


type optional_content_value = [
| `array of (char * optional_content_value array)
| `char of char
| `float of float
| `int of int
| `string of string ]
type optional_content = (string * char * optional_content_value) list   
type alignment = {
  query_template_name: string;
  flags: Flags.t;
  reference_sequence: [ `reference_sequence of reference_sequence
                      | `none
                      | `name of string ];
  position: int option;
  mapping_quality: int option;
  cigar_operations: cigar_op array;

  next_ref_name: [`qname | `none | `name of string
                 | `reference_sequence of reference_sequence ];
  next_ref_position: int option;

  template_length: int option;

  sequence: [ `string of string | `reference | `none];
  quality: Biocaml_phred_score.t array;

  optional_content: optional_content;
}
type item = [
| `comment of string
| `header_line of
    string * [`unknown | `unsorted | `queryname | `coordinate ] *
      (string * string) list
| `reference_sequence_dictionary of reference_sequence array
| `header of string * (string * string) list
| `alignment of alignment
]

type optional_content_parsing_error = [
| `wrong_optional of (string * char * string) list *
    [ `not_a_char of string
    | `not_a_float of string
    | `not_an_int of string
    | `unknown_type of char
    | `wrong_array of
        [ `not_a_char of string
        | `not_a_float of string
        | `not_an_int of string
        | `wrong_type of string 
        | `unknown_type of char ]
    | `wrong_type of string ]
]
  
type item_parsing_error = [
| `comment_after_end_of_header of int * string
| `duplicate_in_reference_sequence_dictionary of reference_sequence array
| `header_after_end_of_header of int * (string * (string * string) list)
| `header_line_not_first of int
| `header_line_without_version of (string * string) list
| `header_line_wrong_sorting of string
| `missing_ref_sequence_length of (string * string) list
| `missing_ref_sequence_name of (string * string) list
| `wrong_cigar_text of string
| `wrong_flag of raw_alignment
| `wrong_mapq of raw_alignment
| `wrong_phred_scores of raw_alignment
| `wrong_pnext of raw_alignment
| `wrong_pos of raw_alignment
| `wrong_qname of raw_alignment
| `wrong_ref_sequence_length of (string * string) list
| `wrong_tlen of raw_alignment 
| optional_content_parsing_error
]


let expand_header_line l =
  let version = ref None in
  let sorting_order = ref (Ok `unknown) in
  let unknown =
    List.filter_map l (function
    | ("VN", s) -> version := Some s; None
    | "SO", "unknown" ->     sorting_order := Ok `unknown; None
    | "SO", "unsorted" ->    sorting_order := Ok `unsorted; None
    | "SO", "queryname" ->   sorting_order := Ok `queryname; None
    | "SO", "coordinate" ->  sorting_order := Ok `coordinate; None
    | "SO", any -> sorting_order := Error any; None
    | other -> Some other) in
  match !version, !sorting_order with
  | Some v, Ok so -> return (`header_line (v, so, unknown))
  | None, _ -> fail (`header_line_without_version l)
  | _, Error s -> fail (`header_line_wrong_sorting s)
    
let reference_sequence_aggregator () =
  let refs = ref [] in
  let get_line line = 
    let sn = ref None in
    let ln = ref (Error "") in
    let asi = ref None in
    let m5 = ref None in
    let sp = ref None in
    let ur = ref None in
    let ios s = try Ok (Int.of_string s) with e -> Error s in
    let set r v = r := Some v; None in
    let ref_unknown =
      List.filter_map line (function
      | "SN", name -> set sn name
      | "LN", l -> ln := ios l; None
      | "AS", a -> set asi a
      | "M5", m -> set m5 m
      | "SP", s -> set sp s
      | "UR", u -> set ur u
      | other -> Some other) in
    match !sn, !ln with
    | Some sn, Ok ln ->
      refs := {
        ref_name = sn;
        ref_length = ln;
        ref_assembly_identifier = !asi;
        ref_checksum = !m5;
        ref_species = !sp;
        ref_uri = !ur;
        ref_unknown } :: !refs;
      return ()
    | None, _ -> fail (`missing_ref_sequence_name line)
    | _, Error "" -> fail (`missing_ref_sequence_length line)
    | _, Error s -> fail (`wrong_ref_sequence_length line)
  in
  let finish () =
    let deduped =
      List.dedup ~compare:(fun a b -> compare a.ref_name b.ref_name) !refs in
    if List.length deduped <> List.length !refs then
      fail (`duplicate_in_reference_sequence_dictionary
               (Array.of_list_rev !refs))
    else 
      return (Array.of_list_rev !refs)
  in
  (get_line, finish)

let parse_cigar_text text =
  if text = "*" then return [| |]
  else begin
    let ch = Scanf.Scanning.from_string text in
    let rec loop acc =
      if Scanf.Scanning.end_of_input ch then return acc
      else begin
        try
          let v = Scanf.bscanf ch "%d" ident in
          let c = Scanf.bscanf ch "%c" ident in
          match c with
          | 'M' -> (loop (`M  v :: acc))
          | 'I' -> (loop (`I  v :: acc))
          | 'D' -> (loop (`D  v :: acc))
          | 'N' -> (loop (`N  v :: acc))
          | 'S' -> (loop (`S  v :: acc))
          | 'H' -> (loop (`H  v :: acc))
          | 'P' -> (loop (`P  v :: acc))
          | '=' -> (loop (`Eq v :: acc))
          | 'X' -> (loop (`X  v :: acc))
          | other -> failwith ""
        with
          e -> fail (`wrong_cigar_text text)
      end
    in
    loop [] >>| Array.of_list_rev
  end
    


let parse_optional_content raw =
  let error e = fail (`wrong_optional (raw, e)) in
  let char tag typ raw = 
    if String.length raw <> 1 then error (`not_a_char raw)
    else return (tag, typ, `char raw.[0]) in
  let int tag typ raw =
    try let i = Int.of_string raw in return (tag, typ, `int i)
    with e -> error (`not_an_int raw) in
  let float tag typ raw =
    try let i = Float.of_string raw in return (tag, typ, `float i)
    with e -> error (`not_a_float raw) in
  let parse_cCsSiIf tag typ raw =
    begin match typ with
    | 'i' | 's' | 'I' | 'S' -> int tag typ raw
    | 'A' | 'c' | 'C' -> char tag typ raw
    | 'f' -> float tag typ raw
    | _ -> error (`unknown_type typ)
    end in
  Result_list.while_ok raw (fun _ (tag, typ, raw_v) ->
    begin match typ with
    | 'Z' -> return (tag, typ, `string raw_v)
    | 'H' -> return (tag, typ, `string raw_v)
    | 'B' -> 
      begin match String.split ~on:',' raw_v with
      | [] ->  error (`wrong_array (`wrong_type raw_v))
      | f :: _ when String.length f <> 1 ->
        error (`wrong_array (`wrong_type raw_v))
      | typs :: l ->
        let array = Array.create List.(length l) (`string "no") in
        let rec loop i = function
          | [] -> return array
          | h :: t ->
            begin match parse_cCsSiIf "" typs.[0] h with
            | Ok (_, _, v) -> array.(i) <- v; loop (i + 1) t
            | Error (`wrong_optional (_, e)) -> error (`wrong_array e)
            end
        in
        loop 0 l
        >>= fun a ->
        return (tag, typ, `array (typs.[0], a))
      end
    | c -> parse_cCsSiIf tag typ raw_v
    end)
  
let expand_alignment raw ref_dict =
  let {qname; flag; rname; pos;
       mapq; cigar; rnext; pnext;
       tlen; seq; qual; optional; } = raw in
  let check c e = if c then return () else fail e in
  check (0 <= flag && flag <= 65535) (`wrong_flag raw) >>= fun () ->
  check (1 <= String.length qname && String.length qname <= 255)
    (`wrong_qname raw)
  >>= fun () ->
  let tryfind rname =
    let open Option in
    ref_dict
    >>= fun ri ->
    Array.find ri ~f:(fun r -> r.ref_name = rname) in
  let reference_sequence =
    match rname with
    | "*" -> `none
    | s ->
      begin match tryfind rname with
      | Some r -> `reference_sequence r
      | None -> `name s
      end
  in
  check (0 <= pos && pos <= 536870911) (`wrong_pos raw) >>= fun () ->
  check (0 <= mapq && mapq <= 255) (`wrong_mapq raw) >>= fun () ->
  parse_cigar_text cigar >>= fun cigar_operations ->
  begin match rnext with
  | "*" -> return `none
  | "=" -> return `qname
  | s -> 
    begin match tryfind s with
    | None -> return (`name s)
    | Some r -> return (`reference_sequence r)
    end
  end
  >>= fun next_ref_name ->
  check (0 <= pnext && pnext <= 536870911) (`wrong_pnext raw) >>= fun () ->
  check (-536870911 <= tlen && tlen <= 536870911) (`wrong_tlen raw)
  >>= fun () ->
  let sequence =
    match seq with
    | "*" -> `none
    | "=" -> `reference
    | s -> `string s in
  (if qual = "*" then return [| |] else begin
    try
      let quality =
        Array.create (String.length qual) Biocaml_phred_score.(of_int_exn 0) in
      for i = 0 to String.length qual - 1 do
        quality.(i) <- Biocaml_phred_score.of_ascii_exn qual.[i];
      done;
      Ok quality
    with
    | e -> fail (`wrong_phred_scores raw)
  end)
  >>= fun quality ->
  parse_optional_content optional
  >>= fun optional_content ->
  
  return {
    query_template_name = qname;
    flags = flag;
    reference_sequence;
    position = if pos = 0 then None else Some pos;
    mapping_quality =if mapq = 255 then None else Some mapq;
    cigar_operations;
    next_ref_name;
    next_ref_position = if pnext = 0 then None else Some pnext;
    template_length  = if tlen = 0 then None else Some tlen;
    sequence;
    quality;
    optional_content;
  }

let item_parser () : (raw_item, item, item_parsing_error) Biocaml_transform.t =
  let name = "sam_item_parser" in
  let raw_queue = Dequeue.create ~dummy:(`comment "no") () in
  let raw_items_count = ref 0 in
  let refseq_line, refseq_end = reference_sequence_aggregator () in
  let of_result r = match r with Ok o -> `output o | Error e -> `error e in
  let header_finished = ref false in
  let ref_dictionary = ref None in
  let rec next stopped =
    if Dequeue.is_empty raw_queue
    then (if stopped then `end_of_stream else `not_ready)
    else begin
      incr raw_items_count;
      begin match Dequeue.take_front_exn raw_queue with
      | `comment c when !header_finished ->
        `error (`comment_after_end_of_header (!raw_items_count, c))
      | `header c when !header_finished ->
        `error (`header_after_end_of_header (!raw_items_count, c))
      | `comment c ->  `output (`comment c)
      | `header ("HD", l) ->
        if !raw_items_count <> 1
        then `error (`header_line_not_first !raw_items_count)
        else of_result (expand_header_line l)
      | `header ("SQ", l) ->
        begin match refseq_line l with
        | Error e -> `error e
        | Ok () ->  next stopped
        end
      | `header _ as other -> `output other
      | `alignment a ->
        if !header_finished then (
          expand_alignment a !ref_dictionary
          >>| (fun a -> `alignment a)
          |! of_result
        ) else begin
          header_finished := true;
          Dequeue.push_front raw_queue (`alignment a);
          begin match refseq_end () with
          | Ok rd ->
            ref_dictionary := Some rd;
            `output (`reference_sequence_dictionary rd)
          | Error e -> `error e
          end
        end
      end
    end
  in
  Biocaml_transform.make_stoppable ~name ~feed:(Dequeue.push_back raw_queue) ()
    ~next
  
let downgrade_alignment al =
  let qname = al.query_template_name in
  let flag = al.flags in
  let rname =
    match al.reference_sequence with
    | `none -> "*"
    | `name s -> s
    | `reference_sequence rs -> rs.ref_name in
  let pos = Option.value ~default:0 al.position in
  let mapq = Option.value ~default:255 al.mapping_quality in
  let cigar = 
    match al.cigar_operations with
    | [| |] -> "*"
    | some ->
      Array.map some ~f:(function
      | `M  v -> sprintf "%d%c" v 'M'
      | `I  v -> sprintf "%d%c" v 'I'
      | `D  v -> sprintf "%d%c" v 'D'
      | `N  v -> sprintf "%d%c" v 'N'
      | `S  v -> sprintf "%d%c" v 'S'
      | `H  v -> sprintf "%d%c" v 'H'
      | `P  v -> sprintf "%d%c" v 'P'
      | `Eq v -> sprintf "%d%c" v '='
      | `X  v -> sprintf "%d%c" v 'X')
      |! String.concat_array ~sep:"" in
  let rnext = 
    match al.next_ref_name with
    | `qname -> "=" | `none -> "*" | `name s -> s
    | `reference_sequence rs -> rs.ref_name in
  let pnext = Option.value ~default:0 al.next_ref_position in
  let tlen =  Option.value ~default:0 al.template_length in
  let seq =
    match al.sequence with | `string s -> s | `reference -> "=" | `none -> "*" in
  begin
    try
      Array.map al.quality ~f:(fun q ->
        Biocaml_phred_score.to_ascii q |! Option.value_exn |! Char.to_string)
      |! String.concat_array ~sep:""
      |! return
    with
      e -> fail (`wrong_phred_scores al)
  end
  >>= fun qual ->
  let optional =
    let rec optv = function
      | `array (t, a) ->
        sprintf "B%c%s" t String.(Array.map a ~f:optv |! concat_array ~sep:"" )
      | `char c -> Char.to_string c
      | `float f -> Float.to_string f
      | `int i -> Int.to_string i
      | `string s -> s in
    List.map al.optional_content (fun (tag, typ, v) -> (tag, typ, optv v))
  in
  return (`alignment {qname; flag; rname; pos;
                          mapq; cigar; rnext; pnext;
                          tlen; seq; qual; optional; })
    

let downgrader () =
  let name = "sam_item_downgrader" in
  let raw_queue = Dequeue.create ~dummy:(`comment "no") () in
  let raw_items_count = ref 0 in
  let of_result r = match r with Ok o -> `output o | Error e -> `error e in
  let reference_sequence_dictionary = ref [| |] in
  let reference_sequence_dictionary_to_output = ref 0 in
  let rec next stopped =
    begin match !reference_sequence_dictionary_to_output with
    | 0 ->
      begin match Dequeue.is_empty raw_queue with
      | true when stopped -> `end_of_stream
      | true (* when not stopped *) -> `not_ready
      | false ->
        incr raw_items_count;
        begin match Dequeue.take_front_exn raw_queue with
        | `comment c -> `output (`comment c)
        | `header_line (version, sorting, rest) ->
          `output (`header ("HD",
                            ("VN", version)
                            :: ("SO",
                                match sorting with
                                | `unknown -> "unknown"
                                | `unsorted -> "unsorted"
                                | `queryname -> "queryname"
                                | `coordinate -> "coordinate")
                            :: rest))
        | `reference_sequence_dictionary rsd ->
          reference_sequence_dictionary := rsd;
          reference_sequence_dictionary_to_output := Array.length rsd;
          next stopped
        | `header ("SQ", _) ->
          (* we simply skip this one *)
          next stopped
        | `header h -> `output (`header h)
        | `alignment al ->
          downgrade_alignment al |! of_result
        end
      end
    | n ->
      let o =
        !reference_sequence_dictionary.(
          Array.length !reference_sequence_dictionary - n) in
      reference_sequence_dictionary_to_output := n - 1;
      `output (`header ("SQ", reference_sequence_to_header o))
    end
  in
  Biocaml_transform.make_stoppable ~name ~feed:(Dequeue.push_back raw_queue) ()
    ~next

    
let alignment_to_string x =
  sprintf "%s\t%d\t%s\t%d\t%d\t%s\t%s\t%d\t%d\t%s\t%s\t%s\n"
    x.qname x.flag x.rname x.pos x.mapq x.cigar x.rnext x.pnext x.tlen x.seq x.qual
    (List.map x.optional (fun (a,b,c) -> sprintf "%s:%c:%s" a b c) |!
        String.concat ~sep:"\t")

let raw_printer () =
  let module PQ = Biocaml_transform.Printer_queue in
  let printer =
    PQ.make ~to_string:(function
    | `comment c -> sprintf "@CO\t%s\n" c
    | `header (t, l) -> sprintf "@%s\t%s\n" t
      (List.map l (fun (a,b) -> sprintf "%s:%s" a b) |! String.concat ~sep:"\t")
    | `alignment a -> alignment_to_string a
    ) () in
  Biocaml_transform.make_stoppable ~name:"sam_printer" ()
    ~feed:(fun r -> PQ.feed printer r)
    ~next:(fun stopped ->
      match (PQ.flush printer) with
      | "" -> if stopped then `end_of_stream else `not_ready
      | s -> `output s)

    

