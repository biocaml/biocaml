open Core_kernel
module Result = Biocaml_result
open Result.Monad_infix

let ( >>?~ )
    (x : 'a option Or_error.t)
    (f : 'a -> 'b Or_error.t)
    : 'b option Or_error.t
    =
  let open Result.Monad_infix in
  x >>= function
  | None -> Ok None
  | Some x -> f x >>| Option.some

(******************************************************************************)
(* Header Types                                                               *)
(******************************************************************************)
type header_item_tag = [
| `HD | `SQ | `RG | `PG | `CO
| `Other of string
] [@@deriving sexp]

type tag_value = string * string
[@@deriving sexp]

type sort_order = [ `Unknown | `Unsorted | `Query_name | `Coordinate ]
[@@deriving sexp]

type group_order = [ `None | `Query | `Reference ]
[@@deriving sexp]

type header_line = {
  version : string;
  sort_order : sort_order option;
  group_order: group_order option;
} [@@deriving sexp]

type ref_seq = {
  name : string;
  length : int;
  assembly : string option;
  md5 : string option;
  species : string option;
  uri : string option;
} [@@deriving sexp]

type platform = [
| `Capillary | `LS454 | `Illumina | `Solid
| `Helicos | `Ion_Torrent | `Pac_Bio
] [@@deriving sexp]

type read_group = {
  id : string;
  seq_center : string option;
  description : string option;
  run_date : [`Date of string | `Time of string] option;
  flow_order : string option;
  key_seq : string option;
  library : string option;
  program : string option;
  predicted_median_insert_size : int option;
  platform : platform option;
  platform_unit : string option;
  sample : string option;
} [@@deriving sexp]

type program = {
  id : string;
  name : string option;
  command_line : string option;
  previous_id : string option;
  description : string option;
  version : string option;
} [@@deriving sexp]

type header_item = [
| `HD of header_line
| `SQ of ref_seq
| `RG of read_group
| `PG of program
| `CO of string
| `Other of string * tag_value list
] [@@deriving sexp]

type header = {
  version : string option;
  sort_order : sort_order option;
  group_order : group_order option;
  ref_seqs : ref_seq list;
  read_groups : read_group list;
  programs : program list;
  comments : string list;
  others : (string * tag_value list) list;
}

let empty_header = {
  version = None;
  sort_order = None;
  group_order = None;
  ref_seqs = [];
  read_groups = [];
  programs = [];
  comments = [];
  others = [];
}


(******************************************************************************)
(* Alignment Types                                                            *)
(******************************************************************************)
module Flags = struct
  type t = int
  [@@deriving sexp]

  let of_int x =
    if (0 <= x) && (x <= 65535) then
      Ok x
    else
      error "flag out of range" x sexp_of_int

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
  let supplementary_alignment          = flag_is_set 0x800
end

type cigar_op = [
  | `Alignment_match of int
  | `Insertion of int
  | `Deletion of int
  | `Skipped of int
  | `Soft_clipping of int
  | `Hard_clipping of int
  | `Padding of int
  | `Seq_match of int
  | `Seq_mismatch of int
] [@@deriving sexp]

type optional_field_value = [
| `A of char
| `i of Int64.t
| `f of float
| `Z of string
| `H of string
| `B of char * string list
] [@@deriving sexp]

type optional_field = {
  tag : string;
  value : optional_field_value
} [@@deriving sexp]

type rnext = [`Value of string | `Equal_to_RNAME]
[@@deriving sexp]

type alignment = {
  qname : string option;
  flags : Flags.t;
  rname : string option;
  pos : int option;
  mapq : int option;
  cigar : cigar_op list;
  rnext : rnext option;
  pnext : int option;
  tlen : int option;
  seq: string option;
  qual: Phred_score.t list;
  optional_fields : optional_field list;
} [@@deriving sexp]



(******************************************************************************)
(* Header Parsers and Constructors                                            *)
(******************************************************************************)
let parse_header_version s =
  let err =
    error "invalid version" (`HD, s)
    [%sexp_of: header_item_tag * string ]
  in
  match String.lsplit2 ~on:'.' s with
  | None -> err
  | Some (a,b) ->
    if (String.for_all a ~f:Char.is_digit)
      && (String.for_all b ~f:Char.is_digit)
    then
      Ok s
    else
      err

let header_line ~version ?sort_order ?group_order () =
  parse_header_version version >>| fun version ->
  {version; sort_order; group_order}

let ref_seq
    ~name ~length
    ?assembly ?md5 ?species ?uri
    ()
    =
  let is_name_first_char_ok = function
    | '!' .. ')' | '+' .. '<' | '>' .. '~' -> true
    | _ -> false
  in

  let is_name_other_char_ok = function '!' .. '~' -> true | _ -> false in

  (if (1 <= length) && (length <= 2147483647) then
      Ok length
   else
      error "invalid reference sequence length" length sexp_of_int
  ) >>= fun length ->

  (if (String.length name > 0)
      && (String.foldi name ~init:true ~f:(fun i accum c ->
        accum && (
          if i = 0 then is_name_first_char_ok c
          else is_name_other_char_ok c
        ) ) )
   then
      Ok name
   else
      error "invalid ref seq name" name sexp_of_string
  ) >>= fun name ->

  Ok {name; length; assembly; md5; species; uri}


let read_group
    ~id ?seq_center ?description ?run_date ?flow_order
    ?key_seq ?library ?program ?predicted_median_insert_size
    ?platform ?platform_unit ?sample
    ()
    =
  (match run_date with
  | None -> Ok None
  | Some run_date ->
    try Ok (Some (`Date run_date))
    with _ ->
      try Ok (Some (`Time run_date))
      with _ ->
        error "invalid run date/time" run_date sexp_of_string
  ) >>= fun run_date ->

  (match flow_order with
  | None -> Ok None
  | Some "" -> Or_error.error_string "invalid empty flow order"
  | Some "*" -> Ok flow_order
  | Some x ->
    if String.for_all x ~f:(function
    | 'A' | 'C' | 'M' | 'G' | 'R' | 'S' | 'V' | 'T' | 'W'| 'Y' | 'H'
    | 'K' | 'D' | 'B' | 'N' -> true
    | _ -> false
    )
    then
      Ok flow_order
    else
      error "invalid flow order" x sexp_of_string
  ) >>| fun flow_order ->

  {
    id; seq_center; description; run_date; flow_order; key_seq;
    library; program; predicted_median_insert_size;
    platform; platform_unit; sample;
  }


let header
    ?version ?sort_order ?group_order ?(ref_seqs=[]) ?(read_groups=[])
    ?(programs=[]) ?(comments=[]) ?(others=[])
    ()
    =
  [
    (
      match version with
      | None -> None
      | Some x -> match parse_header_version x with
        | Error e -> Some e
        | Ok _ -> None
    );
    (
      if Option.is_some sort_order && (version = None) then
        Some (Error.create
                "sort order cannot be defined without version"
                (sort_order, version)
                [%sexp_of: sort_order option * string option ]
        )
      else
        None
    );
    (
      List.map ref_seqs ~f:(fun (x:ref_seq) -> x.name)
      |> List.find_a_dup
      |> Option.map ~f:(fun name ->
         Error.create "duplicate ref seq name" name sexp_of_string
      )
    );
  ]
  |> List.filter_map ~f:Fn.id
  |> function
     | [] -> Ok {
       version; sort_order; group_order; ref_seqs; read_groups;
       programs; comments; others;
     }
     | errs -> Error (Error.of_list errs)


let parse_header_item_tag s =
  let is_letter = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false in
  match String.chop_prefix s ~prefix:"@" with
  | None -> error "header item tag must begin with @" s sexp_of_string
  | Some "HD" -> Ok `HD
  | Some "SQ" -> Ok `SQ
  | Some "RG" -> Ok `RG
  | Some "PG" -> Ok `PG
  | Some "CO" -> Ok `CO
  | Some x ->
    if (String.length x = 2)
      && (String.for_all x ~f:is_letter)
    then
      Ok (`Other x)
    else
      error "invalid header item tag" s sexp_of_string

let parse_tag_value s =
  let parse_tag s =
    if (String.length s = 2)
      && (match s.[0] with 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false)
      && (match s.[1] with
          | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
          | _ -> false
      )
    then
      Ok s
    else
      error "invalid tag" s sexp_of_string
  in
  let parse_value tag s =
    if (s <> "")
      && (String.for_all s ~f:(function ' ' .. '~' -> true | _ -> false))
    then
      Ok s
    else
      error "tag has invalid value" (tag,s)
      [%sexp_of: string * string ]
  in
  match String.lsplit2 s ~on:':' with
  | None ->
    error "tag-value not colon separated" s sexp_of_string
  | Some (tag,value) ->
    parse_tag tag >>= fun tag ->
    parse_value tag value >>= fun value ->
    Ok (tag, value)

(** Find all occurrences of [x'] in the association list [l]. *)
let find_all l x' =
  let rec loop accum = function
    | [] -> accum
    | (x,y)::l ->
      let accum = if x = x' then y::accum else accum in
      loop accum l
  in
  List.rev (loop [] l)

(** Find exactly 1 occurrence [x] in association list [l]. Return
    error if [x] is not defined exactly once. *)
let find1 header_item_tag l x =
  match find_all l x with
  | [] ->
    error "required tag not found" (header_item_tag, x)
    [%sexp_of: header_item_tag * string ]
  | y::[] -> Ok y
  | ys ->
    error "tag found multiple times" (header_item_tag, x, ys)
    [%sexp_of: header_item_tag * string * string list ]

(** Find 0 or 1 occurrence [x] in association list [l]. Return
    error if [x] is defined more than once. *)
let find01 header_item_tag l x =
  match find_all l x with
  | [] -> Ok None
  | y::[] -> Ok (Some y)
  | ys ->
    error "tag found multiple times" (header_item_tag, x, ys)
    [%sexp_of: header_item_tag * string * string list ]

(** Assert that [tvl] contains at most the given [tags]. *)
let assert_tags header_item_tag tvl tags =
  let expected_tags = String.Set.of_list tags in
  let got_tags = List.map tvl ~f:fst |> String.Set.of_list in
  let unexpected_tags = Set.diff got_tags expected_tags in
  if Set.length unexpected_tags = 0 then
    Ok ()
  else
    error
      "unexpected tag for given header item type"
      (header_item_tag, unexpected_tags)
      [%sexp_of: header_item_tag * String.Set.t ]

let parse_sort_order = function
  | "unknown" -> Ok `Unknown
  | "unsorted" -> Ok `Unsorted
  | "queryname" -> Ok `Query_name
  | "coordinate" -> Ok `Coordinate
  | x -> error "invalid sort order" x sexp_of_string

let parse_group_order = function
  | "none" -> Ok `None
  | "query" -> Ok `Query
  | "reference" -> Ok `Reference
  | x -> error "invalid group order" x sexp_of_string

let parse_header_line tvl =
  find1 `HD tvl "VN" >>= fun version ->
  find01 `HD tvl "SO" >>?~
  parse_sort_order >>= fun sort_order ->
  find01 `HD tvl "GO" >>?~
  parse_group_order >>= fun group_order ->
  assert_tags `HD tvl ["VN"; "SO"; "GO"] >>= fun () ->
  header_line ~version ?sort_order ?group_order ()

let parse_ref_seq tvl =
  find1 `SQ tvl "SN" >>= fun name ->
  find1 `SQ tvl "LN" >>= fun length ->
  (try Ok (Int.of_string length)
   with _ ->
     error "invalid ref seq length" length sexp_of_string
  ) >>= fun length ->
  find01 `SQ tvl "AS" >>= fun assembly ->
  find01 `SQ tvl "M5" >>= fun md5 ->
  find01 `SQ tvl "SP" >>= fun species ->
  find01 `SQ tvl "UR" >>= fun uri ->
  assert_tags `SQ tvl ["SN";"LN";"AS";"M5";"SP";"UR"] >>= fun () ->
  ref_seq ~name ~length ?assembly ?md5 ?species ?uri ()

let parse_platform = function
  | "CAPILLARY" -> Ok `Capillary
  | "LS454" -> Ok `LS454
  | "ILLUMINA" -> Ok `Illumina
  | "SOLID" -> Ok `Solid
  | "HELICOS" -> Ok `Helicos
  | "IONTORRENT" -> Ok `Ion_Torrent
  | "PACBIO" -> Ok `Pac_Bio
  | x -> error "unknown platform" x sexp_of_string

let parse_read_group tvl =
  find1 `RG tvl "ID" >>= fun id ->
  find01 `RG tvl "CN" >>= fun seq_center ->
  find01 `RG tvl "DS" >>= fun description ->
  find01 `RG tvl "DT" >>= fun run_date ->
  find01 `RG tvl "FO" >>= fun flow_order ->
  find01 `RG tvl "KS" >>= fun key_seq ->
  find01 `RG tvl "LB" >>= fun library ->
  find01 `RG tvl "PG" >>= fun program ->
  find01 `RG tvl "PI" >>?~ (fun predicted_median_insert_size ->
  try Ok (Int.of_string predicted_median_insert_size)
  with _ ->
    error
      "invalid predicted median insert size"
      predicted_median_insert_size
      sexp_of_string
  ) >>= fun predicted_median_insert_size ->
  find01 `RG tvl "PL" >>?~
  parse_platform >>= fun platform ->
  find01 `RG tvl "PU" >>= fun platform_unit ->
  find01 `RG tvl "SM" >>= fun sample ->
  assert_tags `RG tvl
    ["ID";"CN";"DS";"DT";"FO";"KS";"LB";"PG";"PI";"PL";"PU";"SM"]
  >>= fun () ->
  read_group
    ~id ?seq_center ?description ?run_date ?flow_order ?key_seq
    ?library ?program ?predicted_median_insert_size
    ?platform ?platform_unit ?sample ()

let parse_program tvl =
  find1 `PG tvl "ID" >>= fun id ->
  find01 `PG tvl "PN" >>= fun name ->
  find01 `PG tvl "CL" >>= fun command_line ->
  find01 `PG tvl "PP" >>= fun previous_id ->
  find01 `PG tvl "DS" >>= fun description ->
  find01 `PG tvl "VN" >>= fun version ->
  assert_tags `PG tvl ["ID";"PN";"CL";"PP";"DS";"VN"] >>| fun () ->
  {id; name; command_line; previous_id; description; version}

let parse_header_item line =
  let parse_data tag tvl = match tag with
    | `HD -> parse_header_line tvl >>| fun x -> `HD x
    | `SQ -> parse_ref_seq tvl >>| fun x -> `SQ x
    | `RG -> parse_read_group tvl >>| fun x -> `RG x
    | `PG -> parse_program tvl >>| fun x -> `PG x
    | `Other tag -> Ok (`Other (tag,tvl))
    | `CO -> assert false
  in
  match String.lsplit2 ~on:'\t' (line : Line.t :> string) with
  | None ->
    error "header line contains no tabs" line Line.sexp_of_t
  | Some (tag, data) ->
    parse_header_item_tag tag >>= function
    | `CO -> Ok (`CO data)
    | tag ->
      match String.split ~on:'\t' data with
      | [] -> assert false
      | ""::[] ->
        error "header contains no data" tag sexp_of_header_item_tag
      | tvl ->
        Result.List.map tvl ~f:parse_tag_value >>= fun tvl ->
        parse_data tag tvl


(******************************************************************************)
(* Alignment Parsers and Constructors                                         *)
(******************************************************************************)
let alignment
    ?ref_seqs ?qname ~flags ?rname ?pos ?mapq ?(cigar=[])
    ?rnext ?pnext ?tlen ?seq ?(qual=[])
    ?(optional_fields=[])
    ()
    =
  [
    (match ref_seqs, rname with
    | (None,_) | (_,None) -> None
    | Some ref_seqs, Some rname ->
      if Set.mem ref_seqs rname then
        None
      else
        Some (
          Error.create
            "RNAME not defined in any SQ line"
            rname sexp_of_string
        )
    );

    (match ref_seqs, rnext with
    | (None,_) | (_,None) -> None
    | Some _, Some `Equal_to_RNAME ->
      None (* error will already be detected in RNAME check above *)
    | Some ref_seqs, Some (`Value rnext) ->
      if Set.mem ref_seqs rnext then
        None
      else
        Some (
          Error.create
            "RNEXT not defined in any SQ line"
            rnext sexp_of_string
        )
    );

    (match seq, qual with
    | _, [] -> None
    | None, _ ->
      Some (Error.of_string "QUAL provided without SEQ")
    | Some seq, _ ->
      let s = String.length seq in
      let q = List.length qual in
      if s = q then
        None
      else
        Some (Error.create
                "SEQ and QUAL lengths differ"
                (s, q) [%sexp_of: int * int ]
        )
    );

    (
      List.map optional_fields ~f:(fun x -> x.tag)
      |> List.find_a_dup
      |> Option.map ~f:(fun dup ->
        Error.create "TAG occurs more than once" dup sexp_of_string)
    );
  ]
  |> List.filter_map ~f:Fn.id
  |> function
    | [] -> Ok {
      qname; flags; rname; pos; mapq; cigar;
      rnext; pnext; tlen; seq; qual; optional_fields
    }
    | errs -> Error (Error.of_list errs)


let parse_int_range field lo hi s =
  let out_of_range = sprintf "%s out of range" field in
  let not_an_int = sprintf "%s not an int" field in
  try
    let n = Int.of_string s in
    if (lo <= n) && (n <= hi) then
      Ok n
    else
      error out_of_range (n,lo,hi) [%sexp_of: int * int * int ]
  with _ ->
    error not_an_int s sexp_of_string

(** Parse a string that can either by "*" or some other regexp, with
    "*" denoting [None]. The given regexp [re] should include "*" as
    one of the alternatives. *)
let parse_opt_string field re s =
  if not (Re.execp re s) then
    error (sprintf "invalid %s" field) s sexp_of_string
  else
    match s with
    | "*" -> Ok None
    | _ -> Ok (Some s)

let qname_re =
  let open Re in
  alt [
    char '*';
    repn (alt [rg '!' '?'; rg 'A' '~']) 1 (Some 255);
  ]
  |> compile

let parse_qname s =
  parse_opt_string "QNAME" qname_re s

let parse_flags s =
  try Flags.of_int (Int.of_string s)
  with _ ->
    error "invalid FLAG" s sexp_of_string

let rname_re = Re_perl.compile_pat "^\\*|[!-()+-<>-~][!-~]*$"
let parse_rname s =
  parse_opt_string "RNAME" rname_re s

let parse_pos s =
  parse_int_range "POS" 0 2147483647 s >>| function
  | 0 -> None
  | x -> Some x

let parse_mapq s =
  parse_int_range "MAPQ" 0 255 s >>| function
  | 255 -> None
  | x -> Some x

let positive i =
  let open Or_error in
  if i > 0 then return i else error_string "positive argument expected for cigar operation"

let cigar_op_alignment_match i = Or_error.(positive i >>| fun i -> `Alignment_match i)
let cigar_op_insertion i = Or_error.(positive i >>| fun i -> `Insertion i)
let cigar_op_deletion i = Or_error.(positive i >>| fun i -> `Deletion i)
let cigar_op_skipped i = Or_error.(positive i >>| fun i -> `Skipped i)
let cigar_op_soft_clipping i = Or_error.(positive i >>| fun i -> `Soft_clipping i)
let cigar_op_hard_clipping i = Or_error.(positive i >>| fun i -> `Hard_clipping i)
let cigar_op_padding i = Or_error.(positive i >>| fun i -> `Padding i)
let cigar_op_seq_match i = Or_error.(positive i >>| fun i -> `Seq_match i)
let cigar_op_seq_mismatch i = Or_error.(positive i >>| fun i -> `Seq_mismatch i)

let parse_cigar text =
  match text with
  | "*" -> Ok []
  | "" ->
    error "invalid cigar string" text sexp_of_string
  | _ ->
    let ch = Scanf.Scanning.from_string text in
    let rec loop accum =
      if Scanf.Scanning.end_of_input ch then Ok accum
      else
        try
          let n = Scanf.bscanf ch "%d" ident in
          let c = Scanf.bscanf ch "%c" ident in
          let x =
            match c with
            | 'M' -> cigar_op_alignment_match n
            | 'I' -> cigar_op_insertion n
            | 'D' -> cigar_op_deletion n
            | 'N' -> cigar_op_skipped n
            | 'S' -> cigar_op_soft_clipping n
            | 'H' -> cigar_op_hard_clipping n
            | 'P' -> cigar_op_padding n
            | '=' -> cigar_op_seq_match n
            | 'X' -> cigar_op_seq_mismatch n
            | other -> Or_error.error "invalid cigar operation type" other Char.sexp_of_t
          in
          Or_error.tag x "Sam.parse_cigar: invalid cigar string" >>= fun x ->
          loop (x::accum)
        with
          _ ->
            error "invalid cigar string" text sexp_of_string
    in
    loop [] >>| List.rev

let rnext_re = Re_perl.compile_pat "^\\*|=|[!-()+-<>-~][!-~]*$"
let parse_rnext s =
  if not (Re.execp rnext_re s) then
    error "invalid RNEXT" s sexp_of_string
  else
    match s with
    | "*" -> Ok None
    | "=" -> Ok (Some `Equal_to_RNAME)
    | _ -> Ok (Some (`Value s))

let parse_pnext s =
  parse_int_range "PNEXT" 0 2147483647 s >>| function
  | 0 -> None
  | x -> Some x

let parse_tlen s =
  parse_int_range "TLEN" ~-2147483647 2147483647 s >>| function
  | 0 -> None
  | x -> Some x

let seq_re = Re_perl.compile_pat "^\\*|[A-Za-z=.]+$"
let parse_seq s =
  parse_opt_string "SEQ" seq_re s

let parse_qual s =
  match s with
  | "" -> Or_error.error_string "invalid empty QUAL"
  | "*" -> Ok []
  | _ ->
    String.to_list s
    |> Result.List.map ~f:(Phred_score.of_char ~offset:`Offset33)


let opt_field_tag_re = Re_perl.compile_pat "^[A-Za-z][A-Za-z0-9]$"
let opt_field_Z_re = Re_perl.compile_pat "^[ !-~]+$"
let opt_field_H_re = Re_perl.compile_pat "^[0-9A-F]+$"
let opt_field_int_re = Re_perl.compile_pat "^-?[0-9]+$"
let opt_field_float_re = Re_perl.compile_pat "^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$"

let optional_field_value_err typ value =
  error "invalid value" (typ,value) [%sexp_of: string * string ]

let optional_field_value_A value =
  if List.mem ~equal:Char.equal ['!';'-';'~'] value
  then optional_field_value_err "A" (Char.to_string value)
  else Ok (`A value)

let optional_field_value_i i = `i i

let optional_field_value_f f = `f f

let optional_field_value_Z value =
  if Re.execp opt_field_Z_re value then Ok (`Z value)
  else optional_field_value_err "Z" value

let optional_field_value_H value =
  if Re.execp opt_field_H_re value then Ok (`H value)
  else optional_field_value_err "H" value

let optional_field_value_B elt_type elts =
  let valid_args =
    match elt_type with
    | 'c' | 'C' | 's' | 'S' | 'i' | 'I' ->
      List.for_all elts ~f:(Re.execp opt_field_int_re)
    | 'f' ->
      List.for_all elts ~f:(Re.execp opt_field_float_re)
    | _ -> false
  in
  if valid_args then Ok (`B (elt_type, elts))
  else error "invalid value" ("B", elt_type, elts) [%sexp_of: string * char * string list ]

let optional_field tag value =
  if not (Re.execp opt_field_tag_re tag)
  then error "invalid TAG" tag sexp_of_string
  else Ok {tag; value}

let parse_optional_field_value s =
  match String.lsplit2 s ~on:':' with
  | None ->
    error "missing TYPE in optional field" s sexp_of_string
  | Some (typ,value) ->
    match typ with
    | "A" ->
      if String.length value = 1 then optional_field_value_A value.[0]
      else optional_field_value_err typ value
    | "i" ->
      (try
         if not (Re.execp opt_field_int_re value) then failwith "" ;
         Ok (optional_field_value_i (Int64.of_string value)) (* matching the regular expression is not enough: the number could not fit in 64 bits *)
       with _ -> optional_field_value_err typ value)
    | "f" ->
      (try
         if not (Re.execp opt_field_float_re value) then failwith "" ;
         Ok (optional_field_value_f (Float.of_string value)) (* matching the regular expression is not enough: the number could not fit in native floats *)
       with _ -> optional_field_value_err typ value)
    | "Z" -> optional_field_value_Z value
    | "H" -> optional_field_value_H value
    | "B" -> (
        match String.split ~on:',' value with
        | num_typ :: values ->
          if String.length num_typ = 1 then
            optional_field_value_B num_typ.[0] values
          else
            error "invalid array type" num_typ sexp_of_string
        | _ -> assert false (* [String.split] cannot return an empty list *)
      )
    | _ -> error "invalid type" typ sexp_of_string


let parse_optional_field s =
  match String.lsplit2 s ~on:':' with
  | None ->
    error "missing TAG in optional field" s sexp_of_string
  | Some (tag,s) ->
    parse_optional_field_value s >>= fun value ->
    optional_field tag value


let parse_alignment ?ref_seqs line =
  match String.split ~on:'\t' (line : Line.t :> string) with
  | qname::flags::rname::pos::mapq::cigar::rnext
    ::pnext::tlen::seq::qual::optional_fields
    -> (
      parse_qname qname >>= fun qname ->
      parse_flags flags >>= fun flags ->
      parse_rname rname >>= fun rname ->
      parse_pos pos >>= fun pos ->
      parse_mapq mapq >>= fun mapq ->
      parse_cigar cigar >>= fun cigar ->
      parse_rnext rnext >>= fun rnext ->
      parse_pnext pnext >>= fun pnext ->
      parse_tlen tlen >>= fun tlen ->
      parse_seq seq >>= fun seq ->
      parse_qual qual >>= fun qual ->
      Result.List.map optional_fields ~f:parse_optional_field
      >>= fun optional_fields ->
      alignment
        ?ref_seqs ?qname ~flags ?rname ?pos ?mapq ~cigar
        ?rnext ?pnext ?tlen ?seq ~qual ~optional_fields
        ()
    )
  | _ ->
    Or_error.error_string "alignment line contains < 12 fields"



(******************************************************************************)
(* Header Printers                                                            *)
(******************************************************************************)
let print_header_item_tag = function
  | `HD -> "@HD"
  | `SQ -> "@SQ"
  | `RG -> "@RG"
  | `PG -> "@PG"
  | `CO -> "@CO"
  | `Other x -> sprintf "@%s" x

let print_tag_value (tag,value) = sprintf "%s:%s" tag value
let print_tag_value' = sprintf "%s:%s"

let print_header_version x = print_tag_value' "VN" x

let print_sort_order x =
  print_tag_value' "SO"
    (match x with
    | `Unknown -> "unknown"
    | `Unsorted -> "unsorted"
    | `Query_name -> "queryname"
    | `Coordinate -> "coordinate"
    )

let print_group_order x =
  print_tag_value' "GO"
    (match x with
    | `None -> "none"
    | `Query -> "query"
    | `Reference -> "reference"
    )

let print_header_line ({version; sort_order; group_order} : header_line) =
  sprintf "@HD\tVN:%s%s%s"
    version
    (match sort_order with
    | None -> ""
    | Some x -> sprintf "\t%s" (print_sort_order x)
    )
    (match group_order with
    | None -> ""
    | Some x -> sprintf "\t%s" (print_group_order x)
    )

let print_ref_seq (x:ref_seq) =
  sprintf "@SQ\tSN:%s\tLN:%d%s%s%s%s"
    x.name
    x.length
    (match x.assembly with None -> "" | Some x -> sprintf "\tAS:%s" x)
    (match x.md5 with None -> "" | Some x -> sprintf "\tM5:%s" x)
    (match x.species with None -> "" | Some x -> sprintf "\tSP:%s" x)
    (match x.uri with None -> "" | Some x -> sprintf "\tUR:%s" x)

let print_platform = function
  | `Capillary -> "CAPILLARY"
  | `LS454 -> "LS454"
  | `Illumina -> "ILLUMINA"
  | `Solid -> "SOLID"
  | `Helicos -> "HELICOS"
  | `Ion_Torrent -> "IONTORRENT"
  | `Pac_Bio -> "PACBIO"

let print_read_group (x:read_group) =
  let s tag value = match value with
    |  None -> ""
    | Some x -> sprintf "\t%s:%s" tag x
  in
  sprintf "@RG\tID:%s%s%s%s%s%s%s%s%s%s%s%s"
    x.id
    (s "CN" x.seq_center)
    (s "DS" x.description)
    (s "DT" (Option.map x.run_date
               ~f:(function
                   | `Date x
                   | `Time x -> x) )
    )
    (s "FO" x.flow_order)
    (s "KS" x.key_seq)
    (s "LB" x.library)
    (s "PG" x.program)
    (s "PI" (Option.map x.predicted_median_insert_size ~f:Int.to_string))
    (s "PL" (Option.map x.platform ~f:print_platform))
    (s "PU" x.platform_unit)
    (s "SM" x.sample)

let print_program (x:program) =
  let s tag value = match value with
    |  None -> ""
    | Some x -> sprintf "\t%s:%s" tag x
  in
  sprintf "@PG\tID:%s%s%s%s%s%s"
    x.id
    (s "PN" x.name)
    (s "CL" x.command_line)
    (s "PP" x.previous_id)
    (s "DS" x.description)
    (s "VN" x.version)

let print_other ((tag,l) : string * tag_value list) =
  sprintf "@%s%s"
    tag
    (
      List.map l ~f:(fun (x,y) -> sprintf "\t%s:%s" x y)
      |> String.concat ~sep:""
    )


(******************************************************************************)
(* Alignment Printers                                                         *)
(******************************************************************************)
let print_qname = function Some x -> x | None -> "*"
let print_flags = Int.to_string
let print_rname = function Some x -> x | None -> "*"
let print_pos = function Some x -> Int.to_string x | None -> "0"
let print_mapq = function Some x -> Int.to_string x | None -> "255"

let print_cigar_op = function
  | `Alignment_match x -> sprintf "%dM" x
  | `Insertion x -> sprintf "%dI" x
  | `Deletion x -> sprintf "%dD" x
  | `Skipped x -> sprintf "%dN" x
  | `Soft_clipping x -> sprintf "%dS" x
  | `Hard_clipping x -> sprintf "%dH" x
  | `Padding x -> sprintf "%dP" x
  | `Seq_match x -> sprintf "%d=" x
  | `Seq_mismatch x -> sprintf "%dX" x

let print_cigar = function
  | [] -> "*"
  | cigar_ops ->
    List.map cigar_ops ~f:print_cigar_op
    |> String.concat ~sep:""

let print_rnext = function
  | None -> "*"
  | Some `Equal_to_RNAME -> "="
  | Some (`Value x) -> x

let print_pnext = function Some x -> Int.to_string x | None -> "0"
let print_tlen = function Some x -> Int.to_string x | None -> "0"
let print_seq = function Some x -> x | None -> "*"

let print_qual = function
  | [] -> "*"
  | quals ->
    List.map quals ~f:(fun x ->
      ok_exn (Phred_score.to_char ~offset:`Offset33 x)
    )
    |> String.of_char_list

let print_optional_field (x:optional_field) =
  let typ,value = match x.value with
    | `A x -> 'A', Char.to_string x
    | `i x -> 'i', Int64.to_string x
    | `f x -> 'f', Float.to_string x
    | `Z x -> 'Z', x
    | `H x -> 'H', x
    | `B (c,l) -> 'B', (String.concat ~sep:"," ((String.of_char c)::l))
  in
  sprintf "%s:%c:%s" x.tag typ value

let print_alignment a =
  sprintf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"
    (print_qname a.qname)
    (print_flags a.flags)
    (print_rname a.rname)
    (print_pos a.pos)
    (print_mapq a.mapq)
    (print_cigar a.cigar)
    (print_rnext a.rnext)
    (print_pnext a.pnext)
    (print_tlen a.tlen)
    (print_seq a.seq)
    (print_qual a.qual)
    (
      List.map a.optional_fields ~f:print_optional_field
      |> String.concat ~sep:"\t"
    )


(******************************************************************************)
(* Input/Output                                                               *)
(******************************************************************************)
module MakeIO(Future : Future.S) = struct
  open Future
  module Lines = struct
    include Lines
    include MakeIO(Future)
  end

  let read_header lines =
    let rec loop hdr : header Or_error.t Deferred.t =
      Pipe.peek_deferred lines >>= (function
      | `Eof -> return (Ok hdr)
      | `Ok line ->
        if String.length (line : Line.t :> string) = 0 then
          return (Or_error.error_string "invalid empty line")
        else if (line : Line.t :> string).[0] <> '@' then
          return (Ok hdr)
        else (
          Pipe.junk lines >>= fun () ->
          parse_header_item line |> function
          | Error _ as e -> return e
          | Ok (`HD ({version; sort_order; group_order} : header_line)) -> (
            match hdr.version with
            | Some _ ->
              return (Or_error.error_string "multiple @HD lines not allowed")
            | None ->
              loop {hdr with version = Some version; sort_order; group_order}
          )
          | Ok (`SQ x) -> loop {hdr with ref_seqs = x::hdr.ref_seqs}
          | Ok (`RG x) -> loop {hdr with read_groups = x::hdr.read_groups}
          | Ok (`PG x) -> loop {hdr with programs = x::hdr.programs}
          | Ok (`CO x) -> loop {hdr with comments = x::hdr.comments}
          | Ok (`Other x) -> loop {hdr with others = x::hdr.others}
        )
      )
    in
    loop empty_header >>| function
    | Error _ as e -> e
    | Ok ({version; sort_order; group_order; _} as x) ->
      let ref_seqs = List.rev x.ref_seqs in
      let read_groups = List.rev x.read_groups in
      let programs = List.rev x.programs in
      let comments = List.rev x.comments in
      let others = List.rev x.others in
      header
        ?version ?sort_order ?group_order ~ref_seqs ~read_groups
        ~programs ~comments ~others ()


  let read ?(start=Pos.(incr_line unknown)) r =
    let pos = ref start in
    let lines =
      Pipe.map (Lines.read r) ~f:(fun line ->
        pos := Pos.incr_line !pos;
        line
      )
    in
    read_header lines >>| function
    | Error _ as e -> Or_error.tag_arg e "position" !pos Pos.sexp_of_t
    | Ok hdr ->
      let alignments = Pipe.map lines ~f:(fun line ->
        Or_error.tag_arg
          (parse_alignment line)
          "position" !pos Pos.sexp_of_t
      )
      in
      Ok (hdr, alignments)


  let write_header w (h:header) =
    let open Writer in
    (match h.version with
    | None -> Deferred.unit
    | Some version ->
      write_line w (print_header_line {version; sort_order=h.sort_order; group_order=h.group_order})
    ) >>= fun () ->
    Deferred.List.iter h.ref_seqs ~f:(fun x ->
      write_line w (print_ref_seq x)
    ) >>= fun () ->
    Deferred.List.iter h.read_groups ~f:(fun x ->
      write_line w (print_read_group x)
    ) >>= fun () ->
    Deferred.List.iter h.programs ~f:(fun x ->
      write_line w (print_program x)
    ) >>= fun () ->
    Deferred.List.iter h.comments ~f:(fun x ->
      write w "@CO\t" >>= fun () ->
      write_line w x
    ) >>= fun () ->
    Deferred.List.iter h.others ~f:(fun x ->
      write_line w (print_other x)
    )

  let write w ?(header=empty_header) alignments =
    write_header w header >>= fun () ->
    Pipe.iter alignments ~f:(fun a ->
      Writer.write_line w (print_alignment a)
    )

  let write_file ?perm ?append file ?header alignments =
    Writer.with_file ?perm ?append file ~f:(fun w ->
      write w ?header alignments
    )

end
include MakeIO(Future_unix)

let parse_header text =
  read_header (Lines.of_string text)
