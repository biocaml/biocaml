open! Import
open Result.Monad_infix

let ( >>?~ ) (x : 'a option Or_error.t) (f : 'a -> 'b Or_error.t) : 'b option Or_error.t =
  let open Result.Monad_infix in
  x
  >>= function
  | None -> Ok None
  | Some x -> f x >>| Option.some
;;

module Header = struct
  module Type = struct
    type t =
      [ `HD
      | `SQ
      | `RG
      | `PG
      | `CO
      | `Other of string
      ]
    [@@deriving sexp]

    let string_of_t = function
      | `HD -> "@HD"
      | `SQ -> "@SQ"
      | `RG -> "@RG"
      | `PG -> "@PG"
      | `CO -> "@CO"
      | `Other x -> sprintf "@%s" x
    ;;

    let t_of_string s =
      let is_letter = function
        | 'A' .. 'Z' | 'a' .. 'z' -> true
        | _ -> false
      in
      match String.chop_prefix s ~prefix:"@" with
      | None -> Error (Error.create "header item tag must begin with @" s sexp_of_string)
      | Some "HD" -> Ok `HD
      | Some "SQ" -> Ok `SQ
      | Some "RG" -> Ok `RG
      | Some "PG" -> Ok `PG
      | Some "CO" -> Ok `CO
      | Some x ->
        if String.length x = 2 && String.for_all x ~f:is_letter
        then Ok (`Other x)
        else Error (Error.create "invalid header item tag" s sexp_of_string)
    ;;
  end

  module Tag_value = struct
    type t = string * string [@@deriving sexp]

    let t_of_string s =
      let parse_tag s =
        if
          String.length s = 2
          && (match s.[0] with
              | 'A' .. 'Z' | 'a' .. 'z' -> true
              | _ -> false)
          &&
          match s.[1] with
          | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
          | _ -> false
        then Ok s
        else Error (Error.create "invalid tag" s sexp_of_string)
      in
      let parse_value tag s =
        if
          String.(s <> "")
          && String.for_all s ~f:(function
            | ' ' .. '~' -> true
            | _ -> false)
        then Ok s
        else
          Error
            (Error.create "tag has invalid value" (tag, s) [%sexp_of: string * string])
      in
      match String.lsplit2 s ~on:':' with
      | None -> Error (Error.create "tag-value not colon separated" s sexp_of_string)
      | Some (tag, value) ->
        parse_tag tag >>= fun tag -> parse_value tag value >>= fun value -> Ok (tag, value)
    ;;

    let string_of_t (tag, value) = sprintf "%s:%s" tag value
    let print_tag_value' = sprintf "%s:%s"

    (** Find all occurrences of [x'] in the association list [l]. *)
    let find_all l x' =
      let rec loop accum = function
        | [] -> accum
        | (x, y) :: l ->
          let accum = if Poly.(x = x') then y :: accum else accum in
          loop accum l
      in
      List.rev (loop [] l)
    ;;

    (** Find exactly 1 occurrence [x] in association list [l]. Return
    error if [x] is not defined exactly once. *)
    let find1 header_item_tag l x =
      match find_all l x with
      | [] ->
        Error
          (Error.create
             "required tag not found"
             (header_item_tag, x)
             [%sexp_of: Type.t * string])
      | y :: [] -> Ok y
      | ys ->
        Error
          (Error.create
             "tag found multiple times"
             (header_item_tag, x, ys)
             [%sexp_of: Type.t * string * string list])
    ;;

    (** Find 0 or 1 occurrence [x] in association list [l]. Return
    error if [x] is defined more than once. *)
    let find01 header_item_tag l x =
      match find_all l x with
      | [] -> Ok None
      | y :: [] -> Ok (Some y)
      | ys ->
        Error
          (Error.create
             "tag found multiple times"
             (header_item_tag, x, ys)
             [%sexp_of: Type.t * string * string list])
    ;;

    (** Assert that [tvl] contains at most the given [tags]. *)
    let assert_tags header_item_tag tvl tags =
      let expected_tags = Set.of_list (module String) tags in
      let got_tags = List.map tvl ~f:fst |> Set.of_list (module String) in
      let unexpected_tags = Set.diff got_tags expected_tags in
      if Set.length unexpected_tags = 0
      then Ok ()
      else
        Error
          (Error.create
             "unexpected tag for given header item type"
             (header_item_tag, unexpected_tags)
             [%sexp_of: Type.t * Set.M(String).t])
    ;;
  end

  module HD = struct
    module SO = struct
      type t =
        [ `Unknown
        | `Unsorted
        | `Query_name
        | `Coordinate
        ]
      [@@deriving sexp]

      let t_of_string = function
        | "unknown" -> Ok `Unknown
        | "unsorted" -> Ok `Unsorted
        | "queryname" -> Ok `Query_name
        | "coordinate" -> Ok `Coordinate
        | x -> Error (Error.create "invalid sort order" x sexp_of_string)
      ;;

      let string_of_t x =
        Tag_value.print_tag_value'
          "SO"
          (match x with
           | `Unknown -> "unknown"
           | `Unsorted -> "unsorted"
           | `Query_name -> "queryname"
           | `Coordinate -> "coordinate")
      ;;
    end

    module GO = struct
      type t =
        [ `None
        | `Query
        | `Reference
        ]
      [@@deriving sexp]

      let t_of_string = function
        | "none" -> Ok `None
        | "query" -> Ok `Query
        | "reference" -> Ok `Reference
        | x -> Error (Error.create "invalid group order" x sexp_of_string)
      ;;

      let string_of_t x =
        Tag_value.print_tag_value'
          "GO"
          (match x with
           | `None -> "none"
           | `Query -> "query"
           | `Reference -> "reference")
      ;;
    end

    module VN = struct
      type t = string [@@deriving sexp]

      let t_of_string s =
        let err =
          Error (Error.create "invalid version" (`HD, s) [%sexp_of: Type.t * string])
        in
        match String.lsplit2 ~on:'.' s with
        | None -> err
        | Some (a, b) ->
          if String.for_all a ~f:Char.is_digit && String.for_all b ~f:Char.is_digit
          then Ok s
          else err
      ;;

      let string_of_t x = Tag_value.print_tag_value' "VN" x
    end

    type t =
      { version : VN.t
      ; sort_order : SO.t option
      ; group_order : GO.t option
      }
    [@@deriving sexp]

    let make ~version ?sort_order ?group_order () =
      VN.t_of_string version >>| fun version -> { version; sort_order; group_order }
    ;;

    let t_of_tag_value_list tvl =
      Tag_value.find1 `HD tvl "VN"
      >>= fun version ->
      Tag_value.find01 `HD tvl "SO"
      >>?~ SO.t_of_string
      >>= fun sort_order ->
      Tag_value.find01 `HD tvl "GO"
      >>?~ GO.t_of_string
      >>= fun group_order ->
      Tag_value.assert_tags `HD tvl [ "VN"; "SO"; "GO" ]
      >>= fun () -> make ~version ?sort_order ?group_order ()
    ;;

    let string_of_t { version; sort_order; group_order } =
      sprintf
        "@HD\tVN:%s%s%s"
        version
        (match sort_order with
         | None -> ""
         | Some x -> sprintf "\t%s" (SO.string_of_t x))
        (match group_order with
         | None -> ""
         | Some x -> sprintf "\t%s" (GO.string_of_t x))
    ;;
  end

  module SQ = struct
    type t =
      { name : string
      ; length : int
      ; assembly : string option
      ; md5 : string option
      ; species : string option
      ; uri : string option
      }
    [@@deriving sexp]

    let make ~name ~length ?assembly ?md5 ?species ?uri () =
      let is_name_first_char_ok = function
        | '!' .. ')' | '+' .. '<' | '>' .. '~' -> true
        | _ -> false
      in
      let is_name_other_char_ok = function
        | '!' .. '~' -> true
        | _ -> false
      in
      (if 1 <= length && length <= 2147483647
       then Ok length
       else
         Error (Error.create "invalid reference sequence length" length [%sexp_of: int]))
      >>= fun length ->
      (if
         String.length name > 0
         && String.foldi name ~init:true ~f:(fun i accum c ->
           accum && if i = 0 then is_name_first_char_ok c else is_name_other_char_ok c)
       then Ok name
       else Error (Error.create "invalid ref seq name" name [%sexp_of: string]))
      >>= fun name -> Ok { name; length; assembly; md5; species; uri }
    ;;

    let t_of_tag_value_list tvl =
      Tag_value.find1 `SQ tvl "SN"
      >>= fun name ->
      Tag_value.find1 `SQ tvl "LN"
      >>= fun length ->
      (try Ok (Int.of_string length) with
       | _ -> Error (Error.create "invalid ref seq length" length sexp_of_string))
      >>= fun length ->
      Tag_value.find01 `SQ tvl "AS"
      >>= fun assembly ->
      Tag_value.find01 `SQ tvl "M5"
      >>= fun md5 ->
      Tag_value.find01 `SQ tvl "SP"
      >>= fun species ->
      Tag_value.find01 `SQ tvl "UR"
      >>= fun uri ->
      Tag_value.assert_tags `SQ tvl [ "SN"; "LN"; "AS"; "M5"; "SP"; "UR" ]
      >>= fun () -> make ~name ~length ?assembly ?md5 ?species ?uri ()
    ;;

    let string_of_t (x : t) =
      sprintf
        "@SQ\tSN:%s\tLN:%d%s%s%s%s"
        x.name
        x.length
        (match x.assembly with
         | None -> ""
         | Some x -> sprintf "\tAS:%s" x)
        (match x.md5 with
         | None -> ""
         | Some x -> sprintf "\tM5:%s" x)
        (match x.species with
         | None -> ""
         | Some x -> sprintf "\tSP:%s" x)
        (match x.uri with
         | None -> ""
         | Some x -> sprintf "\tUR:%s" x)
    ;;
  end

  module RG = struct
    module PL = struct
      type t =
        [ `Capillary
        | `LS454
        | `Illumina
        | `Solid
        | `Helicos
        | `Ion_Torrent
        | `Pac_Bio
        ]
      [@@deriving sexp]

      let t_of_string = function
        | "CAPILLARY" -> Ok `Capillary
        | "LS454" -> Ok `LS454
        | "ILLUMINA" -> Ok `Illumina
        | "SOLID" -> Ok `Solid
        | "HELICOS" -> Ok `Helicos
        | "IONTORRENT" -> Ok `Ion_Torrent
        | "PACBIO" -> Ok `Pac_Bio
        | x -> Error (Error.create "unknown platform" x sexp_of_string)
      ;;

      let string_of_t = function
        | `Capillary -> "CAPILLARY"
        | `LS454 -> "LS454"
        | `Illumina -> "ILLUMINA"
        | `Solid -> "SOLID"
        | `Helicos -> "HELICOS"
        | `Ion_Torrent -> "IONTORRENT"
        | `Pac_Bio -> "PACBIO"
      ;;
    end

    type t =
      { id : string
      ; seq_center : string option
      ; description : string option
      ; run_date : [ `Date of string | `Time of string ] option
      ; flow_order : string option
      ; key_seq : string option
      ; library : string option
      ; program : string option
      ; predicted_median_insert_size : int option
      ; platform : PL.t option
      ; platform_unit : string option
      ; sample : string option
      }
    [@@deriving sexp]

    let make
          ~id
          ?seq_center
          ?description
          ?run_date
          ?flow_order
          ?key_seq
          ?library
          ?program
          ?predicted_median_insert_size
          ?platform
          ?platform_unit
          ?sample
          ()
      =
      (match run_date with
       | None -> Ok None
       | Some run_date -> (
         try Ok (Some (`Date run_date)) with
         | _ -> (
           try Ok (Some (`Time run_date)) with
           | _ -> Error (Error.create "invalid run date/time" run_date sexp_of_string))))
      >>= fun run_date ->
      (match flow_order with
       | None -> Ok None
       | Some "" -> Or_error.error_string "invalid empty flow order"
       | Some "*" -> Ok flow_order
       | Some x ->
         if
           String.for_all x ~f:(function
             | 'A'
             | 'C'
             | 'M'
             | 'G'
             | 'R'
             | 'S'
             | 'V'
             | 'T'
             | 'W'
             | 'Y'
             | 'H'
             | 'K'
             | 'D'
             | 'B'
             | 'N' -> true
             | _ -> false)
         then Ok flow_order
         else Error (Error.create "invalid flow order" x sexp_of_string))
      >>| fun flow_order ->
      { id
      ; seq_center
      ; description
      ; run_date
      ; flow_order
      ; key_seq
      ; library
      ; program
      ; predicted_median_insert_size
      ; platform
      ; platform_unit
      ; sample
      }
    ;;

    let t_of_tag_value_list tvl =
      Tag_value.find1 `RG tvl "ID"
      >>= fun id ->
      Tag_value.find01 `RG tvl "CN"
      >>= fun seq_center ->
      Tag_value.find01 `RG tvl "DS"
      >>= fun description ->
      Tag_value.find01 `RG tvl "DT"
      >>= fun run_date ->
      Tag_value.find01 `RG tvl "FO"
      >>= fun flow_order ->
      Tag_value.find01 `RG tvl "KS"
      >>= fun key_seq ->
      Tag_value.find01 `RG tvl "LB"
      >>= fun library ->
      Tag_value.find01 `RG tvl "PG"
      >>= fun program ->
      Tag_value.find01 `RG tvl "PI"
      >>?~ (fun predicted_median_insert_size ->
      try Ok (Int.of_string predicted_median_insert_size) with
      | _ ->
        Error
          (Error.create
             "invalid predicted median insert size"
             predicted_median_insert_size
             sexp_of_string))
      >>= fun predicted_median_insert_size ->
      Tag_value.find01 `RG tvl "PL"
      >>?~ PL.t_of_string
      >>= fun platform ->
      Tag_value.find01 `RG tvl "PU"
      >>= fun platform_unit ->
      Tag_value.find01 `RG tvl "SM"
      >>= fun sample ->
      Tag_value.assert_tags
        `RG
        tvl
        [ "ID"; "CN"; "DS"; "DT"; "FO"; "KS"; "LB"; "PG"; "PI"; "PL"; "PU"; "SM" ]
      >>= fun () ->
      make
        ~id
        ?seq_center
        ?description
        ?run_date
        ?flow_order
        ?key_seq
        ?library
        ?program
        ?predicted_median_insert_size
        ?platform
        ?platform_unit
        ?sample
        ()
    ;;

    let string_of_t (x : t) =
      let s tag value =
        match value with
        | None -> ""
        | Some x -> sprintf "\t%s:%s" tag x
      in
      sprintf
        "@RG\tID:%s%s%s%s%s%s%s%s%s%s%s%s"
        x.id
        (s "CN" x.seq_center)
        (s "DS" x.description)
        (s "DT" (Option.map x.run_date ~f:(function `Date x | `Time x -> x)))
        (s "FO" x.flow_order)
        (s "KS" x.key_seq)
        (s "LB" x.library)
        (s "PG" x.program)
        (s "PI" (Option.map x.predicted_median_insert_size ~f:Int.to_string))
        (s "PL" (Option.map x.platform ~f:PL.string_of_t))
        (s "PU" x.platform_unit)
        (s "SM" x.sample)
    ;;
  end

  module PG = struct
    type t =
      { id : string
      ; name : string option
      ; command_line : string option
      ; previous_id : string option
      ; description : string option
      ; version : string option
      }
    [@@deriving sexp]

    let t_of_tag_value_list tvl =
      Tag_value.find1 `PG tvl "ID"
      >>= fun id ->
      Tag_value.find01 `PG tvl "PN"
      >>= fun name ->
      Tag_value.find01 `PG tvl "CL"
      >>= fun command_line ->
      Tag_value.find01 `PG tvl "PP"
      >>= fun previous_id ->
      Tag_value.find01 `PG tvl "DS"
      >>= fun description ->
      Tag_value.find01 `PG tvl "VN"
      >>= fun version ->
      Tag_value.assert_tags `PG tvl [ "ID"; "PN"; "CL"; "PP"; "DS"; "VN" ]
      >>| fun () -> { id; name; command_line; previous_id; description; version }
    ;;

    let string_of_t (x : t) =
      let s tag value =
        match value with
        | None -> ""
        | Some x -> sprintf "\t%s:%s" tag x
      in
      sprintf
        "@PG\tID:%s%s%s%s%s%s"
        x.id
        (s "PN" x.name)
        (s "CL" x.command_line)
        (s "PP" x.previous_id)
        (s "DS" x.description)
        (s "VN" x.version)
    ;;
  end

  module Other = struct
    type t = string * Tag_value.t list [@@deriving sexp]

    let string_of_t ((tag, l) : t) =
      sprintf
        "@%s%s"
        tag
        (List.map l ~f:(fun (x, y) -> sprintf "\t%s:%s" x y) |> String.concat ~sep:"")
    ;;
  end

  module Item = struct
    type t =
      [ `HD of HD.t
      | `SQ of SQ.t
      | `RG of RG.t
      | `PG of PG.t
      | `CO of string
      | `Other of Other.t
      ]
    [@@deriving sexp]

    let t_of_string line =
      let parse_data tag tvl =
        match tag with
        | `HD -> HD.t_of_tag_value_list tvl >>| fun x -> `HD x
        | `SQ -> SQ.t_of_tag_value_list tvl >>| fun x -> `SQ x
        | `RG -> RG.t_of_tag_value_list tvl >>| fun x -> `RG x
        | `PG -> PG.t_of_tag_value_list tvl >>| fun x -> `PG x
        | `Other tag -> Ok (`Other (tag, tvl))
        | `CO -> assert false
      in
      match String.lsplit2 ~on:'\t' line with
      | None -> Error (Error.create "header line contains no tabs" line sexp_of_string)
      | Some (tag, data) -> (
        Type.t_of_string tag
        >>= function
        | `CO -> Ok (`CO data)
        | tag -> (
          match String.split ~on:'\t' data with
          | [] -> assert false
          | "" :: [] -> Error (Error.create "header contains no data" tag Type.sexp_of_t)
          | tvl ->
            Result_list.map tvl ~f:Tag_value.t_of_string >>= fun tvl -> parse_data tag tvl
          ))
    ;;
  end

  module Item_list_rev : sig
    type t = private Item.t list [@@deriving sexp]

    val empty : t
    val append : t -> Item.t -> t
  end = struct
    type t = Item.t list [@@deriving sexp]

    let empty = []
    let append t x = x :: t
  end

  type t =
    { version : HD.VN.t option
    ; sort_order : HD.SO.t option
    ; group_order : HD.GO.t option
    ; ref_seqs : SQ.t list
    ; read_groups : RG.t list
    ; programs : PG.t list
    ; comments : string list
    ; others : Other.t list
    }
  [@@deriving sexp]

  let empty =
    { version = None
    ; sort_order = None
    ; group_order = None
    ; ref_seqs = []
    ; read_groups = []
    ; programs = []
    ; comments = []
    ; others = []
    }
  ;;

  let make
        ?version
        ?sort_order
        ?group_order
        ?(ref_seqs = [])
        ?(read_groups = [])
        ?(programs = [])
        ?(comments = [])
        ?(others = [])
        ()
    =
    [ (match version with
       | None -> None
       | Some x -> (
         match HD.VN.t_of_string x with
         | Error e -> Some e
         | Ok _ -> None))
    ; (if Option.is_some sort_order && Poly.(version = None)
       then
         Some
           (Error.create
              "sort order cannot be defined without version"
              (sort_order, version)
              [%sexp_of: HD.SO.t option * string option])
       else None)
    ; List.map ref_seqs ~f:(fun (x : SQ.t) -> x.name)
      |> List.find_a_dup ~compare:String.compare
      |> Option.map ~f:(fun name ->
        Error.create "duplicate ref seq name" name sexp_of_string)
    ]
    |> List.filter_map ~f:Fn.id
    |> function
    | [] ->
      Ok
        { version
        ; sort_order
        ; group_order
        ; ref_seqs
        ; read_groups
        ; programs
        ; comments
        ; others
        }
    | errs -> Error (Error.of_list errs)
  ;;

  let of_item_list_rev (items : Item_list_rev.t) =
    let rec loop t (items : Item.t list) : t Or_error.t =
      match items with
      | [] -> Ok t
      | item :: items -> (
        match item with
        | `HD { HD.version; sort_order; group_order } -> (
          match t.version with
          | Some _ -> Or_error.error_string "multiple @HD lines not allowed"
          | None -> loop { t with version = Some version; sort_order; group_order } items)
        | `SQ x -> loop { t with ref_seqs = x :: t.ref_seqs } items
        | `RG x -> loop { t with read_groups = x :: t.read_groups } items
        | `PG x -> loop { t with programs = x :: t.programs } items
        | `CO x -> loop { t with comments = x :: t.comments } items
        | `Other x -> loop { t with others = x :: t.others } items)
    in
    match loop empty (items :> Item.t list) with
    | Error _ as e -> e
    | Ok
        { version
        ; sort_order
        ; group_order
        ; ref_seqs
        ; read_groups
        ; programs
        ; comments
        ; others
        } ->
      make
        ?version
        ?sort_order
        ?group_order
        ~ref_seqs
        ~read_groups
        ~programs
        ~comments
        ~others
        ()
  ;;

  let num_items t =
    List.length t.ref_seqs
    + List.length t.read_groups
    + List.length t.programs
    + List.length t.comments
    + List.length t.others
    +
    match t.version with
    | None -> 0
    | Some _ -> 1
  ;;
end

let parse_int_range field lo hi s =
  let out_of_range = sprintf "%s out of range" field in
  let not_an_int = sprintf "%s not an int" field in
  try
    let n = Int.of_string s in
    if lo <= n && n <= hi
    then Ok n
    else Error (Error.create out_of_range (n, lo, hi) [%sexp_of: int * int * int])
  with
  | _ -> Error (Error.create not_an_int s sexp_of_string)
;;

(** Parse a string that can either by "*" or some other regexp, with
  "*" denoting [None]. The given regexp [re] should include "*" as
  one of the alternatives. *)
let parse_opt_string field re s =
  if not (Re.execp re s)
  then Error (Error.create (sprintf "invalid %s" field) s sexp_of_string)
  else (
    match s with
    | "*" -> Ok None
    | _ -> Ok (Some s))
;;

module Qname = struct
  type t = string [@@deriving sexp]

  let regexp =
    let open Re in
    alt [ char '*'; repn (alt [ rg '!' '?'; rg 'A' '~' ]) 1 (Some 255) ] |> compile
  ;;

  let t_option_of_string s = parse_opt_string "QNAME" regexp s

  let string_of_t_option = function
    | Some x -> x
    | None -> "*"
  ;;
end

module Flag = struct
  type t = int [@@deriving sexp]

  let of_int x =
    if 0 <= x && x <= 65535
    then Ok x
    else Error (Error.create "flag out of range" x sexp_of_int)
  ;;

  let t_of_string s =
    try of_int (Int.of_string s) with
    | _ -> Error (Error.create "invalid FLAG" s sexp_of_string)
  ;;

  let string_of_t = Int.to_string
  let flag_is_set s f = f land s <> 0
  let has_multiple_segments = flag_is_set 0x1
  let each_segment_properly_aligned = flag_is_set 0x2
  let segment_unmapped = flag_is_set 0x4
  let next_segment_unmapped = flag_is_set 0x8
  let seq_is_reverse_complemented = flag_is_set 0x10
  let next_seq_is_reverse_complemented = flag_is_set 0x20
  let first_segment = flag_is_set 0x40
  let last_segment = flag_is_set 0x80
  let secondary_alignment = flag_is_set 0x100
  let not_passing_quality_controls = flag_is_set 0x200
  let pcr_or_optical_duplicate = flag_is_set 0x400
  let supplementary_alignment = flag_is_set 0x800
end

module Rname = struct
  type t = string [@@deriving sexp]

  let regexp = Re.Perl.compile_pat "^\\*|[!-()+-<>-~][!-~]*$"
  let t_option_of_string s = parse_opt_string "RNAME" regexp s

  let string_of_t_option = function
    | Some x -> x
    | None -> "*"
  ;;
end

module Pos = struct
  type t = int [@@deriving sexp]

  let t_option_of_string s =
    parse_int_range "POS" 0 2147483647 s
    >>| function
    | 0 -> None
    | x -> Some x
  ;;

  let string_of_t_option = function
    | Some x -> Int.to_string x
    | None -> "0"
  ;;
end

module Mapq = struct
  type t = int [@@deriving sexp]

  let t_option_of_string s =
    parse_int_range "MAPQ" 0 255 s
    >>| function
    | 255 -> None
    | x -> Some x
  ;;

  let string_of_t_option = function
    | Some x -> Int.to_string x
    | None -> "255"
  ;;
end

module Cigar = struct
  module Op = struct
    type t =
      [ `Alignment_match of int
      | `Insertion of int
      | `Deletion of int
      | `Skipped of int
      | `Soft_clipping of int
      | `Hard_clipping of int
      | `Padding of int
      | `Seq_match of int
      | `Seq_mismatch of int
      ]
    [@@deriving sexp]

    let positive i =
      let open Or_error in
      if i > 0
      then return i
      else error_string "positive argument expected for cigar operation"
    ;;

    let alignment_match_of_int i = Or_error.(positive i >>| fun i -> `Alignment_match i)
    let insertion_of_int i = Or_error.(positive i >>| fun i -> `Insertion i)
    let deletion_of_int i = Or_error.(positive i >>| fun i -> `Deletion i)
    let skipped_of_int i = Or_error.(positive i >>| fun i -> `Skipped i)
    let soft_clipping_of_int i = Or_error.(positive i >>| fun i -> `Soft_clipping i)
    let hard_clipping_of_int i = Or_error.(positive i >>| fun i -> `Hard_clipping i)
    let padding_of_int i = Or_error.(positive i >>| fun i -> `Padding i)
    let seq_match_of_int i = Or_error.(positive i >>| fun i -> `Seq_match i)
    let seq_mismatch_of_int i = Or_error.(positive i >>| fun i -> `Seq_mismatch i)

    let string_of_t = function
      | `Alignment_match x -> sprintf "%dM" x
      | `Insertion x -> sprintf "%dI" x
      | `Deletion x -> sprintf "%dD" x
      | `Skipped x -> sprintf "%dN" x
      | `Soft_clipping x -> sprintf "%dS" x
      | `Hard_clipping x -> sprintf "%dH" x
      | `Padding x -> sprintf "%dP" x
      | `Seq_match x -> sprintf "%d=" x
      | `Seq_mismatch x -> sprintf "%dX" x
    ;;
  end

  type t = Op.t list [@@deriving sexp]

  let t_of_string text =
    match text with
    | "*" -> Ok []
    | "" -> Error (Error.create "invalid cigar string" text sexp_of_string)
    | _ ->
      let ch = Stdlib.Scanf.Scanning.from_string text in
      let rec loop accum =
        if Stdlib.Scanf.Scanning.end_of_input ch
        then Ok accum
        else (
          try
            let n = Stdlib.Scanf.bscanf ch "%d" Fn.id in
            let c = Stdlib.Scanf.bscanf ch "%c" Fn.id in
            let x =
              match c with
              | 'M' -> Op.alignment_match_of_int n
              | 'I' -> Op.insertion_of_int n
              | 'D' -> Op.deletion_of_int n
              | 'N' -> Op.skipped_of_int n
              | 'S' -> Op.soft_clipping_of_int n
              | 'H' -> Op.hard_clipping_of_int n
              | 'P' -> Op.padding_of_int n
              | '=' -> Op.seq_match_of_int n
              | 'X' -> Op.seq_mismatch_of_int n
              | other ->
                Or_error.error "invalid cigar operation type" other Char.sexp_of_t
            in
            Or_error.tag x ~tag:"Sam.parse_cigar: invalid cigar string"
            >>= fun x -> loop (x :: accum)
          with
          | _ -> Error (Error.create "invalid cigar string" text sexp_of_string))
      in
      loop [] >>| List.rev
  ;;

  let string_of_t = function
    | [] -> "*"
    | cigar_ops -> List.map cigar_ops ~f:Op.string_of_t |> String.concat ~sep:""
  ;;
end

module Rnext = struct
  type t =
    [ `Value of string
    | `Equal_to_RNAME
    ]
  [@@deriving sexp]

  let rnext_re = Re.Perl.compile_pat "^\\*|=|[!-()+-<>-~][!-~]*$"

  let t_option_of_string s =
    if not (Re.execp rnext_re s)
    then Error (Error.create "invalid RNEXT" s sexp_of_string)
    else (
      match s with
      | "*" -> Ok None
      | "=" -> Ok (Some `Equal_to_RNAME)
      | _ -> Ok (Some (`Value s)))
  ;;

  let string_of_t_option = function
    | None -> "*"
    | Some `Equal_to_RNAME -> "="
    | Some (`Value x) -> x
  ;;
end

module Pnext = struct
  type t = int [@@deriving sexp]

  let t_option_of_string s =
    parse_int_range "PNEXT" 0 2147483647 s
    >>| function
    | 0 -> None
    | x -> Some x
  ;;

  let string_of_t_option = function
    | Some x -> Int.to_string x
    | None -> "0"
  ;;
end

module Tlen = struct
  type t = int [@@deriving sexp]

  let t_option_of_string s =
    parse_int_range "TLEN" ~-2147483647 2147483647 s
    >>| function
    | 0 -> None
    | x -> Some x
  ;;

  let string_of_t_option = function
    | Some x -> Int.to_string x
    | None -> "0"
  ;;
end

module Seq = struct
  type t = string [@@deriving sexp]

  let regexp = Re.Perl.compile_pat "^\\*|[A-Za-z=.]+$"
  let t_option_of_string s = parse_opt_string "SEQ" regexp s

  let string_of_t_option = function
    | Some x -> x
    | None -> "*"
  ;;
end

module Qual = struct
  type t = Phred_score.t list [@@deriving sexp]

  let t_of_string s =
    match s with
    | "" -> Or_error.error_string "invalid empty QUAL"
    | "*" -> Ok []
    | _ -> String.to_list s |> Result_list.map ~f:(Phred_score.of_char ~offset:`Offset33)
  ;;

  let string_of_t = function
    | [] -> "*"
    | quals ->
      List.map quals ~f:(fun x ->
        Or_error.ok_exn (Phred_score.to_char ~offset:`Offset33 x))
      |> String.of_char_list
  ;;
end

module Optional_field = struct
  module Value = struct
    type t =
      [ `A of char
      | `i of Int64.t
      | `f of float
      | `Z of string
      | `H of string
      | `B of char * string list
      ]
    [@@deriving sexp]

    let opt_field_Z_re = Re.Perl.compile_pat "^[ !-~]+$"
    let opt_field_H_re = Re.Perl.compile_pat "^[0-9A-F]+$"
    let opt_field_int_re = Re.Perl.compile_pat "^-?[0-9]+$"

    let opt_field_float_re =
      Re.Perl.compile_pat "^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$"
    ;;

    let parse_err typ value =
      Error (Error.create "invalid value" (typ, value) [%sexp_of: string * string])
    ;;

    let t_of_char_A = function
      | '!' .. '~' as value -> Ok (`A value)
      | c -> parse_err "A" (sprintf "char code %d" (Char.to_int c))
    ;;

    let t_of_int64_i i = `i i
    let t_of_float_f f = `f f

    let t_of_string_Z value =
      if Re.execp opt_field_Z_re value then Ok (`Z value) else parse_err "Z" value
    ;;

    let t_of_string_H value =
      if Re.execp opt_field_H_re value then Ok (`H value) else parse_err "H" value
    ;;

    let t_of_char_string_list_B elt_type elts =
      let valid_args =
        match elt_type with
        | 'c' | 'C' | 's' | 'S' | 'i' | 'I' ->
          List.for_all elts ~f:(Re.execp opt_field_int_re)
        | 'f' -> List.for_all elts ~f:(Re.execp opt_field_float_re)
        | _ -> false
      in
      if valid_args
      then Ok (`B (elt_type, elts))
      else
        Error
          (Error.create
             "invalid value"
             ("B", elt_type, elts)
             [%sexp_of: string * char * string list])
    ;;

    let t_of_string s =
      match String.lsplit2 s ~on:':' with
      | None -> Error (Error.create "missing TYPE in optional field" s sexp_of_string)
      | Some (typ, value) -> (
        match typ with
        | "A" ->
          if String.length value = 1 then t_of_char_A value.[0] else parse_err typ value
        | "i" -> (
          try
            if not (Re.execp opt_field_int_re value) then failwith "";
            Ok (t_of_int64_i (Int64.of_string value))
            (* matching the regular expression is not enough: the number could not fit in 64 bits *)
          with
          | _ -> parse_err typ value)
        | "f" -> (
          try
            if not (Re.execp opt_field_float_re value) then failwith "";
            Ok (t_of_float_f (Float.of_string value))
            (* matching the regular expression is not enough: the number could not fit in native floats *)
          with
          | _ -> parse_err typ value)
        | "Z" -> t_of_string_Z value
        | "H" -> t_of_string_H value
        | "B" -> (
          match String.split ~on:',' value with
          | num_typ :: values ->
            if String.length num_typ = 1
            then t_of_char_string_list_B num_typ.[0] values
            else Error (Error.create "invalid array type" num_typ sexp_of_string)
          | _ -> assert false (* [String.split] cannot return an empty list *))
        | _ -> Error (Error.create "invalid type" typ sexp_of_string))
    ;;
  end

  type t =
    { tag : string
    ; value : Value.t
    }
  [@@deriving sexp]

  let opt_field_tag_re = Re.Perl.compile_pat "^[A-Za-z][A-Za-z0-9]$"

  let make tag value =
    if not (Re.execp opt_field_tag_re tag)
    then Error (Error.create "invalid TAG" tag sexp_of_string)
    else Ok { tag; value }
  ;;

  let t_of_string s =
    match String.lsplit2 s ~on:':' with
    | None -> Error (Error.create "missing TAG in optional field" s sexp_of_string)
    | Some (tag, s) -> Value.t_of_string s >>= fun value -> make tag value
  ;;

  let string_of_t (x : t) =
    let typ, value =
      match x.value with
      | `A x -> 'A', Char.to_string x
      | `i x -> 'i', Int64.to_string x
      | `f x -> 'f', Float.to_string x
      | `Z x -> 'Z', x
      | `H x -> 'H', x
      | `B (c, l) -> 'B', String.concat ~sep:"," (String.of_char c :: l)
    in
    sprintf "%s:%c:%s" x.tag typ value
  ;;

  module Test = struct
    let test_parse_optional_field (s : string) (v : t Or_error.t) =
      let f : t Or_error.t = t_of_string s in
      Stdlib.Printf.printf
        "Optional field value (i type): %s = %s: %b\n"
        (f |> Or_error.sexp_of_t sexp_of_t |> Sexp.to_string_hum)
        (v |> Or_error.sexp_of_t sexp_of_t |> Sexp.to_string_hum)
        (Or_error.equal Poly.equal f v)
    ;;

    let%expect_test "test_parser" =
      test_parse_optional_field "YS:i:-1" (make "YS" (Value.t_of_int64_i (-1L)));
      [%expect
        {| Optional field value (i type): (Ok ((tag YS) (value (i -1)))) = (Ok ((tag YS) (value (i -1)))): true |}]
    ;;
  end
end

module Alignment = struct
  type t =
    { qname : Qname.t option
    ; flag : Flag.t
    ; rname : Rname.t option
    ; pos : Pos.t option
    ; mapq : Mapq.t option
    ; cigar : Cigar.t
    ; rnext : Rnext.t option
    ; pnext : Pnext.t option
    ; tlen : Tlen.t option
    ; seq : Seq.t option
    ; qual : Qual.t
    ; optional_fields : Optional_field.t list
    }
  [@@deriving sexp]

  let make
        ?ref_seqs
        ?qname
        ~flag
        ?rname
        ?pos
        ?mapq
        ?(cigar = [])
        ?rnext
        ?pnext
        ?tlen
        ?seq
        ?(qual = [])
        ?(optional_fields = [])
        ()
    =
    [ (match ref_seqs, rname with
       | None, _ | _, None -> None
       | Some ref_seqs, Some rname ->
         if Set.mem ref_seqs rname
         then None
         else Some (Error.create "RNAME not defined in any SQ line" rname sexp_of_string))
    ; (match ref_seqs, rnext with
       | None, _ | _, None -> None
       | Some _, Some `Equal_to_RNAME ->
         None (* error will already be detected in RNAME check above *)
       | Some ref_seqs, Some (`Value rnext) ->
         if Set.mem ref_seqs rnext
         then None
         else Some (Error.create "RNEXT not defined in any SQ line" rnext sexp_of_string))
    ; (match seq, qual with
       | _, [] -> None
       | None, _ -> Some (Error.of_string "QUAL provided without SEQ")
       | Some seq, _ ->
         let s = String.length seq in
         let q = List.length qual in
         if s = q
         then None
         else
           Some (Error.create "SEQ and QUAL lengths differ" (s, q) [%sexp_of: int * int]))
    ; List.map optional_fields ~f:(fun x -> x.tag)
      |> List.find_a_dup ~compare:String.compare
      |> Option.map ~f:(fun dup ->
        Error.create "TAG occurs more than once" dup sexp_of_string)
    ]
    |> List.filter_map ~f:Fn.id
    |> function
    | [] ->
      Ok
        { qname
        ; flag
        ; rname
        ; pos
        ; mapq
        ; cigar
        ; rnext
        ; pnext
        ; tlen
        ; seq
        ; qual
        ; optional_fields
        }
    | errs -> Error (Error.of_list errs)
  ;;

  let t_of_string ?ref_seqs line =
    match String.split ~on:'\t' line with
    | qname
      :: flag
      :: rname
      :: pos
      :: mapq
      :: cigar
      :: rnext
      :: pnext
      :: tlen
      :: seq
      :: qual
      :: optional_fields ->
      Qname.t_option_of_string qname
      >>= fun qname ->
      Flag.t_of_string flag
      >>= fun flag ->
      Rname.t_option_of_string rname
      >>= fun rname ->
      Pos.t_option_of_string pos
      >>= fun pos ->
      Mapq.t_option_of_string mapq
      >>= fun mapq ->
      Cigar.t_of_string cigar
      >>= fun cigar ->
      Rnext.t_option_of_string rnext
      >>= fun rnext ->
      Pnext.t_option_of_string pnext
      >>= fun pnext ->
      Tlen.t_option_of_string tlen
      >>= fun tlen ->
      Seq.t_option_of_string seq
      >>= fun seq ->
      Qual.t_of_string qual
      >>= fun qual ->
      Result_list.map optional_fields ~f:Optional_field.t_of_string
      >>= fun optional_fields ->
      make
        ?ref_seqs
        ?qname
        ~flag
        ?rname
        ?pos
        ?mapq
        ~cigar
        ?rnext
        ?pnext
        ?tlen
        ?seq
        ~qual
        ~optional_fields
        ()
    | _ -> Or_error.error_string "alignment line contains < 12 fields"
  ;;

  let string_of_t a =
    sprintf
      "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"
      (Qname.string_of_t_option a.qname)
      (Flag.string_of_t a.flag)
      (Rname.string_of_t_option a.rname)
      (Pos.string_of_t_option a.pos)
      (Mapq.string_of_t_option a.mapq)
      (Cigar.string_of_t a.cigar)
      (Rnext.string_of_t_option a.rnext)
      (Pnext.string_of_t_option a.pnext)
      (Tlen.string_of_t_option a.tlen)
      (Seq.string_of_t_option a.seq)
      (Qual.string_of_t a.qual)
      (List.map a.optional_fields ~f:Optional_field.string_of_t |> String.concat ~sep:"\t")
  ;;
end

module State = struct
  module Phase = struct
    type t =
      [ `Header of Header.Item_list_rev.t
      | `Alignment of Header.t * Alignment.t
      ]
  end

  type 'a t =
    { parse_line : string -> 'a t Or_error.t
    ; phase : Phase.t
    ; data : 'a
    ; on_alignment : 'a -> Header.t -> Alignment.t -> 'a
    }

  let rec parse_header_line ~on_alignment ~data items line : 'a t Or_error.t =
    match Header.Item.t_of_string line with
    | Ok item ->
      Ok (header_state ~on_alignment ~data (Header.Item_list_rev.append items item))
    | Error _ -> (
      (* If line couldn't be parsed as a header item, assume the header
         section is complete and try to parse the line as an alignment. *)
      match Header.of_item_list_rev items with
      | Error _ as e -> e
      | Ok header -> (
        match Alignment.t_of_string line with
        | Error _ as e -> e
        | Ok alignment ->
          let data = on_alignment data header alignment in
          Ok (alignment_state ~on_alignment ~data header alignment)))

  and header_state ~on_alignment ~data items : 'a t =
    { parse_line = parse_header_line ~on_alignment ~data items
    ; phase = `Header items
    ; data
    ; on_alignment
    }

  and parse_alignment_line ~on_alignment ~data header line =
    match Alignment.t_of_string line with
    | Error _ as e -> e
    | Ok aln ->
      let data = on_alignment data header aln in
      Ok (alignment_state ~on_alignment ~data header aln)

  and alignment_state ~on_alignment ~data header alignment : 'a t =
    { parse_line = parse_alignment_line ~on_alignment ~data header
    ; phase = `Alignment (header, alignment)
    ; data
    ; on_alignment
    }
  ;;

  let init ~on_alignment ~data : 'a t =
    header_state ~on_alignment ~data Header.Item_list_rev.empty
  ;;

  let reduce { parse_line; phase = _; data = _; on_alignment = _ } line = parse_line line
  let reduce_exn x line = reduce x line |> Or_error.ok_exn

  let header { phase; parse_line = _; data = _; on_alignment = _ } =
    match phase with
    | `Header items -> Header.of_item_list_rev items
    | `Alignment (header, _) -> Ok header
  ;;

  let header_exn x = header x |> Or_error.ok_exn
end

let of_lines lines =
  let on_alignment data _header alignment = alignment :: data in
  let rec loop state lines =
    match lines with
    | [] -> Ok state
    | line :: lines -> (
      match State.reduce state line with
      | Error _ as e -> e
      | Ok state -> loop state lines)
  in
  match loop (State.init ~on_alignment ~data:[]) lines with
  | Error _ as e -> e
  | Ok state -> (
    let alignments = List.rev state.data in
    match State.header state with
    | Error _ as e -> e
    | Ok header -> Ok (header, alignments))
;;

(* We could implement this as [of_lines |> ok_exn], but we also want to demonstrate
   how to use [reduce_exn] versus [reduce] to contrast with the implementation of
   [of_lines] above. *)
let of_lines_exn lines =
  let on_alignment data _header alignment = alignment :: data in
  let state =
    List.fold lines ~init:(State.init ~on_alignment ~data:[]) ~f:State.reduce_exn
  in
  let header = State.header_exn state in
  let alignments = List.rev state.data in
  header, alignments
;;

let of_string s = s |> String.split_lines |> of_lines
let of_string_exn s = s |> String.split_lines |> of_lines_exn

module Test = struct
  (* module Sam = Biocaml_unix.Sam_deprecated *)
  (* module Tfxm = Biocaml_unix.Tfxm *)

  (* let test_parser_deprecated () = *)
  (*   let transfo = Sam.Transform.string_to_raw () in *)
  (*   let test_line l f = *)
  (*     Tfxm.feed transfo (l ^ "\n"); *)
  (*     assert_bool l (f (Tfxm.next transfo)) *)
  (*   in *)
  (*   let test_output l o = test_line l (fun oo -> `output (Ok o) = oo) in *)

  (*   test_output "@CO\tsome comment" (`comment "some comment"); *)
  (*   test_output "@HD\tVN:1.3\tSO:coordinate" *)
  (*     (`header ("HD", [ "VN", "1.3"; "SO", "coordinate"])); *)
  (*   test_output "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT\t*" *)
  (*     (`alignment *)
  (*         {Sam.qname = "r001"; flag = 83; rname = "ref"; pos = 37; mapq = 30; *)
  (*          cigar = "9M"; rnext = "="; pnext = 7; tlen = -39; seq = "CAGCGCCAT"; *)
  (*          qual = "*"; optional = []}); *)
  (*   test_output "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT\t*\tNM:i:0" *)
  (*     (`alignment *)
  (*         {Sam.qname = "r001"; flag = 83; rname = "ref"; pos = 37; mapq = 30; *)
  (*          cigar = "9M"; rnext = "="; pnext = 7; tlen = -39; seq = "CAGCGCCAT"; *)
  (*          qual = "*"; optional = [("NM", 'i', "0")]}); *)
  (*   test_output "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT\t*\tNM:i:0\tKJ:A:a" *)
  (*     (`alignment *)
  (*         {Sam.qname = "r001"; flag = 83; rname = "ref"; pos = 37; mapq = 30; *)
  (*          cigar = "9M"; rnext = "="; pnext = 7; tlen = -39; seq = "CAGCGCCAT"; *)
  (*          qual = "*"; optional = [("NM", 'i', "0"); ("KJ", 'A', "a")]}); *)
  (*   test_line "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT\t*\tNM:i:0\tKJ:A" *)
  (*     (function *)
  (*     | `output (Error (`wrong_optional_field (_, _))) -> true *)
  (*     | _ -> false); *)
  (*   test_line "r001\t83\tref\t37\t30\t9M\t=\t7h\t-39\tCAGCGCCAT\t*\tNM:i:0\tKJ:A:a" *)
  (*     (function *)
  (*     | `output (Error (`not_an_int (_, "pnext", "7h")))  -> true *)
  (*     | _ -> false); *)
  (*   test_line  "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT" *)
  (*     (function *)
  (*     | `output (Error (`wrong_alignment (_, _))) -> true *)
  (*     | _ -> false); *)
  (*   test_line "@HD\tT" *)
  (*     (function *)
  (*     | `output (Error (`invalid_tag_value_list (_, ["T"]))) -> true *)
  (*     | _ -> false); *)
  (*   () *)

  (* let test_item_parser_deprecated () = *)
  (*   let c = ref 0 in *)
  (*   let check t v f = *)
  (*     let p = Sam.Transform.raw_to_string () in *)
  (*     incr c; *)
  (*     Tfxm.feed t v; *)
  (*     let res = f (Tfxm.next t) in *)
  (*     if not res then *)
  (*       eprintf "Error on %s\n" *)
  (*         Tfxm.( *)
  (*           match feed p v; next p with `output o -> o | _ -> failwith "printer!" *)
  (*         ); *)
  (*     assert_bool (sprintf "test_item_parser.check %d" !c) res *)
  (*   in *)
  (*   let check_output t v o = check t v ((=) (`output (Ok o))) in *)

  (*   let t = Sam.Transform.raw_to_item () in *)
  (*   check_output t (`comment "comment") (`comment "comment"); *)
  (*   check t (`header ("HD", ["VN", "42.1"])) (function *)
  (*   | `output (Error (`header_line_not_first 2)) -> true *)
  (*   | _ -> false); *)

  (*   let t = Sam.Transform.raw_to_item () in *)
  (*   check_output t (`header ("HD", ["VN", "42.1"])) *)
  (*     (`header_line ("42.1", `unknown, [])); *)
  (*   check t (`header ("SQ", [])) (function *)
  (*   | `output (Error (`missing_ref_sequence_name [])) -> true *)
  (*   | _ -> false); *)
  (*   check t (`header ("SQ", ["SN", "chr0"])) (function *)
  (*   | `output (Error (`missing_ref_sequence_length [("SN", "chr0")])) -> true *)
  (*   | _ -> false); *)
  (*   check t (`header ("SQ", ["SN", "chr0"; "LN", "not an int"])) (function *)
  (*   | `output (Error (`wrong_ref_sequence_length _))  -> true *)
  (*   | _ -> false); *)

  (*   let t = Sam.Transform.raw_to_item () in *)
  (*   check_output t (`header ("HD", ["VN", "42.1"; *)
  (*                                   "SO", "coordinate"; "HP", "some other"])) *)
  (*     (`header_line ("42.1", `coordinate, [("HP", "some other")])); *)
  (*   check t (`header ("SQ", ["SN", "chr0"; "LN", "42"])) ((=) `not_ready); *)
  (*   check t (`header ("SQ", ["SN", "chr1"; "LN", "42"; "M5", "abd34f90"])) *)
  (*     ((=) `not_ready); *)
  (*   (\* the ref-info is being buffered, the first alignment will output it *\) *)
  (*   check_output t (`alignment *)
  (*             {Sam.qname = "r001"; flag = 83; rname = "ref"; pos = 37; mapq = 30; *)
  (*              cigar = "9M"; rnext = "="; pnext = 7; tlen = -39; seq = "CAGCGCCAT"; *)
  (*              qual = "*"; optional = [("NM", 'i', "0")]}) *)
  (*     (`reference_sequence_dictionary *)
  (*         [| *)
  (*           {Sam.ref_name = "chr0"; ref_length = 42; ref_assembly_identifier = None; *)
  (*            ref_checksum = None; ref_species = None; ref_uri = None; *)
  (*            ref_unknown = []}; *)
  (*           {Sam.ref_name = "chr1"; ref_length = 42; ref_assembly_identifier = None; *)
  (*            ref_checksum = Some "abd34f90"; ref_species = None; ref_uri = None; *)
  (*            ref_unknown = []}; *)
  (*         |]); *)
  (*   (\* This one get the previous alignment: *\) *)
  (*   check_output t *)
  (*     (`alignment *)
  (*         {Sam.qname = "chr0"; flag = 83; rname = "chr0"; pos = 37; mapq = 30; *)
  (*          cigar = "9M"; rnext = "*"; pnext = 7; tlen = -39; seq = "CAGCGCCAT"; *)
  (*          qual = "*"; optional = [("NM", 'i', "0")]}) *)
  (*     (`alignment *)
  (*         {Sam.query_template_name = "r001"; flag = Sam.Flag.of_int 83; *)
  (*          reference_sequence = `name "ref"; position = Some 37; *)
  (*          mapping_quality = Some 30; cigar_operations = [|`M 9|]; *)
  (*          next_reference_sequence = `qname; next_position = Some 7; *)
  (*          template_length = Some (-39); sequence = `string "CAGCGCCAT"; *)
  (*          quality = [| |]; optional_content = [ "NM", 'i', `int 0] }); *)
  (*   Tfxm.stop t; *)
  (*   (\* We still have one to get: *\) *)
  (*   assert_bool "last alignment" (Tfxm.next t = *)
  (*       `output (Ok *)
  (*                  (`alignment *)
  (*                      {Sam.query_template_name = "chr0"; flag = Sam.Flag.of_int 83; *)
  (*                       reference_sequence = *)
  (*                          `reference_sequence *)
  (*                            {Sam.ref_name = "chr0"; ref_length = 42; *)
  (*                             ref_assembly_identifier = None; *)
  (*                             ref_checksum = None; ref_species = None; ref_uri = None; *)
  (*                             ref_unknown = []}; *)
  (*                       position = Some 37; mapping_quality = Some 30; *)
  (*                       cigar_operations = [|`M 9|]; next_reference_sequence = `none; *)
  (*                       next_position = Some 7; template_length = Some (-39); *)
  (*                       sequence = `string "CAGCGCCAT"; quality = [| |]; *)
  (*                       optional_content = [ "NM", 'i', `int 0]}))); *)
  (*   assert_bool "EOS" (Tfxm.next t = `end_of_stream); *)

  (*
     check (`header ("HD", ["VN", "42.1"; "SO", "coordinate"]));
  check (`header ("HD", ["VN", "42.1"; "SO", "wut?"]));
  check (`header ("HD", ["VN", "42.1"; "SO", "coordinate"; "HP", "some other"]));
  check (`header ("HD", []));
  check (`header ("SQ", []));
  check (`header ("SQ", ["SN", "chr0"]));
  check (`header ("SQ", ["SN", "chr0"; "LN", "not an int"]));
  check (`header ("SQ", ["SN", "chr0"; "LN", "42"]));
  check (`header ("SQ", ["SN", "chr1"; "LN", "42"; "M5", "abd34f90"]));
  check (`header ("RR", ["SN", "chr1"; "LN", "42"; "M5", "abd34f90"]));
  check (`alignment
            {qname = "r001"; flag = 83; rname = "ref"; pos = 37; mapq = 30;
             cigar = "9M"; rnext = "="; pnext = 7; tlen = -39; seq = "CAGCGCCAT";
             qual = "*"; optional = [("NM", 'i', "0")]});

  check (`alignment
            {qname = "chr0"; flag = 83; rname = "chr0"; pos = 37; mapq = 30;
             cigar = "9M"; rnext = "*"; pnext = 7; tlen = -39; seq = "CAGCGCCAT";
             qual = "*"; optional = [("NM", 'i', "0")]});
  *)

  (* let test_printer_deprecated () = *)
  (*   let transfo = Sam.Transform.raw_to_string () in *)
  (*   let test_line i l = *)
  (*     Tfxm.feed transfo i; *)
  (*     assert_bool l (Tfxm.next transfo = `output (l ^ "\n")) *)
  (*   in *)

  (*   test_line *)
  (*     (`alignment *)
  (*         {Sam.qname = "r001"; flag = 83; rname = "ref"; pos = 37; mapq = 30; *)
  (*          cigar = "9M"; rnext = "="; pnext = 7; tlen = -39; seq = "CAGCGCCAT"; *)
  (*          qual = "*"; optional = [("NM", 'i', "0")]}) *)
  (*     "r001\t83\tref\t37\t30\t9M\t=\t7\t-39\tCAGCGCCAT\t*\tNM:i:0"; *)

  (*   test_line (`comment "some comment") "@CO\tsome comment" ; *)
  (*   test_line (`header ("HD", [ "VN", "1.3"; "SO", "coordinate"])) *)
  (*     "@HD\tVN:1.3\tSO:coordinate"; *)
  (*   () *)

  (* let tests = "SAM" >::: [ *)
  (*     "Parse SAM" >:: test_parser ; *)
  (*     "Parse SAM raw" >:: test_parser_deprecated; *)
  (*     "Print SAM" >:: test_printer_deprecated; *)
  (*     "Parse SAM item" >:: test_item_parser_deprecated; *)
  (*   ] *)
end
