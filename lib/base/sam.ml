open! Import
open Result.Monad_infix

let ( >>?~ ) (x : 'a option Or_error.t) (f : 'a -> 'b Or_error.t) : 'b option Or_error.t =
  let open Result.Monad_infix in
  x
  >>= function
  | None -> Ok None
  | Some x -> f x >>| Option.some
;;

(******************************************************************************)
(* Header Types                                                               *)
(******************************************************************************)

module Header = struct
  module Header_item_tag = struct
    type t =
      [ `HD
      | `SQ
      | `RG
      | `PG
      | `CO
      | `Other of string
      ]
    [@@deriving sexp]

    let print_header_item_tag = function
      | `HD -> "@HD"
      | `SQ -> "@SQ"
      | `RG -> "@RG"
      | `PG -> "@PG"
      | `CO -> "@CO"
      | `Other x -> sprintf "@%s" x
    ;;

    let parse_header_item_tag s =
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
           [%sexp_of: Header_item_tag.t * string])
    | y :: [] -> Ok y
    | ys ->
      Error
        (Error.create
           "tag found multiple times"
           (header_item_tag, x, ys)
           [%sexp_of: Header_item_tag.t * string * string list])
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
           [%sexp_of: Header_item_tag.t * string * string list])
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
           [%sexp_of: Header_item_tag.t * Set.M(String).t])
  ;;

  module Tag_value = struct
    type t = string * string [@@deriving sexp]

    let parse_tag_value s =
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

    let print_tag_value (tag, value) = sprintf "%s:%s" tag value
    let print_tag_value' = sprintf "%s:%s"
  end

  module Sort_order = struct
    type t =
      [ `Unknown
      | `Unsorted
      | `Query_name
      | `Coordinate
      ]
    [@@deriving sexp]

    let parse_sort_order = function
      | "unknown" -> Ok `Unknown
      | "unsorted" -> Ok `Unsorted
      | "queryname" -> Ok `Query_name
      | "coordinate" -> Ok `Coordinate
      | x -> Error (Error.create "invalid sort order" x sexp_of_string)
    ;;

    let print_sort_order x =
      Tag_value.print_tag_value'
        "SO"
        (match x with
         | `Unknown -> "unknown"
         | `Unsorted -> "unsorted"
         | `Query_name -> "queryname"
         | `Coordinate -> "coordinate")
    ;;
  end

  module Group_order = struct
    type t =
      [ `None
      | `Query
      | `Reference
      ]
    [@@deriving sexp]

    let parse_group_order = function
      | "none" -> Ok `None
      | "query" -> Ok `Query
      | "reference" -> Ok `Reference
      | x -> Error (Error.create "invalid group order" x sexp_of_string)
    ;;

    let print_group_order x =
      Tag_value.print_tag_value'
        "GO"
        (match x with
         | `None -> "none"
         | `Query -> "query"
         | `Reference -> "reference")
    ;;
  end

  let parse_header_version s =
    let err =
      Error
        (Error.create "invalid version" (`HD, s) [%sexp_of: Header_item_tag.t * string])
    in
    match String.lsplit2 ~on:'.' s with
    | None -> err
    | Some (a, b) ->
      if String.for_all a ~f:Char.is_digit && String.for_all b ~f:Char.is_digit
      then Ok s
      else err
  ;;

  let print_header_version x = Tag_value.print_tag_value' "VN" x

  module Header_line = struct
    type t =
      { version : string
      ; sort_order : Sort_order.t option
      ; group_order : Group_order.t option
      }
    [@@deriving sexp]

    let header_line ~version ?sort_order ?group_order () =
      parse_header_version version >>| fun version -> { version; sort_order; group_order }
    ;;

    let parse_header_line tvl =
      find1 `HD tvl "VN"
      >>= fun version ->
      find01 `HD tvl "SO"
      >>?~ Sort_order.parse_sort_order
      >>= fun sort_order ->
      find01 `HD tvl "GO"
      >>?~ Group_order.parse_group_order
      >>= fun group_order ->
      assert_tags `HD tvl [ "VN"; "SO"; "GO" ]
      >>= fun () -> header_line ~version ?sort_order ?group_order ()
    ;;

    let print_header_line { version; sort_order; group_order } =
      sprintf
        "@HD\tVN:%s%s%s"
        version
        (match sort_order with
         | None -> ""
         | Some x -> sprintf "\t%s" (Sort_order.print_sort_order x))
        (match group_order with
         | None -> ""
         | Some x -> sprintf "\t%s" (Group_order.print_group_order x))
    ;;
  end

  module Ref_seq = struct
    type t =
      { name : string
      ; length : int
      ; assembly : string option
      ; md5 : string option
      ; species : string option
      ; uri : string option
      }
    [@@deriving sexp]

    let ref_seq ~name ~length ?assembly ?md5 ?species ?uri () =
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

    let parse_ref_seq tvl =
      find1 `SQ tvl "SN"
      >>= fun name ->
      find1 `SQ tvl "LN"
      >>= fun length ->
      (try Ok (Int.of_string length) with
       | _ -> Error (Error.create "invalid ref seq length" length sexp_of_string))
      >>= fun length ->
      find01 `SQ tvl "AS"
      >>= fun assembly ->
      find01 `SQ tvl "M5"
      >>= fun md5 ->
      find01 `SQ tvl "SP"
      >>= fun species ->
      find01 `SQ tvl "UR"
      >>= fun uri ->
      assert_tags `SQ tvl [ "SN"; "LN"; "AS"; "M5"; "SP"; "UR" ]
      >>= fun () -> ref_seq ~name ~length ?assembly ?md5 ?species ?uri ()
    ;;

    let print_ref_seq (x : t) =
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

  module Platform = struct
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

    let parse_platform = function
      | "CAPILLARY" -> Ok `Capillary
      | "LS454" -> Ok `LS454
      | "ILLUMINA" -> Ok `Illumina
      | "SOLID" -> Ok `Solid
      | "HELICOS" -> Ok `Helicos
      | "IONTORRENT" -> Ok `Ion_Torrent
      | "PACBIO" -> Ok `Pac_Bio
      | x -> Error (Error.create "unknown platform" x sexp_of_string)
    ;;

    let print_platform = function
      | `Capillary -> "CAPILLARY"
      | `LS454 -> "LS454"
      | `Illumina -> "ILLUMINA"
      | `Solid -> "SOLID"
      | `Helicos -> "HELICOS"
      | `Ion_Torrent -> "IONTORRENT"
      | `Pac_Bio -> "PACBIO"
    ;;
  end

  module Read_group = struct
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
      ; platform : Platform.t option
      ; platform_unit : string option
      ; sample : string option
      }
    [@@deriving sexp]

    let read_group
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

    let parse_read_group tvl =
      find1 `RG tvl "ID"
      >>= fun id ->
      find01 `RG tvl "CN"
      >>= fun seq_center ->
      find01 `RG tvl "DS"
      >>= fun description ->
      find01 `RG tvl "DT"
      >>= fun run_date ->
      find01 `RG tvl "FO"
      >>= fun flow_order ->
      find01 `RG tvl "KS"
      >>= fun key_seq ->
      find01 `RG tvl "LB"
      >>= fun library ->
      find01 `RG tvl "PG"
      >>= fun program ->
      find01 `RG tvl "PI"
      >>?~ (fun predicted_median_insert_size ->
      try Ok (Int.of_string predicted_median_insert_size) with
      | _ ->
        Error
          (Error.create
             "invalid predicted median insert size"
             predicted_median_insert_size
             sexp_of_string))
      >>= fun predicted_median_insert_size ->
      find01 `RG tvl "PL"
      >>?~ Platform.parse_platform
      >>= fun platform ->
      find01 `RG tvl "PU"
      >>= fun platform_unit ->
      find01 `RG tvl "SM"
      >>= fun sample ->
      assert_tags
        `RG
        tvl
        [ "ID"; "CN"; "DS"; "DT"; "FO"; "KS"; "LB"; "PG"; "PI"; "PL"; "PU"; "SM" ]
      >>= fun () ->
      read_group
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

    let print_read_group (x : t) =
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
        (s "PL" (Option.map x.platform ~f:Platform.print_platform))
        (s "PU" x.platform_unit)
        (s "SM" x.sample)
    ;;
  end

  module Program = struct
    type t =
      { id : string
      ; name : string option
      ; command_line : string option
      ; previous_id : string option
      ; description : string option
      ; version : string option
      }
    [@@deriving sexp]

    let parse_program tvl =
      find1 `PG tvl "ID"
      >>= fun id ->
      find01 `PG tvl "PN"
      >>= fun name ->
      find01 `PG tvl "CL"
      >>= fun command_line ->
      find01 `PG tvl "PP"
      >>= fun previous_id ->
      find01 `PG tvl "DS"
      >>= fun description ->
      find01 `PG tvl "VN"
      >>= fun version ->
      assert_tags `PG tvl [ "ID"; "PN"; "CL"; "PP"; "DS"; "VN" ]
      >>| fun () -> { id; name; command_line; previous_id; description; version }
    ;;

    let print_program (x : t) =
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

  module Header_item = struct
    type t =
      [ `HD of Header_line.t
      | `SQ of Ref_seq.t
      | `RG of Read_group.t
      | `PG of Program.t
      | `CO of string
      | `Other of string * Tag_value.t list
      ]
    [@@deriving sexp]

    let parse_header_item line =
      let parse_data tag tvl =
        match tag with
        | `HD -> Header_line.parse_header_line tvl >>| fun x -> `HD x
        | `SQ -> Ref_seq.parse_ref_seq tvl >>| fun x -> `SQ x
        | `RG -> Read_group.parse_read_group tvl >>| fun x -> `RG x
        | `PG -> Program.parse_program tvl >>| fun x -> `PG x
        | `Other tag -> Ok (`Other (tag, tvl))
        | `CO -> assert false
      in
      match String.lsplit2 ~on:'\t' (line : Line.t :> string) with
      | None -> Error (Error.create "header line contains no tabs" line Line.sexp_of_t)
      | Some (tag, data) -> (
        Header_item_tag.parse_header_item_tag tag
        >>= function
        | `CO -> Ok (`CO data)
        | tag -> (
          match String.split ~on:'\t' data with
          | [] -> assert false
          | "" :: [] ->
            Error (Error.create "header contains no data" tag Header_item_tag.sexp_of_t)
          | tvl ->
            Result_list.map tvl ~f:Tag_value.parse_tag_value
            >>= fun tvl -> parse_data tag tvl))
    ;;
  end

  (** TODO(ashish): This function appears to be unused. Should we delete it? *)
  let print_other ((tag, l) : string * Tag_value.t list) =
    sprintf
      "@%s%s"
      tag
      (List.map l ~f:(fun (x, y) -> sprintf "\t%s:%s" x y) |> String.concat ~sep:"")
  ;;

  type t =
    { version : string option
    ; sort_order : Sort_order.t option
    ; group_order : Group_order.t option
    ; ref_seqs : Ref_seq.t list
    ; read_groups : Read_group.t list
    ; programs : Program.t list
    ; comments : string list
    ; others : (string * Tag_value.t list) list
    }

  let empty_header =
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

  let header
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
         match parse_header_version x with
         | Error e -> Some e
         | Ok _ -> None))
    ; (if Option.is_some sort_order && Poly.(version = None)
       then
         Some
           (Error.create
              "sort order cannot be defined without version"
              (sort_order, version)
              [%sexp_of: Sort_order.t option * string option])
       else None)
    ; List.map ref_seqs ~f:(fun (x : Ref_seq.t) -> x.name)
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
end

(******************************************************************************)
(* Alignment Types                                                            *)
(******************************************************************************)
module Flags = struct
  type t = int [@@deriving sexp]

  let of_int x =
    if 0 <= x && x <= 65535
    then Ok x
    else Error (Error.create "flag out of range" x sexp_of_int)
  ;;

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

module Cigar_op = struct
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
              | 'M' -> cigar_op_alignment_match n
              | 'I' -> cigar_op_insertion n
              | 'D' -> cigar_op_deletion n
              | 'N' -> cigar_op_skipped n
              | 'S' -> cigar_op_soft_clipping n
              | 'H' -> cigar_op_hard_clipping n
              | 'P' -> cigar_op_padding n
              | '=' -> cigar_op_seq_match n
              | 'X' -> cigar_op_seq_mismatch n
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
  ;;

  let print_cigar = function
    | [] -> "*"
    | cigar_ops -> List.map cigar_ops ~f:print_cigar_op |> String.concat ~sep:""
  ;;
end

module Optional_field_value = struct
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
  let opt_field_float_re = Re.Perl.compile_pat "^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$"

  let optional_field_value_err typ value =
    Error (Error.create "invalid value" (typ, value) [%sexp_of: string * string])
  ;;

  let optional_field_value_A = function
    | '!' .. '~' as value -> Ok (`A value)
    | c -> optional_field_value_err "A" (sprintf "char code %d" (Char.to_int c))
  ;;

  let optional_field_value_i i = `i i
  let optional_field_value_f f = `f f

  let optional_field_value_Z value =
    if Re.execp opt_field_Z_re value
    then Ok (`Z value)
    else optional_field_value_err "Z" value
  ;;

  let optional_field_value_H value =
    if Re.execp opt_field_H_re value
    then Ok (`H value)
    else optional_field_value_err "H" value
  ;;

  let optional_field_value_B elt_type elts =
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

  let parse_optional_field_value s =
    match String.lsplit2 s ~on:':' with
    | None -> Error (Error.create "missing TYPE in optional field" s sexp_of_string)
    | Some (typ, value) -> (
      match typ with
      | "A" ->
        if String.length value = 1
        then optional_field_value_A value.[0]
        else optional_field_value_err typ value
      | "i" -> (
        try
          if not (Re.execp opt_field_int_re value) then failwith "";
          Ok (optional_field_value_i (Int64.of_string value))
          (* matching the regular expression is not enough: the number could not fit in 64 bits *)
        with
        | _ -> optional_field_value_err typ value)
      | "f" -> (
        try
          if not (Re.execp opt_field_float_re value) then failwith "";
          Ok (optional_field_value_f (Float.of_string value))
          (* matching the regular expression is not enough: the number could not fit in native floats *)
        with
        | _ -> optional_field_value_err typ value)
      | "Z" -> optional_field_value_Z value
      | "H" -> optional_field_value_H value
      | "B" -> (
        match String.split ~on:',' value with
        | num_typ :: values ->
          if String.length num_typ = 1
          then optional_field_value_B num_typ.[0] values
          else Error (Error.create "invalid array type" num_typ sexp_of_string)
        | _ -> assert false (* [String.split] cannot return an empty list *))
      | _ -> Error (Error.create "invalid type" typ sexp_of_string))
  ;;
end

module Optional_field = struct
  type t =
    { tag : string
    ; value : Optional_field_value.t
    }
  [@@deriving sexp]

  let opt_field_tag_re = Re.Perl.compile_pat "^[A-Za-z][A-Za-z0-9]$"

  let optional_field tag value =
    if not (Re.execp opt_field_tag_re tag)
    then Error (Error.create "invalid TAG" tag sexp_of_string)
    else Ok { tag; value }
  ;;

  let parse_optional_field s =
    match String.lsplit2 s ~on:':' with
    | None -> Error (Error.create "missing TAG in optional field" s sexp_of_string)
    | Some (tag, s) ->
      Optional_field_value.parse_optional_field_value s
      >>= fun value -> optional_field tag value
  ;;

  let print_optional_field (x : t) =
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
      let f : t Or_error.t = parse_optional_field s in
      Stdlib.Printf.printf
        "Optional field value (i type): %s = %s: %b\n"
        (f |> Or_error.sexp_of_t sexp_of_t |> Sexp.to_string_hum)
        (v |> Or_error.sexp_of_t sexp_of_t |> Sexp.to_string_hum)
        (Or_error.equal Poly.equal f v)
    ;;

    let%expect_test "test_parser" =
      test_parse_optional_field
        "YS:i:-1"
        (optional_field "YS" (Optional_field_value.optional_field_value_i (-1L)));
      [%expect
        {| Optional field value (i type): (Ok ((tag YS) (value (i -1)))) = (Ok ((tag YS) (value (i -1)))): true |}]
    ;;
  end
end

module Rnext = struct
  type t =
    [ `Value of string
    | `Equal_to_RNAME
    ]
  [@@deriving sexp]

  let rnext_re = Re.Perl.compile_pat "^\\*|=|[!-()+-<>-~][!-~]*$"

  let parse_rnext s =
    if not (Re.execp rnext_re s)
    then Error (Error.create "invalid RNEXT" s sexp_of_string)
    else (
      match s with
      | "*" -> Ok None
      | "=" -> Ok (Some `Equal_to_RNAME)
      | _ -> Ok (Some (`Value s)))
  ;;

  let print_rnext = function
    | None -> "*"
    | Some `Equal_to_RNAME -> "="
    | Some (`Value x) -> x
  ;;
end

module Alignment = struct
  type t =
    { qname : string option
    ; flags : Flags.t
    ; rname : string option
    ; pos : int option
    ; mapq : int option
    ; cigar : Cigar_op.t list
    ; rnext : Rnext.t option
    ; pnext : int option
    ; tlen : int option
    ; seq : string option
    ; qual : Phred_score.t list
    ; optional_fields : Optional_field.t list
    }
  [@@deriving sexp]

  let alignment
        ?ref_seqs
        ?qname
        ~flags
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
        ; flags
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

  let qname_re =
    let open Re in
    alt [ char '*'; repn (alt [ rg '!' '?'; rg 'A' '~' ]) 1 (Some 255) ] |> compile
  ;;

  let parse_qname s = parse_opt_string "QNAME" qname_re s

  let parse_flags s =
    try Flags.of_int (Int.of_string s) with
    | _ -> Error (Error.create "invalid FLAG" s sexp_of_string)
  ;;

  let rname_re = Re.Perl.compile_pat "^\\*|[!-()+-<>-~][!-~]*$"
  let parse_rname s = parse_opt_string "RNAME" rname_re s

  let parse_pos s =
    parse_int_range "POS" 0 2147483647 s
    >>| function
    | 0 -> None
    | x -> Some x
  ;;

  let parse_mapq s =
    parse_int_range "MAPQ" 0 255 s
    >>| function
    | 255 -> None
    | x -> Some x
  ;;

  let parse_pnext s =
    parse_int_range "PNEXT" 0 2147483647 s
    >>| function
    | 0 -> None
    | x -> Some x
  ;;

  let parse_tlen s =
    parse_int_range "TLEN" ~-2147483647 2147483647 s
    >>| function
    | 0 -> None
    | x -> Some x
  ;;

  let seq_re = Re.Perl.compile_pat "^\\*|[A-Za-z=.]+$"
  let parse_seq s = parse_opt_string "SEQ" seq_re s

  let parse_qual s =
    match s with
    | "" -> Or_error.error_string "invalid empty QUAL"
    | "*" -> Ok []
    | _ -> String.to_list s |> Result_list.map ~f:(Phred_score.of_char ~offset:`Offset33)
  ;;

  let parse_alignment ?ref_seqs line =
    match String.split ~on:'\t' (line : Line.t :> string) with
    | qname
      :: flags
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
      parse_qname qname
      >>= fun qname ->
      parse_flags flags
      >>= fun flags ->
      parse_rname rname
      >>= fun rname ->
      parse_pos pos
      >>= fun pos ->
      parse_mapq mapq
      >>= fun mapq ->
      Cigar_op.parse_cigar cigar
      >>= fun cigar ->
      Rnext.parse_rnext rnext
      >>= fun rnext ->
      parse_pnext pnext
      >>= fun pnext ->
      parse_tlen tlen
      >>= fun tlen ->
      parse_seq seq
      >>= fun seq ->
      parse_qual qual
      >>= fun qual ->
      Result_list.map optional_fields ~f:Optional_field.parse_optional_field
      >>= fun optional_fields ->
      alignment
        ?ref_seqs
        ?qname
        ~flags
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

  let print_qname = function
    | Some x -> x
    | None -> "*"
  ;;

  let print_flags = Int.to_string

  let print_rname = function
    | Some x -> x
    | None -> "*"
  ;;

  let print_pos = function
    | Some x -> Int.to_string x
    | None -> "0"
  ;;

  let print_mapq = function
    | Some x -> Int.to_string x
    | None -> "255"
  ;;

  let print_pnext = function
    | Some x -> Int.to_string x
    | None -> "0"
  ;;

  let print_tlen = function
    | Some x -> Int.to_string x
    | None -> "0"
  ;;

  let print_seq = function
    | Some x -> x
    | None -> "*"
  ;;

  let print_qual = function
    | [] -> "*"
    | quals ->
      List.map quals ~f:(fun x ->
        Or_error.ok_exn (Phred_score.to_char ~offset:`Offset33 x))
      |> String.of_char_list
  ;;

  let print_alignment a =
    sprintf
      "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"
      (print_qname a.qname)
      (print_flags a.flags)
      (print_rname a.rname)
      (print_pos a.pos)
      (print_mapq a.mapq)
      (Cigar_op.print_cigar a.cigar)
      (Rnext.print_rnext a.rnext)
      (print_pnext a.pnext)
      (print_tlen a.tlen)
      (print_seq a.seq)
      (print_qual a.qual)
      (List.map a.optional_fields ~f:Optional_field.print_optional_field
       |> String.concat ~sep:"\t")
  ;;
end

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
  (*         {Sam.query_template_name = "r001"; flags = Sam.Flags.of_int 83; *)
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
  (*                      {Sam.query_template_name = "chr0"; flags = Sam.Flags.of_int 83; *)
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
