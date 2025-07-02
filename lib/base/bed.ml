type parser_error = [ `Bed_parser_error of int * string ] [@@deriving sexp]

module Field = Table.Field

type item = string * int * int * string list

let item_of_line l =
  let open Result in
  match Biocaml.Line.split l ~on:'\t' with
  | chrom :: chrom_start :: chrom_end :: others ->
    Field.(parse ~ctx:"chrom_start" int) chrom_start
    >>= fun chrom_start ->
    Field.(parse ~ctx:"chrom_end" int) chrom_end
    >>| fun chrom_end -> chrom, chrom_start, chrom_end, others
  | _ -> Error "Expected at least 3 fields separated by tab characters"
;;

let line_of_item (chr, start_pos, stop_pos, others) =
  String.concat
    ~sep:"\t"
    (chr :: Int.to_string start_pos :: Int.to_string stop_pos :: others)
  |> Biocaml.Line.of_string_unsafe
;;

module Bed3 = struct
  type item =
    { chrom : string
    ; chrom_start : int
    ; chrom_end : int
    ; others : string array
    }
  [@@deriving sexp]
end

module Bed4 = struct
  type item =
    { chrom : string
    ; chrom_start : int
    ; chrom_end : int
    ; name : string
    ; others : string array
    }
  [@@deriving sexp]
end

module Bed5_raw = struct
  type item =
    { chrom : string
    ; chrom_start : int
    ; chrom_end : int
    ; name : string
    ; score : int
    ; others : string list
    }
  [@@deriving sexp]

  let make ~chrom ~chrom_start ~chrom_end ~name ~score ?(others = []) () =
    let open Result in
    Field.(parse ~ctx:"chrom" string_with_no_sep) chrom
    >>= fun chrom ->
    Field.(parse ~ctx:"name" string_with_no_sep) name
    >>= fun name ->
    Field.(parse_all string_with_no_sep) others
    >>= fun others -> Ok { chrom; chrom_start; chrom_end; name; score; others }
  ;;

  let set_score it score = { it with score }

  let item_of_line l =
    let open Result in
    match Biocaml.Line.split l ~on:'\t' with
    | chrom :: chrom_start :: chrom_end :: name :: score :: others ->
      Field.(parse ~ctx:"chrom_start" int) chrom_start
      >>= fun chrom_start ->
      Field.(parse ~ctx:"chrom_end" int) chrom_end
      >>= fun chrom_end ->
      Field.(parse ~ctx:"score" int) score
      >>= fun score ->
      (* don't call [make] to avoid unnecessary checks*)
      Ok { chrom; chrom_start; chrom_end; name; score; others }
    | _ -> Error "Expected at least 5 fields separated by tab characters"
  ;;

  let line_of_item it =
    it.chrom
    :: Int.to_string it.chrom_start
    :: Int.to_string it.chrom_end
    :: it.name
    :: Int.to_string it.score
    :: it.others
    |> String.concat ~sep:"\t"
    |> Biocaml.Line.of_string_unsafe
  ;;
end

module Bed5 = struct
  include Bed5_raw

  let make ~chrom ~chrom_start ~chrom_end ~name ~score ?(others = []) () =
    let open Result in
    Field.(parse ~ctx:"chrom" string_with_no_sep) chrom
    >>= fun chrom ->
    Field.(parse ~ctx:"name" string_with_no_sep) name
    >>= fun name ->
    Field.(parse_all ~ctx:"others" string_with_no_sep) others
    >>= fun others -> Ok { chrom; chrom_start; chrom_end; name; score; others }
  ;;

  let item_of_line l =
    let open Result in
    match Biocaml.Line.split l ~on:'\t' with
    | chrom :: chrom_start :: chrom_end :: name :: score :: others ->
      Field.(parse ~ctx:"chrom_start" positive_int) chrom_start
      >>= fun chrom_start ->
      Field.(parse ~ctx:"chrom_end" positive_int) chrom_end
      >>= fun chrom_end ->
      Field.(parse ~ctx:"score" (bounded_int ~lo:0 ~hi:1000)) score
      >>= fun score ->
      (* don't call [make] to avoid unnecessary checks*)
      Ok { chrom; chrom_start; chrom_end; name; score; others }
    | _ -> Error "Expected at least 5 fields separated by tab characters"
  ;;

  let line_of_item = Bed5_raw.line_of_item
end
