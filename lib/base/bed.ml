open Base

type parser_error = [ `Bed_parser_error of int * string ]
[@@deriving sexp]

module Field (* field parsing *) = struct
  open Result.Monad_infix

  let exn_validate f x msg =
    try Ok (f x)
    with _ -> Error msg

  let int s =
    exn_validate Int.of_string s "Expected integer value"

  let positive_int s =
    int s >>= fun i ->
    if i >= 0 then Ok i
    else Error "Expected positive integer value"

  let bounded_int ~lo ~hi s =
    int s >>= fun i ->
    if i >= lo && i <= hi then Ok i
    else
      let msg = Printf.sprintf "Expected integer value between %d and %d" lo hi in
      Error msg

  let string_with_no_sep s =
    if String.contains s '\n' || String.contains s '\t' then
      let msg = "Expected string without tab or newline character" in
      Error msg
    else
      Ok s

  let list f xs =
    Result.all (List.map xs ~f)

  let parse field_name f s =
    match f s with
    | Ok _ as r -> r
    | Error msg ->
      let msg = Printf.sprintf "Field %s is incorrect: %s" field_name msg in
      Error msg
end

module Bed3 = struct
  type item = {
    chrom : string ;
    chrom_start : int ;
    chrom_end : int ;
    others : string array ;
  }
  [@@deriving sexp]
end

module Bed4 = struct
  type item = {
    chrom : string ;
    chrom_start : int ;
    chrom_end : int ;
    name : string ;
    others : string array ;
  }
  [@@deriving sexp]

end

module Bed5_raw = struct
  type item = {
    chrom : string ;
    chrom_start : int ;
    chrom_end : int ;
    name : string ;
    score : int ;
    others : string list ;
  }
  [@@deriving sexp]

  let make ~chrom ~chrom_start ~chrom_end ~name ~score ?(others = []) () =
    let open Result in
    Field.(parse "chrom" string_with_no_sep) chrom >>= fun chrom ->
    Field.(parse "name" string_with_no_sep) name >>= fun name ->
    Field.(parse "others" (list string_with_no_sep)) others >>= fun others ->
    Ok { chrom ; chrom_start ; chrom_end ; name ; score ; others }

  let set_score it score = { it with score }

  let item_of_line l =
    let open Result in
    match Line.split l ~on:'\t' with
    | chrom :: chrom_start :: chrom_end :: name :: score :: others ->
      Field.(parse "chrom_start" int) chrom_start >>= fun chrom_start ->
      Field.(parse "chrom_end" int) chrom_end >>= fun chrom_end ->
      Field.(parse "score" int) score >>= fun score ->
      (* don't call [make] to avoid unnecessary checks*)
      Ok { chrom ; chrom_start ; chrom_end ; name ; score ; others }
    | _ -> Error "Expected at least 5 fields separated by tab characters"

  let line_of_item it =
    it.chrom ::
    Int.to_string it.chrom_start ::
    Int.to_string it.chrom_end ::
    it.name ::
    Int.to_string it.score ::
    it.others
    |> String.concat ~sep:"\t"
    |> Line.of_string_unsafe
end

module Bed5 = struct
  include Bed5_raw

  let make ~chrom ~chrom_start ~chrom_end ~name ~score ?(others = []) () =
    let open Result in
    Field.(parse "chrom" string_with_no_sep) chrom >>= fun chrom ->
    Field.(parse "name" string_with_no_sep) name >>= fun name ->
    Field.(parse "others" (list string_with_no_sep)) others >>= fun others ->
    Ok { chrom ; chrom_start ; chrom_end ; name ; score ; others }

  let item_of_line l =
    let open Result in
    match Line.split l ~on:'\t' with
    | chrom :: chrom_start :: chrom_end :: name :: score :: others ->
      Field.(parse "chrom_start" positive_int) chrom_start >>= fun chrom_start ->
      Field.(parse "chrom_end" positive_int) chrom_end >>= fun chrom_end ->
      Field.(parse "score" (bounded_int ~lo:0 ~hi:1000)) score >>= fun score ->
      (* don't call [make] to avoid unnecessary checks*)
      Ok { chrom ; chrom_start ; chrom_end ; name ; score ; others }
    | _ -> Error "Expected at least 5 fields separated by tab characters"

  let line_of_item = Bed5_raw.line_of_item
end
