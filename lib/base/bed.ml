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

  let parse field_number f s =
    match f s with
    | Ok _ as r -> r
    | Error msg ->
      let msg =
        Printf.sprintf "Field %d is incorrect: %s" field_number msg
      in
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

module Raw_bed5 = struct
  type item = {
    chrom : string ;
    chrom_start : int ;
    chrom_end : int ;
    name : string ;
    score : int ;
    others : string array ;
  }
  [@@deriving sexp]

  let item_of_line l =
    let open Result in
    match Line.split l ~on:'\t' with
    | chrom :: chrom_start :: chrom_end :: name :: score :: others ->
      Field.(parse 2 int) chrom_start >>= fun chrom_start ->
      Field.(parse 3 int) chrom_end >>= fun chrom_end ->
      Field.(parse 5 int) score >>= fun score ->
      Ok { chrom ; chrom_start ; chrom_end ; name ; score ;
           others = Array.of_list others }
    | _ -> Error "Expected at least 5 fields separated by tab characters"
end

module Bed5 = struct
  type item = {
    chrom : string ;
    chrom_start : int ;
    chrom_end : int ;
    name : string ;
    score : int ;
    others : string array ;
  }
  [@@deriving sexp]

  let item_of_line l =
    let open Result in
    match Line.split l ~on:'\t' with
    | chrom :: chrom_start :: chrom_end :: name :: score :: others ->
      Field.(parse 2 positive_int) chrom_start >>= fun chrom_start ->
      Field.(parse 3 positive_int) chrom_end >>= fun chrom_end ->
      Field.(parse 5 (bounded_int ~lo:0 ~hi:1000)) score >>= fun score ->
      Ok { chrom ; chrom_start ; chrom_end ; name ; score ;
           others = Array.of_list others }
    | _ -> Error "Expected at least 5 fields separated by tab characters"

  let line_of_item it =
    it.chrom ::
    Int.to_string it.chrom_start ::
    Int.to_string it.chrom_end ::
    it.name ::
    Int.to_string it.score ::
    (Array.to_list it.others)
    |> String.concat ~sep:"\t"
    |> Line.of_string_unsafe
end
