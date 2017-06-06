open Core_kernel

type t = int
[@@deriving sexp]

type offset = [`Offset33 | `Offset64]
[@@deriving sexp]

let int_of_offset = function `Offset33 -> 33 | `Offset64 -> 64

let min_as_char = int_of_offset
let max_as_char = 126

let round_float_to_int x =
  if Float.mod_float x 1.0 < 0.5
  then Float.(to_int (round_down x))
  else Float.(to_int (round_up x))

let to_int t = t

let to_probability t =
  10.0 ** (Float.of_int t /. -10.0)

let to_char ?(offset=`Offset33) t =
  let offset' = int_of_offset offset in
  let x = t + offset' in
  if offset' <= x && x <= max_as_char then
    Ok (Char.of_int_exn x)
  else
    error
      "cannot convert PHRED score with requested offset to a \
       visible ASCII character"
      (t, offset)
      [%sexp_of: t * offset ]

let of_int x =
  if x >= 0 then Ok x
  else error "invalid PHRED score" x sexp_of_int

let of_char ?(offset=`Offset33) x =
  let offset' = int_of_offset offset in
  let c = Char.to_int x in
  if offset' <= c && c <= max_as_char then
    Ok (c - offset')
  else
    error
      "character with given offset is not a valid PHRED score"
      (x, offset)
      [%sexp_of: char * offset ]

let of_probability ?(f = round_float_to_int) x =
  if 0.0 < x && x <= 1.0 then
    Ok (f (-10. *. Float.log10 x))
  else
    error "invalid probability" x sexp_of_float

let of_solexa_score ?(f = round_float_to_int) x =
  f (10. *. Float.log10((10. ** (Float.of_int x /. 10.)) +. 1.))

let to_solexa_score ?(f = round_float_to_int) t =
  f (10. *. Float.log10((10. ** (Float.of_int t /. 10.)) -. 1.))
