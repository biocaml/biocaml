type t = int [@@deriving sexp]

type offset =
  [ `Offset33
  | `Offset64
  ]
[@@deriving sexp]

let int_of_offset = function
  | `Offset33 -> 33
  | `Offset64 -> 64
;;

let min_as_char = int_of_offset
let max_as_char = 126

let round_float_to_int x =
  if Float.(mod_float x 1.0 < 0.5)
  then Float.(to_int (round_down x))
  else Float.(to_int (round_up x))
;;

let to_int t = t
let to_probability t = Float.(10.0 ** (of_int t / -10.0))

let to_char ?(offset = `Offset33) t =
  let offset' = int_of_offset offset in
  let x = t + offset' in
  if offset' <= x && x <= max_as_char
  then Ok (Char.of_int_exn x)
  else
    Error
      (Error.create
         "cannot convert PHRED score with requested offset to a visible ASCII character"
         (t, offset)
         [%sexp_of: t * offset])
;;

let of_int x =
  if x >= 0 then Ok x else Error (Error.create "invalid PHRED score" x [%sexp_of: int])
;;

let of_char ?(offset = `Offset33) x =
  let offset' = int_of_offset offset in
  let c = Char.to_int x in
  if offset' <= c && c <= max_as_char
  then Ok (c - offset')
  else
    Error
      (Error.create
         "character with given offset is not a valid PHRED score"
         (x, offset)
         [%sexp_of: char * offset])
;;

let of_probability ?(f = round_float_to_int) x =
  if Float.(0.0 < x && x <= 1.0)
  then Ok (f Float.(-10. * log10 x))
  else Error (Error.create "invalid probability" x [%sexp_of: float])
;;

let of_solexa_score ?(f = round_float_to_int) x =
  f Float.(10. * log10 ((10. ** (of_int x / 10.)) +. 1.))
;;

let to_solexa_score ?(f = round_float_to_int) t =
  f Float.(10. * log10 ((10. ** (of_int t / 10.)) -. 1.))
;;

module Test = struct
  let visible_chars = List.range ~stride:1 ~start:`inclusive ~stop:`inclusive 33 126

  let%test _ =
    List.for_all visible_chars ~f:(fun i ->
      let x = i - 33 in
      (* subtract default offset *)
      x
      |> (fun x ->
           Or_error.ok_exn (of_int x)
           |> fun x ->
           Or_error.ok_exn (to_char x) |> fun x -> Or_error.ok_exn (of_char x) |> to_int)
      = x)
  ;;
end
