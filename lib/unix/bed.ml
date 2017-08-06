open Core_kernel
open CFStream

type item = string * int * int * Table.Row.t
[@@deriving sexp]

type parsing_spec = [
| `enforce of Table.Row.t_type
| `strings
]
[@@deriving sexp]

module Error = struct

  type parsing_base = [
    | `wrong_format of
        [ `column_number
        | `float_of_string of string
        | `int_of_string of string ] *
          Table.Row.t_type * string
    | `wrong_number_of_columns of Table.Row.t ]
  [@@deriving sexp]

  type parsing = [ `bed of parsing_base ]
  [@@deriving sexp]

  type t = parsing [@@deriving sexp]
end

let item_of_line ~how line =
  let separators = ['\t'; ' '] in
  let format, strict =
    let base = [| `type_string; `type_int; `type_int |] in
    match how with
    | `strings -> (base, false)
    | `enforce tt -> (Array.append base tt, true) in
  Table.Row.of_line ~separators ~format
    ~strict_row_length:strict ~strict_cell_type:strict line
  |> begin function
  | Ok row when Array.length row >= 3 ->
    let n = match row.(0) with `string s -> s | _ -> assert false in
    let h = match row.(1) with `int i -> i | _ -> assert false in
    let l = match row.(2) with `int i -> i | _ -> assert false in
    let q = Array.slice row 3 (Array.length row) in
    Ok (n, h, l, q)
  | Ok row ->
    Error (`bed (`wrong_number_of_columns row))
  | Error e -> Error (`bed e)
  end

let item_to_line (n, l, h, r) =
  Table.Row.to_line ~sep:"\t"
    (Array.append [| `string n; `int l; `int h |] r)


module Transform = struct

  let string_to_item ?(more_columns=`strings) () =
    Tfxm.on_output
      (Lines.Transform.string_to_item ())
      ~f:(fun line -> item_of_line ~how:more_columns line)

  let item_to_string () =
    Tfxm.on_input
      (Lines.Transform.item_to_string ())
      ~f:(fun item -> item_to_line item)

end

exception Error of  Error.t
let error_to_exn e = Error e

let in_channel_to_item_stream ?(buffer_size=65536) ?more_columns inp =
  let x = Transform.string_to_item ?more_columns () in
  Tfxm.(in_channel_strings_to_stream inp x ~buffer_size)

let in_channel_to_item_stream_exn ?buffer_size ?more_columns inp =
  Stream.result_to_exn ~error_to_exn
    (in_channel_to_item_stream ?buffer_size ?more_columns inp)

