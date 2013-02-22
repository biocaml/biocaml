open Biocaml_internal_pervasives
open Result

type item = string * int * int * Biocaml_table.Row.t
with sexp

type parsing_spec = [
| `enforce of Biocaml_table.Row.t_type
| `strings
]
with sexp

module Error = struct

  type parsing_base = [
    | `wrong_format of
        [ `column_number
        | `float_of_string of string
        | `int_of_string of string ] *
          Biocaml_table.Row.t_type * string
    | `wrong_number_of_columns of Biocaml_table.Row.t ]
  with sexp

  type parsing = [ `bed of parsing_base ]
  with sexp
end

let item_of_line ~how line =
  let separators = ['\t'; ' '] in
  let format, strict =
    let base = [| `type_string; `type_int; `type_int |] in
    match how with
    | `strings -> (base, false)
    | `enforce tt -> (Array.append base tt, true) in
  Biocaml_table.Row.of_line ~separators ~format ~strict line
  |! begin function
  | Ok row when Array.length row >= 3 ->
    let n = match row.(0) with `string s -> s | _ -> assert false in
    let h = match row.(1) with `int i -> i | _ -> assert false in
    let l = match row.(2) with `int i -> i | _ -> assert false in
    let q = Array.slice row 3 (Array.length row) in
    return (n, h, l, q)
  | Ok row ->
    fail (`bed (`wrong_number_of_columns row))
  | Error e -> fail (`bed e)
  end

let item_to_line (n, l, h, r) =
  Biocaml_table.Row.to_line ~sep:"\t"
    (Array.append [| `string n; `int l; `int h |] r)


module Transform = struct

  let string_to_item ?(more_columns=`strings) () =
    Biocaml_transform.on_output
      (Biocaml_lines.Transform.string_to_item ())
      ~f:(fun line -> item_of_line more_columns line)

  let item_to_string () =
    Biocaml_transform.on_input
      (Biocaml_lines.Transform.item_to_string ())
      ~f:(fun item -> item_to_line item)

end
