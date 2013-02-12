

open Biocaml_internal_pervasives


module Row = struct
  type item_type = [`type_int | `type_float | `type_string ]
  type t_type = item_type array

  type item = [`int of int | `float of float | `string of string ]
  type t = item array

  let of_line ?(separators=[' '; '\t']) ~format line =
    let l = (line : Biocaml_line.t :> string) in
    let module With_exns = struct
      exception Int_of_string of string
      exception Float_of_string of string
      let int s =
        try Int.of_string s with e -> raise (Int_of_string s)
      let float s =
        try Float.of_string s with e -> raise (Float_of_string s)
      let of_line ~format l =
        let tokens =
          String.split_on_chars ~on:separators l |! List.filter ~f:((<>) "")
          |! Array.of_list in
        begin try
          let row =
            Array.map2_exn format tokens ~f:(fun typ tok ->
              match typ with
              | `type_int -> `int (int tok)
              | `type_float -> `float (float tok)
              | `type_string -> `string tok) in
          Ok row
        with
        | Invalid_argument _ (* should be map2 *) ->
          Error (`wrong_format (`column_number, format, l))
        | Int_of_string s ->
          Error (`wrong_format (`int_of_string s, format, l))
        | Float_of_string s ->
          Error (`wrong_format (`float_of_string s, format, l))
        end
    end in
    (With_exns.of_line ~format l : (t, _) Result.t)

end
