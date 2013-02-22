

open Biocaml_internal_pervasives


module Row = struct
  type item_type = [`type_int | `type_float | `type_string ] with sexp
  type t_type = item_type array with sexp

  type item = [`int of int | `float of float | `string of string ] with sexp
  type t = item array with sexp

  let of_line ?(separators=[' '; '\t']) ?(strict=false) ?format line =
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
        begin match format with
        | None ->
          Ok (Array.map tokens ~f:(fun s -> `string s))
        | Some format ->
          begin try
            if strict && Array.length format > Array.length tokens
            then Error (`wrong_format (`column_number, format, l))
            else begin
              let row =
                Array.mapi tokens ~f:(fun i tok ->
                  let typ =
                    if strict then format.(i)
                    else (try format.(i) with _ -> `type_string) in
                  match typ with
                | `type_int -> `int (int tok)
                | `type_float -> `float (float tok)
                | `type_string -> `string tok) in
              Ok row
            end
          with
          | Invalid_argument _ (* should be the array access *) ->
            Error (`wrong_format (`column_number, format, l))
          | Int_of_string s ->
            Error (`wrong_format (`int_of_string s, format, l))
          | Float_of_string s ->
            Error (`wrong_format (`float_of_string s, format, l))
          end
        end
    end in
    (With_exns.of_line ~format l : (t, _) Result.t)

  let to_line ~sep t =
    let item_to_string = function
      | `int i -> Int.to_string i
      | `float f -> sprintf "%g" f
      | `string s -> s in
    Biocaml_line.of_string_unsafe
      (String.concat_array ~sep (Array.map t ~f:item_to_string))
end
