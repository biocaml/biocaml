open Core_kernel

module Row = struct

  let module_error e = Error (`table_row e)


  type item_type = [`type_int | `type_float | `type_string ] [@@deriving sexp]
  type t_type = item_type array [@@deriving sexp]

  type item = [`int of int | `float of float | `string of string ] [@@deriving sexp]
  type t = item array [@@deriving sexp]

  module Tags = struct

    type t = [
      | `separator of char
      | `strict_about of [ `row_length | `cell_type ]
      | `format of item_type array
    ] list
    [@@deriving sexp]

    let separators tags =
      List.filter_map tags ~f:(function
        | `separator c -> Some c
        | _ -> None)

    let strict_row_length tags =
      List.exists tags ~f:(function `strict_about `row_length -> true | _ -> false)

    let strict_cell_type tags =
      List.exists tags ~f:(function `strict_about `cell_type -> true | _ -> false)

    let format tags =
      List.find_map tags ~f:(function `format f -> Some f | _ -> None)

    let default = [ `separator '\t' ]

    let default_extension tags =
      match separators tags with
      | '\t' :: _ -> "tsv"
      | ',' :: _ -> "csv"
      | _ -> "table"

    let to_string t = sexp_of_t t |> Sexplib.Sexp.to_string
    let of_string s =
      try Ok (Sexplib.Sexp.of_string s |> t_of_sexp)
      with e -> module_error (`tags_of_string e)

  end

  module Error = struct

    type line_parsing =
      [ `wrong_format of
          [ `column_number
          | `float_of_string of string
          | `int_of_string of string ] * t_type * string ]
    [@@deriving sexp]

    type t = line_parsing [@@deriving sexp]

  end

  let of_line ?(separators=[' '; '\t']) ?(strict_row_length=false)
      ?(strict_cell_type=false) ?format line =
    let l = (line : Line.t :> string) in
    let module With_exns = struct
      exception Int_of_string of string
      exception Float_of_string of string
      let int s =
        try Int.of_string s with _ -> raise (Int_of_string s)
      let float s =
        try Float.of_string s with _ -> raise (Float_of_string s)
      let of_line ~format l =
        let tokens =
          String.split_on_chars ~on:separators l |> List.filter ~f:((<>) "")
          |> Array.of_list in
        begin match format with
        | None ->
          Ok (Array.map tokens ~f:(fun s -> `string s))
        | Some format ->
          begin try
            if strict_row_length && Array.length format > Array.length tokens
            then Error (`wrong_format (`column_number, format, l))
            else begin
              let row =
                Array.mapi tokens ~f:(fun i tok ->
                  let typ =
                    if strict_cell_type then format.(i)
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
    Line.of_string_unsafe
      (String.concat_array ~sep (Array.map t ~f:item_to_string))


  module Transform = struct

    let line_to_item ?(tags: Tags.t = Tags.default) () =
      let separators = Tags.separators tags in
      let strict_row_length = Tags.strict_row_length tags in
      let strict_cell_type = Tags.strict_cell_type tags in
      let format = Tags.format tags in
      Tfxm.on_output
        ~f:begin fun s ->
          of_line ~separators ~strict_row_length ~strict_cell_type ?format
            (s : Lines.item)
          |> begin function
          | Ok o -> Ok o
          | Error e -> Error (`table_row (e : Error.line_parsing))
          end
        end
        (Tfxm.identity ())

    let item_to_line  ?(tags: Tags.t = Tags.default) () =
      let sep =
        Tags.separators tags |> List.hd |> Option.value ~default:'\t'
        |> Char.to_string in
      Tfxm.on_output ~f:(to_line ~sep)
        (Tfxm.identity ())

  end

end
