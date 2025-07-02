module Row = struct
  let module_error e = Error (`table_row e)

  type item_type =
    [ `type_int
    | `type_float
    | `type_string
    ]
  [@@deriving sexp]

  type t_type = item_type array [@@deriving sexp]

  type item =
    [ `int of int
    | `float of float
    | `string of string
    ]
  [@@deriving sexp]

  type t = item array [@@deriving sexp]

  module Tags = struct
    type t =
      [ `separator of char
      | `strict_about of [ `row_length | `cell_type ]
      | `format of item_type array
      ]
      list
    [@@deriving sexp]

    let separators tags =
      List.filter_map tags ~f:(function
        | `separator c -> Some c
        | _ -> None)
    ;;

    let strict_row_length tags =
      List.exists tags ~f:(function
        | `strict_about `row_length -> true
        | _ -> false)
    ;;

    let strict_cell_type tags =
      List.exists tags ~f:(function
        | `strict_about `cell_type -> true
        | _ -> false)
    ;;

    let format tags =
      List.find_map tags ~f:(function
        | `format f -> Some f
        | _ -> None)
    ;;

    let default = [ `separator '\t' ]

    let default_extension tags =
      match separators tags with
      | '\t' :: _ -> "tsv"
      | ',' :: _ -> "csv"
      | _ -> "table"
    ;;

    let to_string t = sexp_of_t t |> Sexplib.Sexp.to_string

    let of_string s =
      try Ok (Sexplib.Sexp.of_string s |> t_of_sexp) with
      | e -> module_error (`tags_of_string e)
    ;;
  end

  module Error = struct
    type line_parsing =
      [ `wrong_format of
        [ `column_number | `float_of_string of string | `int_of_string of string ]
        * t_type
        * string
      ]
    [@@deriving sexp]

    type t = line_parsing [@@deriving sexp]
  end

  let of_line
    ?(separators = [ ' '; '\t' ])
    ?(strict_row_length = false)
    ?(strict_cell_type = false)
    ?format
    line
    =
    let l = (line : Biocaml.Line.t :> string) in
    let module With_exns = struct
      exception Int_of_string of string
      exception Float_of_string of string

      let int s =
        try Int.of_string s with
        | _ -> raise (Int_of_string s)
      ;;

      let float s =
        try Float.of_string s with
        | _ -> raise (Float_of_string s)
      ;;

      let of_line ~format l =
        let tokens =
          String.split_on_chars ~on:separators l
          |> List.filter ~f:String.(( <> ) "")
          |> Array.of_list
        in
        match format with
        | None -> Ok (Array.map tokens ~f:(fun s -> `string s))
        | Some format -> (
          try
            if strict_row_length && Array.length format > Array.length tokens
            then Error (`wrong_format (`column_number, format, l))
            else (
              let row =
                Array.mapi tokens ~f:(fun i tok ->
                  let typ =
                    if strict_cell_type
                    then format.(i)
                    else (
                      try format.(i) with
                      | _ -> `type_string)
                  in
                  match typ with
                  | `type_int -> `int (int tok)
                  | `type_float -> `float (float tok)
                  | `type_string -> `string tok)
              in
              Ok row)
          with
          | Invalid_argument _ (* should be the array access *) ->
            Error (`wrong_format (`column_number, format, l))
          | Int_of_string s -> Error (`wrong_format (`int_of_string s, format, l))
          | Float_of_string s -> Error (`wrong_format (`float_of_string s, format, l)))
      ;;
    end
    in
    (With_exns.of_line ~format l : (t, _) Result.t)
  ;;

  let to_line ~sep t =
    let item_to_string = function
      | `int i -> Int.to_string i
      | `float f -> sprintf "%g" f
      | `string s -> s
    in
    Biocaml.Line.of_string_unsafe
      (String.concat_array ~sep (Array.map t ~f:item_to_string))
  ;;

  module Transform = struct
    let line_to_item ?(tags : Tags.t = Tags.default) () =
      let separators = Tags.separators tags in
      let strict_row_length = Tags.strict_row_length tags in
      let strict_cell_type = Tags.strict_cell_type tags in
      let format = Tags.format tags in
      Tfxm.on_output
        ~f:(fun s ->
          of_line
            ~separators
            ~strict_row_length
            ~strict_cell_type
            ?format
            (s : Lines.item)
          |> function
          | Ok o -> Ok o
          | Error e -> Error (`table_row (e : Error.line_parsing)))
        (Tfxm.identity ())
    ;;

    let item_to_line ?(tags : Tags.t = Tags.default) () =
      let sep =
        Tags.separators tags |> List.hd |> Option.value ~default:'\t' |> Char.to_string
      in
      Tfxm.on_output ~f:(to_line ~sep) (Tfxm.identity ())
    ;;
  end
end

module Test = struct
  let ( >>< ) x f = x |> f

  let%expect_test _ =
    let tdash ?to_line s r =
      let row_result =
        Row.of_line
          ~separators:[ '-' ]
          ~format:[| `type_int; `type_float; `type_string |]
          (Biocaml.Line.of_string_unsafe s)
      in
      printf "%s: %b\n" (sprintf "%s: of_line" s) (Poly.( = ) r row_result);
      Result.iter row_result ~f:(fun row ->
        match to_line with
        | Some str ->
          printf
            "%s: %b\n"
            (sprintf "%s: to_line" s)
            (String.equal (Row.to_line ~sep:"-" row : Biocaml.Line.t :> string) str)
        | None ->
          printf
            "%s: %b\n"
            (sprintf "%s: to_line" s)
            (String.equal (Row.to_line ~sep:"-" row : Biocaml.Line.t :> string) s)
        (* row = Biocaml.Line.of_string_unsafe s) *))
    in
    tdash "42-42-42" (Ok [| `int 42; `float 42.; `string "42" |]);
    tdash "42-42.-42" (Ok [| `int 42; `float 42.; `string "42" |]) ~to_line:"42-42-42";
    tdash
      "42-42.-42-any-string-42"
      (Ok
         [| `int 42
          ; `float 42.
          ; `string "42"
          ; `string "any"
          ; `string "string"
          ; `string "42"
         |])
      ~to_line:"42-42-42-any-string-42";
    let fail_test fmt = ksprintf (fun s -> printf "%s: %b\n" s false) fmt in
    let test_tol = Row.of_line ~separators:[ ' ' ] ~format:[| `type_int; `type_float |] in
    let line = Biocaml.Line.of_string_unsafe in
    (test_tol (line "42 42")
    >>< function
    | Ok [| `int 42; `float 42. |] -> ()
    | _ -> fail_test "wrong parsing of 42 42");
    (test_tol (line "42")
    >>< function
    | Ok [| `int 42 |] -> ()
    | _ -> fail_test "wrong parsing of 42");
    (test_tol (line "42 42 some more")
    >>< function
    | Ok [| `int 42; `float 42.; `string "some"; `string "more" |] -> ()
    | _ -> fail_test "wrong parsing of '42 42 some more'");
    (test_tol ~strict_row_length:true (line "42")
    >>< function
    | Error (`wrong_format (`column_number, _, _)) -> ()
    | _ -> fail_test "wrong strict parsing of 42");
    (test_tol ~strict_row_length:true ~strict_cell_type:true (line "42 42 some more")
    >>< function
    | Error (`wrong_format (`column_number, _, _)) -> ()
    | _ -> fail_test "wrong strict parsing of '42 42 some more'");
    [%expect
      {|
      42-42-42: of_line: true
      42-42-42: to_line: true
      42-42.-42: of_line: true
      42-42.-42: to_line: true
      42-42.-42-any-string-42: of_line: true
      42-42.-42-any-string-42: to_line: true
    |}]
  ;;
end
