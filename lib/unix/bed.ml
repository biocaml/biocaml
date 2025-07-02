type item = string * int * int * Table.Row.t [@@deriving sexp]

type parsing_spec =
  [ `enforce of Table.Row.t_type
  | `strings
  ]
[@@deriving sexp]

module Error = struct
  type parsing_base =
    [ `wrong_format of
      [ `column_number | `float_of_string of string | `int_of_string of string ]
      * Table.Row.t_type
      * string
    | `wrong_number_of_columns of Table.Row.t
    ]
  [@@deriving sexp]

  type parsing = [ `bed of parsing_base ] [@@deriving sexp]
  type t = parsing [@@deriving sexp]
end

let item_of_line ~how line =
  let separators = [ '\t'; ' ' ] in
  let format, strict =
    let base = [| `type_string; `type_int; `type_int |] in
    match how with
    | `strings -> base, false
    | `enforce tt -> Array.append base tt, true
  in
  Table.Row.of_line
    ~separators
    ~format
    ~strict_row_length:strict
    ~strict_cell_type:strict
    line
  |> function
  | Ok row when Array.length row >= 3 ->
    let n =
      match row.(0) with
      | `string s -> s
      | _ -> assert false
    in
    let h =
      match row.(1) with
      | `int i -> i
      | _ -> assert false
    in
    let l =
      match row.(2) with
      | `int i -> i
      | _ -> assert false
    in
    let q = Array.slice row 3 (Array.length row) in
    Ok (n, h, l, q)
  | Ok row -> Error (`bed (`wrong_number_of_columns row))
  | Error e -> Error (`bed e)
;;

let item_to_line (n, l, h, r) =
  Table.Row.to_line ~sep:"\t" (Array.append [| `string n; `int l; `int h |] r)
;;

module Transform = struct
  let string_to_item ?(more_columns = `strings) () =
    Tfxm.on_output (Lines.Transform.string_to_item ()) ~f:(fun line ->
      item_of_line ~how:more_columns line)
  ;;

  let item_to_string () =
    Tfxm.on_input (Lines.Transform.item_to_string ()) ~f:(fun item -> item_to_line item)
  ;;
end

exception Error of Error.t

let error_to_exn e = Error e

let in_channel_to_item_stream ?(buffer_size = 65536) ?more_columns inp =
  let x = Transform.string_to_item ?more_columns () in
  Tfxm.(in_channel_strings_to_stream inp x ~buffer_size)
;;

let in_channel_to_item_stream_exn ?buffer_size ?more_columns inp =
  CFStream.Stream.result_to_exn
    ~error_to_exn
    (in_channel_to_item_stream ?buffer_size ?more_columns inp)
;;

module Test = struct
  let make_stream ?more_columns file : (item, Error.parsing) Result.t CFStream.Stream.t =
    let filename = Filename.concat "../../etc/test_data" file in
    let bed_parser = Transform.string_to_item ?more_columns () in
    let inp = In_channel.create filename in
    Tfxm.in_channel_strings_to_stream ~buffer_size:10 inp bed_parser
  ;;

  let some_ok x = Some (Ok x)

  let%expect_test "test_parser" =
    let s = make_stream "bed_01.bed" in
    printf
      "%s: %b\n"
      "01 chrA"
      Poly.(CFStream.Stream.next s = some_ok ("chrA", 42, 45, [||]));
    printf
      "%s: %b\n"
      "01 chrB"
      Poly.(CFStream.Stream.next s = some_ok ("chrB", 100, 130, [||]));
    printf
      "%s: %b\n"
      "01 chrC"
      Poly.(CFStream.Stream.next s = some_ok ("chrC", 200, 245, [||]));
    printf "%s: %b\n" "01 EOF" Poly.(CFStream.Stream.next s = None);
    let s = make_stream "bed_02_incomplete_line.bed" in
    printf
      "%s: %b\n"
      "02 chrA"
      Poly.(CFStream.Stream.next s = some_ok ("chrA", 42, 45, [||]));
    printf
      "%s: %b\n"
      "02 chrB error "
      (match CFStream.Stream.next s with
       | Some _ -> true
       | _ -> false);
    let s =
      make_stream
        ~more_columns:(`enforce [| `type_string; `type_int; `type_float |])
        "bed_03_more_cols.bed"
    in
    let the_expected_list = [| `string "some_string"; `int 42; `float 3.14 |] in
    printf
      "%s: %b\n"
      "03 chrA"
      Poly.(CFStream.Stream.next s = some_ok ("chrA", 42, 45, the_expected_list));
    printf
      "%s: %b\n"
      "03 chrB"
      Poly.(CFStream.Stream.next s = some_ok ("chrB", 100, 130, the_expected_list));
    printf
      "%s: %b\n"
      "03 chrC"
      Poly.(CFStream.Stream.next s = some_ok ("chrC", 200, 245, the_expected_list));
    printf "%s: %b\n" "03 EOF" Poly.(CFStream.Stream.next s = None);
    let s =
      make_stream
        ~more_columns:(`enforce [| `type_string; `type_int; `type_float |])
        "bed_04_more_cols_error.bed"
    in
    let the_expected_list = [| `string "some_string"; `int 42; `float 3.14 |] in
    printf
      "%s: %b\n"
      "04 chrA"
      Poly.(CFStream.Stream.next s = some_ok ("chrA", 42, 45, the_expected_list));
    printf
      "%s: %b\n"
      "04 chrB error "
      (match CFStream.Stream.next s with
       | Some (Error (`bed (`wrong_format (`int_of_string _, _, _)))) -> true
       | _ -> false);
    printf
      "%s: %b\n"
      "04 chrC error "
      (match CFStream.Stream.next s with
       | Some (Error (`bed (`wrong_format (`column_number, _, _)))) -> true
       | _ -> false);
    printf "%s: %b\n" "04 EOF" Poly.(CFStream.Stream.next s = None);
    [%expect
      {|
      01 chrA: true
      01 chrB: true
      01 chrC: true
      01 EOF: true
      02 chrA: true
      02 chrB error : true
      03 chrA: true
      03 chrB: true
      03 chrC: true
      03 EOF: true
      04 chrA: true
      04 chrB error : true
      04 chrC error : true
      04 EOF: true
    |}]
  ;;

  let make_printer_stream ?more_columns file =
    let filename = Filename.concat "../../etc/test_data" file in
    let bed_parser = Transform.string_to_item ?more_columns () in
    let printer = Transform.item_to_string () in
    let trans = Tfxm.compose_result_left bed_parser printer in
    let ic = In_channel.create filename in
    Tfxm.in_channel_strings_to_stream ~buffer_size:10 ic trans
  ;;

  let%expect_test "test_printer" =
    let s =
      make_printer_stream
        ~more_columns:(`enforce [| `type_string; `type_int; `type_float |])
        "bed_03_more_cols.bed"
    in
    let camlstream =
      CFStream.Stream.result_to_exn
        ~error_to_exn:(fun _ -> failwith "Unexpected error in camlstream")
        s
    in
    let l = CFStream.Stream.npeek camlstream Int.max_value in
    printf
      "%b\n"
      (List.equal
         String.equal
         l
         [ "chrA\t42\t45\tsome_string\t42\t3.14\n"
         ; "chrB\t100\t130\tsome_string\t42\t3.14\n"
         ; "chrC\t200\t245\tsome_string\t42\t3.14\n"
         ]);
    [%expect {| true |}]
  ;;
end
