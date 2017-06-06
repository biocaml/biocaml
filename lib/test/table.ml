open Core_kernel.Std
module Table = Biocaml_unix.Table
module Line = Biocaml_unix.Line
open OUnit

let ( >>< ) x f = ( |> ) x f

let test_row () =
  let tdash ?to_line s r =
    let row_result =
      Table.Row.of_line ~separators:['-']
        ~format:[| `type_int; `type_float; `type_string|]
        (Line.of_string_unsafe s) in
    assert_bool (sprintf "%s: of_line" s) ((=) r row_result);
    Result.iter row_result ~f:(fun row ->
          match to_line with
          | Some str ->
            assert_equal ~msg:(sprintf "%s: to_line" s) ~printer:ident
              (Table.Row.to_line ~sep:"-" row : Line.t :> string) str
          | None ->
            assert_equal ~msg:(sprintf "%s: to_line" s) ~printer:ident
              (Table.Row.to_line ~sep:"-" row : Line.t :> string) s
              (* row = Line.of_string_unsafe s) *)
      )

  in
  tdash "42-42-42" (Ok [| `int 42; `float 42.; `string "42" |]);
  tdash "42-42.-42" (Ok [| `int 42; `float 42.; `string "42" |])
    ~to_line:"42-42-42";

  tdash "42-42.-42-any-string-42" (Ok [| `int 42; `float 42.; `string "42";
                                         `string "any"; `string "string";
                                         `string "42" |])
    ~to_line:"42-42-42-any-string-42";

  let fail_test fmt = ksprintf (fun s -> assert_bool s false) fmt in
  let test_tol =
    Table.Row.of_line ~separators:[' '] ~format:[| `type_int; `type_float |]
  in
  let line = Line.of_string_unsafe in
  test_tol (line "42 42")
  >>< begin function
  | Ok [| `int 42; `float 42. |] -> ()
  | _ -> fail_test "wrong parsing of 42 42"
  end;

  test_tol (line "42")
  >>< begin function
  | Ok [| `int 42; |] -> ()
  | _ -> fail_test "wrong parsing of 42"
  end;

  test_tol (line "42 42 some more")
  >>< begin function
  | Ok [| `int 42; `float 42.; `string "some" ; `string "more"|] -> ()
  | _ -> fail_test "wrong parsing of '42 42 some more'"
  end;

  test_tol ~strict_row_length:true (line "42")
  >>< begin function
  | Error (`wrong_format (`column_number, _, _)) -> ()
  | _ -> fail_test "wrong strict parsing of 42"
  end;

  test_tol ~strict_row_length:true ~strict_cell_type:true
    (line "42 42 some more")
  >>< begin function
  | Error (`wrong_format (`column_number, _, _)) -> ()
  | _ -> fail_test "wrong strict parsing of '42 42 some more'"
  end;

  ()

let tests = "Table" >::: [
  "Rows" >:: test_row;
]
