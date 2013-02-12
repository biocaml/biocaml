
open OUnit
open Biocaml
open Core.Std

open Result

let test_row () =
  let tdash ?to_line s r =
    let row_result =
      Table.Row.of_line ~separators:['-']
        ~format:[| `type_int; `type_float; `type_string|]
        (Line.of_string_unsafe s) in
    assert_bool (sprintf "%s: of_line" s) ((=) r row_result);
    Result.iter row_result
      begin fun row ->
          match to_line with
          | Some str ->
            assert_equal ~msg:(sprintf "%s: to_line" s) ~printer:ident
              (Table.Row.to_line ~sep:"-" row : Line.t :> string) str
          | None ->
            assert_equal ~msg:(sprintf "%s: to_line" s) ~printer:ident
              (Table.Row.to_line ~sep:"-" row : Line.t :> string) s
              (* row = Line.of_string_unsafe s) *)
      end;

  in
  tdash "42-42-42" (Ok [| `int 42; `float 42.; `string "42" |]);
  tdash "42-42.-42" (Ok [| `int 42; `float 42.; `string "42" |])
    ~to_line:"42-42-42";

  ()

let tests = "Table" >::: [
  "Rows" >:: test_row;
]
