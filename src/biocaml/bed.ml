open Batteries_uni;; open Printf

(* Parse single line of a BED file. *)
let parse_line line =
  match String.nsplit line "\t" with
    | [chr; lo; hi] ->
        chr, int_of_string lo, int_of_string hi
    | l -> failwith (l |> List.length |> sprintf "expecting exactly 3 columns but found %d")

let enum_input = IO.lines_of |- (Enum.map parse_line) |- Util.err_enum

let sqlite_db_of_enum ?(db="") ?(table="bed") e =
  let db = Sqlite3.db_open db in
  let stmt = sprintf "CREATE TABLE %s (chr TEXT, start INTEGER, end INTEGER);" table in
  (match Sqlite3.exec db stmt with
    | Sqlite3.Rc.OK -> ()
    | x -> failwith (Sqlite3.Rc.to_string x)
  );
  let insert (chr,lo,hi) =
    let stmt = sprintf "INSERT INTO %s values ('%s', %d, %d);" table chr lo hi in
    match Sqlite3.exec db stmt with
      | Sqlite3.Rc.OK -> ()
      | x -> failwith (Sqlite3.Rc.to_string x)
  in
  Enum.iter insert e;
  db
