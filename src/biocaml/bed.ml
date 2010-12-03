open Batteries_uni;; open Printf

(* Parse single line of a BED file. *)
let parse_line line =
  match String.nsplit line "\t" with
    | [chr; lo; hi] ->
        chr, int_of_string lo, int_of_string hi
    | l -> failwith (l |> List.length |> sprintf "expecting exactly 3 columns but found %d")
        
let enum_input = IO.lines_of |- (Enum.map parse_line)
