open Core
open CFStream
open Biocaml_unix.Std
open Core
open Core_bench.Std

let s = "actgcatgcatgc\ncaactatcatctatca\ncatcattcatcttctatcta\ncatcatctatctatctat\n"
let s = s ^ s ^ s ^ s ^ s ^ s ^ s ^ s ^ s

let core_split () =
  String.split ~on:'\n' s
  |> ignore

let sosa_split () =
  Sosa.Native_string.split ~on:(`Character '\n') s
  |> ignore

let container_split () =
  CCString.lines s
  |> ignore

(* This implementation is adapted from core, i.e. specialized for line
   splitting *)
let specialized_split str =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      String.sub str ~pos:0 ~len:last_pos :: acc
    else
      if str.[pos] = '\n' then
        let pos1 = pos + 1 in
        let sub_str = String.sub str ~pos:pos1 ~len:(last_pos - pos1) in
        loop (sub_str :: acc) pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)

let specialized_split () =
  specialized_split s
  |> ignore


let command =
  Bench.make_command [
    Bench.Test.create ~name:"container.split" container_split ;
    Bench.Test.create ~name:"sosa.split" sosa_split ;
    Bench.Test.create ~name:"core.split" core_split ;
    Bench.Test.create ~name:"specialized.core.split" specialized_split ;
  ]

