open Biocaml_internal_pervasives

(* This values should probably be generated automatically by the script generating the distribution. *)
let major_version = 0
let minor_version = 0
let release_date = 1900,0,0

let version =
  let (year,month,day) = release_date in
  sprintf "%d.%d %d-%d-%d" major_version minor_version year month day
