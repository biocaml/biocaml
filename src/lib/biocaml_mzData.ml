(* File: biocaml_mzXML.ml

   Copyright (C) 2011

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


open Bigarray

type vec = (float, float64_elt, fortran_layout) Array1.t


module Base64 = struct

  external init : unit -> unit = "biocaml_base64_init"
  let () = init ()

  external decode32 :
    string -> npeaks:int -> (float, float64_elt, _) Array1.t -> unit
    = "biocaml_base64_decode32"

  external decode64 :
    string -> npeaks:int -> (float, float64_elt, _) Array1.t -> unit
    = "biocaml_base64_decode64"

  let decode ~precision s =
    if precision = 32 then
      (* Two 32 bits (thus 4 bytes) floats per peak *)
      let npeaks = (String.length s / 4) * 3 / 8 in
      let v = Array1.create float64 fortran_layout npeaks in
      decode32 s ~npeaks v;
      v
    else if precision = 64 then
      let npeaks = (String.length s / 4) * 3 / 16 in
      let v = Array1.create float64 fortran_layout npeaks in
      decode64 s ~npeaks v;
      v
    else invalid_arg "Biocaml_mzData: <peak> precision must be 32 or 64"
end

type spectrum = {
  id: int;
  mslevel: int;
  start_mz: float;
  end_mz: float;
  mz: vec;
  int: vec;
}


let rec concat_data pull close_entid data =
  match pull() with
  | None -> failwith "Biocaml_mzData.spectrums: file ended while gathering \
                     <data> content"
  | Some(Pxp_types.E_end_tag(_, entid)) when entid = close_entid ->
    data
  | Some(Pxp_types.E_char_data(s)) ->
    concat_data pull close_entid (data ^ s)
  | _ -> concat_data pull close_entid data

let rec decode_data pull =
  match pull() with
  | None -> failwith "Biocaml_mzData.spectrums: file ended while looking for \
                     <data>"
  | Some(Pxp_types.E_start_tag("data", atts, _, entid)) ->
    if List.assoc "endian" atts <> "little" then
      failwith "Biocaml_mzData.spectrums: byte order must be little endian";
    let precision = int_of_string(List.assoc "precision" atts) in
    let data = concat_data pull entid "" in
    Base64.decode ~precision data
  | _ -> decode_data pull

let rec get_spectrum pull close_entid spec =
  match pull() with
  | None -> failwith "Biocaml_mzData.spectrums: file ended while parsing for \
                     <spectrum>"
  | Some(Pxp_types.E_start_tag("spectrumInstrument", atts, _, _)) ->
    let mslevel = int_of_string(List.assoc "msLevel" atts)
    and start_mz = float_of_string(List.assoc "mzRangeStart" atts)
    and end_mz = float_of_string(List.assoc "mzRangeStop" atts) in
    let spec = { spec with mslevel; start_mz; end_mz } in
    get_spectrum pull close_entid spec
  | Some(Pxp_types.E_start_tag("mzArrayBinary", _, _, _)) ->
    let spec = { spec with mz = decode_data pull } in
    get_spectrum pull close_entid spec
  | Some(Pxp_types.E_start_tag("intenArrayBinary", _, _, _)) ->
    let spec = { spec with int = decode_data pull } in
    get_spectrum pull close_entid spec
  | Some(Pxp_types.E_end_tag(_, entid)) when entid = close_entid ->
    spec
  | _ -> get_spectrum pull close_entid spec


let dummy_vec = Array1.create float64 fortran_layout 0

let spectrums fname =
  let config = Pxp_types.default_config in
  let source = Pxp_types.from_file fname in
  let entmng = Pxp_ev_parser.create_entity_manager config source in
  let pull = Pxp_ev_parser.create_pull_parser
               config (`Entry_document []) entmng in
  let scans = ref [] in
  let continue = ref true in
  while !continue do
    match pull() with
    | None -> continue := false
    | Some(Pxp_types.E_start_tag("spectrum", atts, _, entid)) ->
      let id = int_of_string(List.assoc "id" atts) in
      (* retentionTime ? *)
      let scan = { id; mslevel = 0; start_mz = 0.; end_mz = 0.;
                   mz = dummy_vec; int = dummy_vec } in
      let scan = get_spectrum pull entid scan in
      scans := scan :: !scans
    | _ -> ()
  done;
  !scans


(* Local Variables: *)
(* compile-command: "make -k -C ../.." *)
(* End: *)
