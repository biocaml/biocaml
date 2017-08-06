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

(* http://www.umanitoba.ca/afs/plant_science/psgendb/local/install/ncbi_cxx--Jun_15_2010/src/algo/ms/formats/mzdata/mzData.dtd *)

open Core_kernel
open Bigarray

type vec = (float, float64_elt, fortran_layout) Array1.t
type int_vec = (int, int_elt, fortran_layout) Array1.t

module Base64 = struct

  external init : unit -> unit = "biocaml_base64_init"
  let () = init ()

  external little32 :
    string -> npeaks:int -> (float, float64_elt, _) Array1.t -> unit
    = "biocaml_base64_little32"

  external big32 :
    string -> npeaks:int -> (float, float64_elt, _) Array1.t -> unit
    = "biocaml_base64_big32"

  external little64 :
    string -> npeaks:int -> (float, float64_elt, _) Array1.t -> unit
    = "biocaml_base64_little64"

  external big64 :
    string -> npeaks:int -> (float, float64_elt, _) Array1.t -> unit
    = "biocaml_base64_big64"

  let decode ~precision ~little_endian s =
    if precision = 32 then
      (* One 32 bits (thus 4 bytes) floats per peak *)
      let npeaks = (String.length s / 4) * 3 / 4 in
      let v = Array1.create float64 fortran_layout npeaks in
      if little_endian then little32 s ~npeaks v else big32 s ~npeaks v;
      v
    else if precision = 64 then
      let npeaks = (String.length s / 4) * 3 / 8 in
      let v = Array1.create float64 fortran_layout npeaks in
      if little_endian then little64 s ~npeaks v else big64 s ~npeaks v;
      v
    else invalid_arg "MzData: <peak> precision must be 32 or 64"
end

(* XML helper functions
 ***********************************************************************)

let rec skip_tag_loop xml depth =
  match Xmlm.input xml with
  | `El_start _ -> skip_tag_loop xml (depth + 1)
  | `El_end -> if depth > 0 then skip_tag_loop xml (depth - 1)
  | `Data _ | `Dtd _ -> skip_tag_loop xml depth

(* The start tag is supposed to be already read.  Skip to the closing tag. *)
let skip_tag xml = skip_tag_loop xml 0

let rec get_next_data xml =
  match Xmlm.input xml with
  | `Data s -> (* No to consecutive `Data are guaranteed, no concat *)
    s
  | `El_start _ ->
    skip_tag xml; (* ensure the corresponding close tag is read *)
    get_next_data xml
  | `El_end ->
    failwith "MzData.spectrums: got tag while looking for XML data"
  | _ -> get_next_data xml

let rec return_on_end_tag xml v =
  match Xmlm.input xml with
  | `El_end -> v
  | `El_start _ -> skip_tag xml;  return_on_end_tag xml v
  | _ -> return_on_end_tag xml v

let rec attribute_exn name = function
  | [] -> failwith "MzData.spectrums: attribute not found"
  | ((_, n), v) :: tl -> if n = name then v else attribute_exn name tl


(* mzData parsing
 ***********************************************************************)

module Precursor = struct
  type t = {
    mslevel: int; (** 1: MS, 2: MS/MS,... *)
    mz: float;  (** MassToChargeRatio *)
    z: float;   (** ChargeState *)
    int: float; (** Intensity *)
  }

  (* Commission of atomic weights and isotopic abondance *)
  let mass_proton = 1.00727646677 (* Dalton *)

  let mass p = (p.mz -. mass_proton) *. p.z

  (* Get <ionSelection> content. FIXME: the spec does not define which
     param. should be present. *)
  let rec get_ionSelection xml p depth =
    match Xmlm.input xml with
    | `El_start((_, "cvParam"), attr) ->
      let depth = depth + 1 in (* for </cvParam> *)
      let name = attribute_exn "name" attr in
      let value = attribute_exn "value" attr in
      if name = "MassToChargeRatio" then
        get_ionSelection xml { p with mz = Float.of_string value } depth
      else if name = "ChargeState" then
        get_ionSelection xml { p with z = Float.of_string value } depth
      else if name = "Intensity" then
        get_ionSelection xml { p with int = Float.of_string value } depth
      else
        get_ionSelection xml p depth
    | `El_start _ -> get_ionSelection xml p (depth + 1)
    | `El_end -> if depth = 0 then p  (* </ionSelection> *)
                else get_ionSelection xml p (depth - 1)
    | `Data _ | `Dtd _ -> get_ionSelection xml p depth (* skip *)

  let rec get_precursor xml p =
    match Xmlm.input xml with
    | `El_start((_, "ionSelection"), _) -> get_ionSelection xml p 0
    | `El_start _ -> skip_tag xml; get_precursor xml p (* <activation> *)
    | `El_end -> p (* </precursor> *)
    | `Data _ | `Dtd _ -> get_precursor xml p

  (* Knowing that <precursorList> was just read, parse the [xml] to
     get the list of precursors.  *)
  let rec add_list xml pl =
    match Xmlm.input xml with
    | `El_start((_, "precursor"), attr) ->
      let mslevel = int_of_string(attribute_exn "msLevel" attr) in
      let p = get_precursor xml { mslevel; mz = Float.nan; z = Float.nan; int = Float.nan } in
      add_list xml (p :: pl)
    | `El_start _ -> skip_tag xml;  add_list xml pl
    | `El_end -> pl                      (* </precursorList> *)
    | `Data _ | `Dtd _ -> add_list xml pl

  let list xml = add_list xml []
end

type spectrum = {
  id: int;
  mslevel: int;
  precursor: Precursor.t list;
  mz: vec;
  int: vec;
  sup: (string * vec) list;
}

(* Parse and decode <data>. *)
let rec vec_of_binary_data xml =
  match Xmlm.input xml with
  | `El_start((_, "data"), atts) ->
    let precision = int_of_string(attribute_exn "precision" atts) in
    let length = int_of_string(attribute_exn "length" atts) in
    let little_endian = attribute_exn "endian" atts = "little" in
    let data = get_next_data xml in
    let v = Base64.decode ~precision ~little_endian data in
    if Array1.dim v <> length then
      failwith(sprintf "MzData: Invalid XML: <data> expected \
                        length: %i, got: %i" length (Array1.dim v));
    return_on_end_tag xml v (* </data> *)
  | _ -> vec_of_binary_data xml

(* [get_spectrum xml spec 0] returns [spec] updated with the content
   of the <spectrum> block.  *)
let rec get_spectrum xml spec depth =
  match Xmlm.input xml with
  | `El_start((_, "spectrumInstrument"), atts) ->
    let mslevel = int_of_string(attribute_exn "msLevel" atts) in
    let spec = { spec with mslevel } in
    get_spectrum xml spec (depth + 1)
  | `El_start((_, "precursorList"), _) ->
    let spec = { spec with precursor = Precursor.list xml } in
    get_spectrum xml spec (depth + 1)
  | `El_start((_, "mzArrayBinary"), _) ->
    let spec = { spec with mz = vec_of_binary_data xml } in
    get_spectrum xml spec (depth + 1)
  | `El_start((_, "intenArrayBinary"), _) ->
    let spec = { spec with int = vec_of_binary_data xml } in
    get_spectrum xml spec (depth + 1)
  (* | `El_start((_, "supDataArray"), _) -> *)
  (* | `El_start((_, "supDataArrayBinary"), _) -> *)
  | `El_start _ -> get_spectrum xml spec (depth + 1)
  | `El_end ->
    if depth = 0 then spec else get_spectrum xml spec (depth - 1)
  | _ -> get_spectrum xml spec depth (* skip *)


let empty_vec = Array1.create float64 fortran_layout 0

let of_file fname =
  let fh = In_channel.create fname in
  let xml = Xmlm.make_input ~enc:(Some `UTF_8) (`Channel fh) in
  let scans = ref [] in
  while not(Xmlm.eoi xml) do
    match Xmlm.input xml with
    | `El_start((_, "spectrum"), atts) ->
      let id = int_of_string(attribute_exn "id" atts) in
      (* retentionTime ? *)
      let scan = { id; mslevel = 0; precursor = [];
                   mz = empty_vec; int = empty_vec; sup = [] } in
      let scan = get_spectrum xml scan 0 in
      scans := scan :: !scans
    | _ -> ()
  done;
  !scans


(* Local Variables: *)
(* compile-command: "make -k -C ../.." *)
(* End: *)
