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


open Printf
open Bigarray

type vec = (float, float64_elt, fortran_layout) Array1.t
type int_vec = (int, int_elt, fortran_layout) Array1.t


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
      (* One 32 bits (thus 4 bytes) floats per peak *)
      let npeaks = (String.length s / 4) * 3 / 4 in
      let v = Array1.create float64 fortran_layout npeaks in
      decode32 s ~npeaks v;
      v
    else if precision = 64 then
      let npeaks = (String.length s / 4) * 3 / 8 in
      let v = Array1.create float64 fortran_layout npeaks in
      decode64 s ~npeaks v;
      v
    else invalid_arg "Biocaml_mzData: <peak> precision must be 32 or 64"
end

type spectrum = {
  id: int;
  mslevel: int;
  mass: float;
  start_mz: float;
  end_mz: float;
  mz: vec;
  int: vec;
  z: int_vec;
}

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
    failwith "Biocaml_mzData.spectrums: got tag while looking for XML data"
  | _ -> get_next_data xml

let rec return_on_end_tag xml v =
  match Xmlm.input xml with
  | `El_end -> v
  | `El_start _ -> skip_tag xml;  return_on_end_tag xml v
  | _ -> return_on_end_tag xml v

let rec attribute name = function
  | [] -> failwith("Biocaml_mzData.spectrums: " ^ name
                  ^ " not found in attributes")
  | ((_, n), v) :: tl -> if n = name then v else attribute name tl

(* mzData parsing
 ***********************************************************************)

(* Parse and decode <data>. *)
let rec decode_data xml =
  match Xmlm.input xml with
  | `El_start((_, "data"), atts) ->
    if attribute "endian" atts <> "little" then
      failwith "Biocaml_mzData.spectrums: byte order must be little endian";
    let precision = int_of_string(attribute "precision" atts) in
    let length = int_of_string(attribute "length" atts) in
    let data = get_next_data xml in
    let v = Base64.decode ~precision data in
    if Array1.dim v <> length then
      failwith(sprintf "Biocaml_mzData.spectrums: <data> expected length: %i, \
                        got: %i" length (Array1.dim v));
    return_on_end_tag xml v (* </data> *)
  | _ -> decode_data xml

(* [get_spectrum xml spec 0] returns [spec] updated with the content
   of the <spectrum> block.  *)
let rec get_spectrum xml spec depth =
  match Xmlm.input xml with
  | `El_start((_,"spectrumInstrument"), atts) ->
    let mslevel = int_of_string(attribute "msLevel" atts)
    and start_mz = float_of_string(attribute "mzRangeStart" atts)
    and end_mz = float_of_string(attribute "mzRangeStop" atts) in
    let spec = { spec with mslevel; start_mz; end_mz } in
    get_spectrum xml spec (depth + 1)
  | `El_start((_,"mzArrayBinary"), _) ->
    let spec = { spec with mz = decode_data xml } in
    get_spectrum xml spec (depth + 1)
  | `El_start((_,"intenArrayBinary"), _) ->
    let spec = { spec with int = decode_data xml } in
    get_spectrum xml spec (depth + 1)
  | `El_start _ ->
    get_spectrum xml spec (depth + 1)
  | `El_end ->
    if depth = 0 then spec else get_spectrum xml spec (depth - 1)
  | _ -> get_spectrum xml spec depth (* skip *)


let empty_vec = Array1.create float64 fortran_layout 0
let empty_int_vec = Array1.create int fortran_layout 0

let spectrums fname =
  let fh = open_in fname in
  let xml = Xmlm.make_input ~enc:`UTF_8 (`Channel fh) in
  let scans = ref [] in
  while not(Xmlm.eoi xml) do
    match Xmlm.input xml with
    | `El_start((_, "spectrum"), atts) ->
      let id = int_of_string(attribute "id" atts) in
      (* retentionTime ? *)
      let scan = { id; mslevel = 0; mass = nan; start_mz = nan; end_mz = nan;
                   mz = empty_vec; int = empty_vec; z = empty_int_vec } in
      let scan = get_spectrum xml scan 0 in
      scans := scan :: !scans
    | _ -> ()
  done;
  !scans


(* Local Variables: *)
(* compile-command: "make -k -C ../.." *)
(* End: *)
