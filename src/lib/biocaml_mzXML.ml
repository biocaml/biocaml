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


module Base64 = struct

  external init : unit -> unit = "biocaml_base64_init"
  let () = init ()

  external decode32 : string -> npeaks:int -> float array -> float array -> unit
    = "biocaml_base64_decode32"

  external decode64 : string -> npeaks:int -> float array -> float array -> unit
    = "biocaml_base64_decode64"

  let decode ~precision s =
    if precision = 32 then
      (* Two 32 bits (thus 8 bytes) floats per peak *)
      let npeaks = (String.length s / 4) * 3 / 8 in
      let mz = Array.make npeaks 0.
      and inty = Array.make npeaks 0. in
      decode32 s ~npeaks mz inty;
      mz, inty
    else if precision = 64 then
      let npeaks = (String.length s / 4) * 3 / 16 in
      let mz = Array.make npeaks 0.
      and inty = Array.make npeaks 0. in
      decode64 s ~npeaks mz inty;
      mz, inty
    else invalid_arg "Biocaml_mzXML: precision must be 32 or 64"
end

include Base64


(* Local Variables: *)
(* compile-command: "make -k -C ../.." *)
(* End: *)
