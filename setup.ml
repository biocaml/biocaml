(* setup.ml generated for the first time by OASIS v0.3.1 *)

(* OASIS_START *)
(* DO NOT EDIT (digest: 7f47a529f70709161149c201ccd90f0b) *)
#use "topfind";;
#require "oasis.dynrun";;
open OASISDynRun;;
(* OASIS_STOP *)

open OASISTypes

(* Check whether ocaml-zip findlib name is "zip" (upstream) or
   "ocamlzip" (godi). *)
let remove_zip_lib =
  try ignore(BaseCheck.package_version "zip"); "camlzip"
  with _ -> "zip"

let keep_zip = function
  | FindlibPackage(pkg, ver) when pkg = remove_zip_lib -> false
  | _ -> true

let setup_t =
  let resolve_zip = function
    | Library(cs, build, lib) ->
       let bs_build_depends = List.filter keep_zip build.bs_build_depends in
       Library(cs, { build with bs_build_depends }, lib)
    | s -> s
  in
  let sections = List.map resolve_zip setup_t.BaseSetup.package.sections in
  let package = { setup_t.BaseSetup.package with sections } in
  { setup_t with BaseSetup.package = package }


let () = BaseSetup.setup setup_t
