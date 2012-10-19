open Batteries
open Core.Std
open Printf

type collection = Core | Phylofacts | CNE | PBM | PBM_HOMEO | PBM_HLH | FAM | SPLICE | POLII

type matrix = {
  id : string ;
  collection : collection ;
  tf_id : string ;
  tf_family : string ;
  information_contents : float ;
  comment : string option ;
  accession : string option ;
  medline : string ;
}

let split = Core.Core_string.split

let collection_of_string = function
| "CNE" -> CNE
| "FAM" -> FAM
| "PHYLOFACTS" -> Phylofacts
| "CORE" -> Core
| "PBM" -> PBM
| "PBM_HOMEO" -> PBM_HOMEO
| "PBM_HLH" -> PBM_HLH
| "SPLICE" -> SPLICE
| "POLII" -> POLII
| s -> failwithf "Biocaml_jaspar.collection_of_string: unknown collection %s" s ()
    
    
let attrs_of_string =
  let rex = Pcre.regexp "; +(?<K>[^\"]+) \"(?<V>[^\"]*)\"" in
  fun s ->
    let r = Pcre.exec_all ~rex s in
    BatEnum.combine
      (BatArray.enum r /@ (Pcre.get_named_substring rex "K"),
       BatArray.enum r /@ (Pcre.get_named_substring rex "V"))
    |> BatList.of_enum

let load_matrix ~id ~info ~tf_id ~tf_family ~attrs ~path = 
  let attrs = attrs_of_string attrs in 
  {
    id ; 
    collection = collection_of_string (List.Assoc.find_exn attrs "collection") ;
    tf_id ; tf_family ; information_contents = float_of_string info ;
    comment = (
      match List.Assoc.find attrs "comment" with
      | None -> None
      | Some "-" -> None
      | x -> x
    ) ;
    accession = (
      match List.Assoc.find attrs "acc" with
      | None -> None
      | Some "" -> None
      | x -> x
    ) ;
    medline = List.Assoc.find_exn attrs "medline" ;
  }

let load path =
  BatFile.lines_of (path ^ "/matrix_list.txt")
  /@ (fun l -> split l ~on:'\t')
  /@ (function
      | [ id ; info ; tf_id ; tf_family ; attrs ] -> load_matrix ~id ~info ~tf_id ~tf_family ~attrs ~path
      | l -> 
          failwithf "Biocaml_jaspar.load: incorrect fields\n%s" (String.concat ~sep:"\n" l) ())
  |> BatList.of_enum




















