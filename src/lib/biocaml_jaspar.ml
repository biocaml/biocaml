open Biocaml_internal_pervasives

type collection = Core | Phylofacts | CNE | PBM | PBM_HOMEO | PBM_HLH | FAM | SPLICE | POLII

type motif = {
  id : string ;
  collection : collection ;
  factor_name : string ;
  factor_class : string ;
  information_contents : float ;
  comment : string option ;
  accession : string option ;
  medline : string ;
  matrix : int array array ;
}

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
  | s -> failwithf "Jaspa.collection_of_string: unknown collection %s" s ()
    
    
let attrs_of_string =
  let lazy_rex = lazy (Pcre.regexp "; +(?<K>[^\"]+) \"(?<V>[^\"]*)\"") in
  fun s ->
    let rex = Lazy.force lazy_rex in
    let r : Pcre.substrings array = Pcre.exec_all ~rex s in
    Stream.(Infix.(combine
      (Stream.of_array r /@ (Pcre.get_named_substring rex "K"),
       Stream.of_array r /@ (Pcre.get_named_substring rex "V"))))
    |! Stream.to_list


let transpose m = 
  Array.init
    (Array.length m.(0))
    (fun i -> 
       Array.init
	 (Array.length m)
	 (fun j -> m.(j).(i)))
    
let space_split =
  let rex = Pcre.regexp "[ \t]+" in
  Pcre.split ~rex

let load_matrix ~path ~id =
  In_channel.read_lines (path ^ "/" ^ id ^ ".pfm")
  |! List.map ~f:String.lstrip
  |! List.map ~f:space_split
  |! List.map ~f:Array.of_list
  |! List.map ~f:(Array.map ~f:(fun x -> x |! Float.of_string |! Int.of_float))
  |! Array.of_list
  |! transpose

let load_motif ~id ~info ~factor_name ~factor_class ~attrs ~path = 
  let attrs = attrs_of_string attrs in 
  {
    id ; factor_name ; factor_class ;
    collection = collection_of_string (List.Assoc.find_exn attrs "collection") ;
    information_contents = Float.of_string info ;
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
    matrix = load_matrix ~path ~id ;
  }

let load path =
  In_channel.read_lines (path ^ "/matrix_list.txt")
  |! List.map ~f:(fun l -> String.split l ~on:'\t')
  |! List.map ~f:(function
      | [ id ; info ; factor_name ; factor_class ; attrs ] -> 
          load_motif ~id ~info ~factor_name ~factor_class ~attrs ~path
      | l -> 
          failwithf "Jaspa.load: incorrect fields\n%s" (String.concat ~sep:"\n" l) ())
