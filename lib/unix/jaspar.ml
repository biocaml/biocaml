open Core_kernel
open CFStream
let (/) = Filename.concat

type collection = Core | Phylofacts | CNE | PBM | PBM_HOMEO | PBM_HLH | FAM | SPLICE | POLII

type motif = {
  id : string ;
  jaspar_id : string ;
  collection : collection ;
  factor_name : string ;
  factor_class : string ;
  family : string option ;
  comment : string option ;
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

let fold_data_file name ~init ~f =
  let add_item accu l =
    let fields = String.split ~on:'\t' (l : Line.t :> string) in
    f accu fields
  in
  In_channel.with_file name ~f:(fun ic ->
    Stream.fold
      (Lines.of_channel ic)
      ~init
      ~f:add_item
  )

let load_matrix fn =
  fold_data_file (fn / "MATRIX.txt") ~init:String.Map.empty ~f:(
    fun accu -> function
    | [ db_id ; collection ; jaspar_id ; _ ; factor_name ] ->
      String.Map.add accu ~key:db_id ~data:(object
	method collection = collection_of_string collection
	method jaspar_id = jaspar_id
	method factor_name = factor_name
      end)
    | _ -> assert false
  )

let load_matrix_data fn =
  let parse = function
    | [ id ; base ; col ; count ] ->
       let col = int_of_string col in
       object method id = id method base = base method col = col method count = count end
    | _ -> assert false
  in
  let vector_of_lines l =
    List.sort ~cmp:(fun l1 l2 -> String.compare l1#base l2#base) l
    |> List.map ~f:(fun l -> Int.of_float (Float.of_string l#count))
    |> Array.of_list
  in
  let matrix_of_lines l =
    let id = (List.hd_exn l)#id in
    let matrix =
      List.sort l ~cmp:(fun x y -> compare x#col y#col)
      |> List.group ~break:(fun l1 l2 -> l1#col <> l2#col)
      |> List.map ~f:vector_of_lines
      |> Array.of_list
    in
    id, matrix
  in
  let data = In_channel.with_file (fn / "MATRIX_DATA.txt") ~f:(fun ic ->
    Lines.of_channel ic
    |> Stream.skip ~n:1
    |> Stream.map ~f:(Line.split ~on:'\t')
    |> Stream.to_list
    |> List.sort ~cmp:(fun x y -> compare (List.hd x) (List.hd y))
    |> List.group ~break:(fun x y -> List.hd x<> List.hd y)
    |> List.map ~f:(List.map ~f:parse)
    |> List.map ~f:matrix_of_lines
  )
  in
  String.Map.of_alist_exn data


module SS = struct
  include Core_kernel.Tuple.Make(String)(String)
  include Core_kernel.Tuple.Comparable(String)(String)
end

module SSM = Map.Make(SS)

let load_annotation fn =
  fold_data_file (fn / "MATRIX_ANNOTATION.txt") ~init:SSM.empty ~f:(fun accu ->
    function
    | id :: field :: data :: _ -> SSM.add accu ~key:(id, field) ~data
    | _ -> assert false
  )

let load fn =
  let matrix = load_matrix fn in
  let matrix_data = load_matrix_data fn in
  let annotations = load_annotation fn in
  let res = String.Map.mapi matrix ~f:(fun ~key ~data -> {
    id = key ;
    jaspar_id = data#jaspar_id ;
    collection = data#collection ;
    factor_name = data#factor_name ;
    factor_class = SSM.find_exn annotations (key, "class") ;
    comment = (
      match SSM.find annotations (key, "comment") with
      | Some "-" -> None
      | x -> x
    ) ;
    family = SSM.find annotations (key, "family")  ;
    medline = SSM.find_exn annotations (key, "medline") ;
    matrix = String.Map.find_exn matrix_data key ;
  })
  in
  String.Map.data res
