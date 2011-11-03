open Batteries

let err_enum ?(source="") e =
  let n = ref 0 in
  Enum.make
    ~next:(
      fun () ->
        incr n;
        (try
            match Enum.get e with
              | None -> raise Enum.No_more_elements
              | Some x -> x
          with
            | Enum.No_more_elements -> raise Enum.No_more_elements
            | exn -> raise (Biocaml_common.PosError(source, !n, -1, exn)))
    )
    ~count:(fun () -> Enum.count e)
    ~clone:(fun () -> Enum.clone e)
