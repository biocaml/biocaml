open Core_kernel

include Result

module List = struct

  let mapi (type error) l ~(f:(int -> 'a -> ('b, error) t)) =
    let module M = struct
      exception E of error
      let the_fun () =
        let run () =
          List.mapi l ~f:(fun i x ->
              match f i x with
              | Ok o -> o
              | Error e -> raise (E e))
        in
        try Ok (run ())
        with
        | E e -> Error e
    end in
    M.the_fun ()

  let map l ~f = mapi l ~f:(fun _ x -> f x)

end
