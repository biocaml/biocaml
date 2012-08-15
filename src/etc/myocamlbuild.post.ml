(* Appended to myocamlbuild.ml, right after (* OASIS_STOP *) *)

let mydispatch = function
  | After_options ->
     Options.ocamldoc := S[!Options.ocamldoc;
                           A"-thread"; A"-charset"; A"utf-8"; A"-sort" ]
  | _ -> ()

let () =
  dispatch
    (MyOCamlbuildBase.dispatch_combine [dispatch_default; mydispatch])
