
(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let mydispatch = function
  | After_options ->
     Options.ocamldoc := S[!Options.ocamldoc;
                           A"-thread"; A"-charset"; A"utf-8"; A"-sort" ]
  | _ -> ()

let () =
  dispatch (fun e ->
            dispatch_default e;
            mydispatch e)
