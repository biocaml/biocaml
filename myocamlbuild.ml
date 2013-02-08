
(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let doc_intro = "src/doc/intro.txt"
let doc_title = "Biocaml API Reference"

let mydispatch = function
  | After_options ->
     Options.ocamldoc := S[!Options.ocamldoc;
                           A"-thread"; A"-charset"; A"utf-8"; A"-sort" ]
  | After_rules ->
    dep ["ocaml"; "doc"; "extension:html"] & [doc_intro];
    flag ["ocaml"; "doc"; "extension:html"] &
      (S[
        A"-t"; A doc_title;
        A"-intro"; P doc_intro;
      ])
  | _ -> ()

let () =
  dispatch (fun e ->
            dispatch_default e;
            mydispatch e)
