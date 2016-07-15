open Printf
open Solvuu_build.Std
open Solvuu_build.Util

let project_name = "biocaml"
let version = "dev"

let annot = ()
let bin_annot = ()
let g = ()
let short_paths = ()
let thread = ()
let w = "A-4-33-41-42-44-45-48"

let lib ?findlib_deps ?internal_deps ?build_if ?ml_files lib_name
  : Project.item
  =
  Project.lib (sprintf "%s_%s" project_name lib_name)
    ~annot ~bin_annot ~g ~short_paths ~thread ~w
    ~pkg:(sprintf "%s.%s" project_name lib_name)
    ~dir:(sprintf "lib/%s" lib_name)
    ~pack_name:(sprintf "%s_%s" project_name lib_name)
    ?findlib_deps
    ?internal_deps
    ?ml_files

let app ?internal_deps name : Project.item =
  Project.app name
    ~annot ~bin_annot ~g ~short_paths ~thread ~w
    ~file:(sprintf "app/%s.ml" name)
    ?internal_deps

let unix = lib "unix"
    ~findlib_deps:["camlzip"; "cfstream"; "core";
                   "future.unix"; "ppx_compare"; "ppx_sexp_conv"; "re.perl";
                   "uri"; "xmlm"
                  ]
    ~ml_files:(`Add ["about.ml"])

let async = lib "async"
    ~internal_deps:[unix]
    ~findlib_deps:["async"; "future.async"]

let lwt = lib "lwt"
    ~internal_deps:[unix]
    ~findlib_deps:["lwt"; "future.lwt"]

let ez = lib "ez"
    ~internal_deps:[unix]
    ~findlib_deps:[]

let benchmark = lib "benchmark"
    ~internal_deps:[unix]
    ~findlib_deps:["core_bench" ; "containers" ; "sosa"]

let test = lib "test"
    ~internal_deps:[unix]
    ~findlib_deps:["oUnit"]

let run_benchmarks = app "biocaml_run_benchmarks"
    ~internal_deps:[benchmark]

let run_tests = app "biocaml_run_tests"
    ~internal_deps:[test]

let optional_pkgs = [
  "async"; "lwt";
  "core_bench"; "containers"; "sosa";
  "oUnit";
]

let items =
  [
    unix; async; lwt; ez; benchmark; test;
    run_benchmarks; run_tests;
  ]
  |> List.filter ~f:(fun x -> Project.dep_opts_sat x optional_pkgs)

;;
let () =
  let open Solvuu_build.Std.Project in

  (* Compute graph to check for cycles and other errors. *)
  ignore (Graph.of_list items);

  let libs = filter_libs items in
  let apps = filter_apps items in

  Ocamlbuild_plugin.dispatch @@ function
  | Ocamlbuild_plugin.After_rules -> (
      Ocamlbuild_plugin.clear_rules();

      Tools.m4_rule ()
        ~_D:[
          "GIT_COMMIT", Some (match Tools.git_last_commit() with
            | None -> "None"
            | Some x -> sprintf "Some \"%s\"" x
          );
          "VERSION", Some version;
        ];

      List.iter libs ~f:build_lib;
      List.iter apps ~f:build_app;

      build_static_file ".merlin" (merlin_file items);
      build_static_file ".ocamlinit"
        (ocamlinit_file items ~postfix:["open Biocaml_unix.Std"]);
      build_static_file "project.mk"
        (makefile items ~project_name);
      Findlib.build_meta_file (meta_file ~version libs);
      build_static_file (sprintf "%s.install" project_name)
        (install_file items);
    )
  | _ -> ()
