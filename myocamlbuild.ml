open Ocamlbuild_plugin
open Solvuu_build

module Project = struct
  let name = "biocaml"
  let version = "dev"
  let info = Info.of_list [
      {
        Info.name = `Lib "unix";
        libs = [];
        pkgs = ["camlzip" ; "cfstream" ; "core" ;
                "future.unix" ;
                "ppx_compare" ; "ppx_sexp_conv" ; "re.perl" ;
                "uri" ; "xmlm" ];
        build_if = [];
      };

      {
        Info.name = `Lib "async";
        libs = ["unix"];
        pkgs = ["async"; "future.async"];
        build_if = [`Pkgs_installed];
      };

      {
        Info.name = `Lib "lwt";
        libs = ["unix"];
        pkgs = ["lwt"; "future.lwt"];
        build_if = [`Pkgs_installed];
      };

      {
        Info.name = `Lib "benchmark";
        libs = ["unix"];
        pkgs = [];
        build_if = [`Pkgs_installed];
      };

      {
        Info.name = `Lib "ez";
        libs = ["unix"];
        pkgs = [];
        build_if = [`Pkgs_installed];
      };

      {
        Info.name = `Lib "test";
        libs = ["unix"];
        pkgs = ["oUnit"];
        build_if = [`Pkgs_installed];
      };

      {
        Info.name = `App "biocaml_run_benchmarks";
        libs = ["benchmark"];
        pkgs = [];
        build_if = [`Pkgs_installed];
      };

      {
        Info.name = `App "biocaml_run_tests";
        libs = ["test"];
        pkgs = [];
        build_if = [`Pkgs_installed];
      };
    ]

  let ocamlinit_postfix = []

end

module Base =  Make(Project)

let plugin = function
  | Before_options ->
    Base.plugin Before_options ;
    Ocamlbuild_pack.Configuration.parse_string "true: -safe_string"
  | x -> Base.plugin x

let () = dispatch plugin
