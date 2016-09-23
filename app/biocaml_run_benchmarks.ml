open Core.Std
open Biocaml_unix.Std
open Biocaml_benchmark

let () =
  Command.(
    let whole_thing =
      group ~summary:"Biocaml's benchmarks" [
        ("linesplit", Line_split.command);
      ] in
    run ~version:About.version whole_thing
  )
