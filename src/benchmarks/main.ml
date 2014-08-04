
open Core.Std

let () =
  Command.(
    let whole_thing =
      group ~summary:"Biocaml's benchmarks" [
        ("zip", Benchmark_zip.command);
        ("bamsam", Bam_sam_and_the_gc.command);
        ("bamcount", Bam_count_alignments.command);
      ] in
    run ~version:Biocaml_about.version whole_thing
  )

