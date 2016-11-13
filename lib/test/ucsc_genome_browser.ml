open Core_kernel.Std
module UGB = Biocaml_base.Std.Ucsc_genome_browser
open OUnit

let test_track_line_to_string_cases = [
  "track type=bigBed name=\"bigBed Example One\" description=\"A bigBed file\" bigDataUrl=http://genome.ucsc.edu/goldenPath/help/examples/bigBedExample.bb",
  [ `type_ "bigBed" ; `name "bigBed Example One" ; `description "A bigBed file" ; `bigDataUrl "http://genome.ucsc.edu/goldenPath/help/examples/bigBedExample.bb" ] ;
]

let test_track_line_to_string () =
  List.iter test_track_line_to_string_cases ~f:(fun (answer, opts) ->
      assert_equal ~printer:ident answer (UGB.track_line opts)
    )

let tests = "Ucsc_genome_browser" >::: [
    "Track line printer" >:: test_track_line_to_string ;
  ]
