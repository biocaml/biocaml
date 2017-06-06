open Core_kernel.Std
module UGB = Biocaml_base.Ucsc_genome_browser
open OUnit

let test_track_line_to_string_cases = [
  "track type=bigBed name=\"bigBed Example One\" description=\"A bigBed file\" bigDataUrl=http://genome.ucsc.edu/goldenPath/help/examples/bigBedExample.bb",
  [ `type_ `bigBed ; `name "bigBed Example One" ; `description "A bigBed file" ; `bigDataUrl "http://genome.ucsc.edu/goldenPath/help/examples/bigBedExample.bb" ] ;
]

let test_track_line_to_string () =
  List.iter test_track_line_to_string_cases ~f:(fun (answer, opts) ->
      assert_equal ~printer:ident answer (UGB.track_line opts)
    )

let test_custom_track_url_cases = [
  "http://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&position=chr22&hgt.customText=http://genome.ucsc.edu/goldenPath/help/test.bed",
  (`hg19, ("chr22", None), "http://genome.ucsc.edu/goldenPath/help/test.bed") ;
]

let test_custom_track_url () =
  List.iter test_custom_track_url_cases ~f:(fun (answer, (db, position, data_url)) ->
      let generated_url = UGB.custom_track_url ~db ~position ~data_url () in
      assert_equal ~printer:ident answer generated_url
    )


let test_bigData_custom_track_url_cases = [
  "http://genome.ucsc.edu/cgi-bin/hgTracks?db=hg18&position=chr21:33038447-33041505&hgct_customText=track%20type=bigBed%20name=%22myBigBedTrack%22%20description=%22a%20bigBed%20track%22%20visibility=full%20bigDataUrl=http://genome.ucsc.edu/goldenPath/help/examples/bigBedExample.bb",
  (`hg18, ("chr21", Some (33038447, 33041505)), [ `type_ `bigBed ; `name "myBigBedTrack" ; `description "a bigBed track" ; `visibility `full ; `bigDataUrl "http://genome.ucsc.edu/goldenPath/help/examples/bigBedExample.bb"]) ;
]

let test_bigData_custom_track_url () =
  List.iter test_bigData_custom_track_url_cases ~f:(fun (answer, (db, position, track)) ->
      let generated_url = UGB.bigData_custom_track_url ~db ~position ~track () in
      assert_equal ~printer:ident answer generated_url
    )


let tests = "Ucsc_genome_browser" >::: [
    "Track line printer" >:: test_track_line_to_string ;
    "Custom track URL" >:: test_custom_track_url ;
    "bigBed custom track URL" >:: test_bigData_custom_track_url ;
  ]
