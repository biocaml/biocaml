open Printf

type assembly =
  [ `dm3
  | `droSim1
  | `hg18
  | `hg19
  | `hg38
  | `mm8
  | `mm9
  | `mm10
  | `sacCer2
  ]

let string_of_assembly = function
  | `dm3 -> "dm3"
  | `droSim1 -> "droSim1"
  | `hg18 -> "hg18"
  | `hg19 -> "hg19"
  | `hg38 -> "hg38"
  | `mm8 -> "mm8"
  | `mm9 -> "mm9"
  | `mm10 -> "mm10"
  | `sacCer2 -> "sacCer2"
;;

type track_attribute =
  [ `name of string
  | `description of string
  | `type_ of track_type
  | `visibility of [ `hide | `full | `dense | `pack | `squish ]
  | `color of color
  | `itemRgb of bool
  | `colorByStrand of color * color
  | `useScore of bool
  | `group of string
  | `priority of int
  | `db of assembly
  | `offset of int
  | `maxItems of int
  | `url of string
  | `htmlUrl of string
  | `bigDataUrl of string
  ]

and color = int * int * int

and track_type =
  [ `bam
  | `bedDetail
  | `bedGraph
  | `bigBed
  | `bigWig
  | `broadPeak
  | `narrowPeak
  | `array
  | `vcf
  | `wig
  ]

let string_of_track_type = function
  | `bam -> "bam"
  | `bedDetail -> "bedDetail"
  | `bedGraph -> "bedGraph"
  | `bigBed -> "bigBed"
  | `bigWig -> "bigWig"
  | `broadPeak -> "broadPeak"
  | `narrowPeak -> "narrowPeak"
  | `array -> "array"
  | `vcf -> "vcf"
  | `wig -> "wig"
;;

let unparse_track_attribute buf = function
  | `name n -> bprintf buf " name=\"%s\"" n
  | `description d -> bprintf buf " description=\"%s\"" d
  | `type_ t -> bprintf buf " type=%s" (string_of_track_type t)
  | `visibility v ->
    let v =
      match v with
      | `full -> "full"
      | `dense -> "dense"
      | `hide -> "hide"
      | `pack -> "pack"
      | `squish -> "squish"
    in
    bprintf buf " visibility=%s" v
  | `color (r, g, b) -> bprintf buf " color=%d,%d,%d" r g b
  | `itemRgb b -> if b then bprintf buf " itemRgb=On"
  | `colorByStrand ((r, g, b), (r', g', b')) ->
    bprintf buf " color=%d,%d,%d %d,%d,%d" r g b r' g' b'
  | `useScore b -> bprintf buf " useScore=%d" (if b then 1 else 0)
  | `group g -> bprintf buf " group=\"%s\"" g
  | `priority p -> bprintf buf " priority=%d" p
  | `db assembly -> bprintf buf " db=%s" (string_of_assembly assembly)
  | `offset o -> bprintf buf " offset=%d" o
  | `maxItems m -> bprintf buf " maxItems=%d" m
  | `url u -> bprintf buf " url=%s" u
  | `htmlUrl u -> bprintf buf " htmlUrl=%s" u
  | `bigDataUrl u -> bprintf buf " bigDataUrl=%s" u
;;

let track_line opts =
  let buf = Buffer.create 1024 in
  bprintf buf "track";
  List.iter ~f:(unparse_track_attribute buf) opts;
  Buffer.contents buf
;;

type url_param =
  [ `pix of int
  | `hgt_labelWidth of int
  | `textSize of int
  ]

let main_server = "http://genome.ucsc.edu"
let base db = main_server ^ "/cgi-bin/hgTracks?db=" ^ string_of_assembly db

let string_of_position (chr, maybe_pos) =
  chr
  ^
  match maybe_pos with
  | None -> ""
  | Some (a, b) -> sprintf ":%d-%d" a b
;;

let encode_url_param = function
  | `pix n -> sprintf "pix=%d" n
  | `hgt_labelWidth n -> sprintf "hgt.labelWidth=%d" n
  | `textSize n -> sprintf "textSize=%d" n
;;

let encode_url_params xs = List.map ~f:encode_url_param xs |> String.concat ~sep:"&"

let custom_track_url ?(params = []) ~db ~position ~data_url () =
  sprintf
    "%s&position=%s&hgt.customText=%s%s"
    (base db)
    (string_of_position position)
    data_url
    (encode_url_params params)
;;

let bigData_custom_track_url ?(params = []) ~db ~position ~track () =
  let escaped_custom_text = Uri.pct_encode ~component:`Query (track_line track) in
  sprintf
    "%s&position=%s&hgct_customText=%s%s"
    (base db)
    (string_of_position position)
    escaped_custom_text
    (encode_url_params params)
;;

module Test = struct
  let test_track_line_to_string_cases =
    [ ( "track type=bigBed name=\"bigBed Example One\" description=\"A bigBed file\" \
         bigDataUrl=http://genome.ucsc.edu/goldenPath/help/examples/bigBedExample.bb"
      , [ `type_ `bigBed
        ; `name "bigBed Example One"
        ; `description "A bigBed file"
        ; `bigDataUrl "http://genome.ucsc.edu/goldenPath/help/examples/bigBedExample.bb"
        ] )
    ]
  ;;

  let%test _ =
    test_track_line_to_string_cases
    |> List.for_all ~f:(fun (answer, opts) -> String.equal answer (track_line opts))
  ;;

  let test_custom_track_url_cases =
    [ ( "http://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&position=chr22&hgt.customText=http://genome.ucsc.edu/goldenPath/help/test.bed"
      , (`hg19, ("chr22", None), "http://genome.ucsc.edu/goldenPath/help/test.bed") )
    ]
  ;;

  let%test _ =
    test_custom_track_url_cases
    |> List.for_all ~f:(fun (answer, (db, position, data_url)) ->
         let generated_url = custom_track_url ~db ~position ~data_url () in
         String.equal answer generated_url)
  ;;

  let test_bigData_custom_track_url_cases =
    [ ( "http://genome.ucsc.edu/cgi-bin/hgTracks?db=hg18&position=chr21:33038447-33041505&hgct_customText=track%20type=bigBed%20name=%22myBigBedTrack%22%20description=%22a%20bigBed%20track%22%20visibility=full%20bigDataUrl=http://genome.ucsc.edu/goldenPath/help/examples/bigBedExample.bb"
      , ( `hg18
        , ("chr21", Some (33038447, 33041505))
        , [ `type_ `bigBed
          ; `name "myBigBedTrack"
          ; `description "a bigBed track"
          ; `visibility `full
          ; `bigDataUrl "http://genome.ucsc.edu/goldenPath/help/examples/bigBedExample.bb"
          ] ) )
    ]
  ;;

  let%test _ =
    List.for_all
      test_bigData_custom_track_url_cases
      ~f:(fun (answer, (db, position, track)) ->
      let generated_url = bigData_custom_track_url ~db ~position ~track () in
      String.equal answer generated_url)
  ;;
end
