type assembly = [ `dm3 | `hg18 | `hg19 | `hg38 | `mm8 | `mm9 | `mm10 | `sacCer2 ]

val string_of_assembly : [< assembly] -> string

type track_attribute = [
  | `name of string
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
and track_type = [
    `bam
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

val track_line : track_attribute list -> string

type url_param = [
  | `pix of int
  | `hgt_labelWidth of int
  | `textSize of int
]

val custom_track_url :
  ?params:url_param list ->
  db:[< assembly] ->
  position:string * (int * int) option ->
  data_url:string ->
  unit -> string

val bigData_custom_track_url :
  ?params:url_param list ->
  db:[< assembly] ->
  position:string * (int * int) option ->
  track:track_attribute list ->
  unit -> string

