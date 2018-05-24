open Printf

type assembly = [ `dm3 | `droSim1 | `hg18 | `hg19 | `hg38 | `mm8 | `mm9 | `mm10 | `sacCer2 ]

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

let unparse_track_attribute buf = function
  | `name n -> bprintf buf " name=\"%s\"" n
  | `description d -> bprintf buf " description=\"%s\"" d
  | `type_ t -> bprintf buf " type=%s" (string_of_track_type t)
  | `visibility v ->
    let v = match v with
      | `full -> "full"
      | `dense -> "dense"
      | `hide -> "hide"
      | `pack -> "pack"
      | `squish -> "squish"
    in
    bprintf buf " visibility=%s" v
  | `color (r,g,b) ->
    bprintf buf " color=%d,%d,%d" r g b
  | `itemRgb b ->
    if b then bprintf buf " itemRgb=On"
  | `colorByStrand ((r,g,b), (r',g',b')) ->
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

let track_line opts =
  let buf = Buffer.create 1024 in
  bprintf buf "track" ;
  List.iter (unparse_track_attribute buf) opts ;
  Buffer.contents buf

type url_param = [
  | `pix of int
  | `hgt_labelWidth of int
  | `textSize of int
]

let main_server = "http://genome.ucsc.edu"

let base db =
  main_server ^ "/cgi-bin/hgTracks?db=" ^ (string_of_assembly db)

let string_of_position (chr, maybe_pos) =
  chr ^ (
    match maybe_pos with
    | None -> ""
    | Some (a, b) -> sprintf ":%d-%d" a b
  )

let encode_url_param = function
  | `pix n -> sprintf "pix=%d" n
  | `hgt_labelWidth n -> sprintf "hgt.labelWidth=%d" n
  | `textSize n -> sprintf "textSize=%d" n

let encode_url_params xs =
  List.map encode_url_param xs
  |> String.concat "&"

let custom_track_url ?(params = []) ~db ~position ~data_url () =
  sprintf
    "%s&position=%s&hgt.customText=%s%s"
    (base db)
    (string_of_position position)
    data_url
    (encode_url_params params)

let bigData_custom_track_url ?(params = []) ~db ~position ~track () =
  let escaped_custom_text =
    Uri.pct_encode ~component:`Query (track_line track)
  in
  sprintf "%s&position=%s&hgct_customText=%s%s"
    (base db)
    (string_of_position position)
    escaped_custom_text
    (encode_url_params params)
