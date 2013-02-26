(** Track files in UCSC Genome Browser format. The following
    documentation assumes knowledge of concepts explained on the UCSC
    Genome Browser's website. Basically, a track file is one of several
    types of data (WIG, GFF, etc.), possibly preceded by comments, browser
    lines, and a track line. This module allows only a single data track
    within a file, although the UCSC specifies that multiple tracks may be
    provided together. *)

(** Track lines define display attributes. They can be thought of as
    lists of attribute-value pairs. For forward compatibility, this module
    allows arbitrary attributes to be set. However, the following lists
    known attributes and specifies restrictions on their values:
    - [name] - string enclosed in double quotes if there are spaces;
      recommended to use only alphanumeric characters and length less
      than 15 characters but this is not enforced
    - [description] - string enclosed in double quotes if there are
      spaces; recommended to use only alphanumeric characters and
      length less than 60 characters but this is not enforced
    - [visibility] - "hide", "dense", "full", "pack", or "squish"; if
      [type=wiggle_0] then "hide", "dense", or "full"; can also use
      integer value where 0 = "hide", 1 = "dense", 2 = "full", 3 =
      "pack", 4 = "squish"
    - [color] - "R,G,B" where each of R, G, and B are integers between
      0 and 255
    - [altColor] - "R,G,B" where each of R, G, and B are integers
      between 0 and 255
    - [itemRgb] - "On" is the only allowed value
    - [useScore] - 0 or 1
    - [group] - string
    - [priority] - integer
    - [autoScale] - "on" or "off"
    - [gridDefault] - "on" or "off"
    - [maxHeightPixels] - "max:default:min" where each of max,
      default, and min are integers
    - [graphType] - "bar" or "points"
    - [viewLimits] - "lower:upper" where each of lower and upper are
      integers
    - [yLineMark] - floating point value
    - [yLineOnOff] - "on" or "off"
    - [windowingFunction] - "maximum", "mean", or "minimum"
    - [smoothingWindow] - "off" or an integer between 2 and 16
    - [db] - should be a valid UCSC assembly ID, but currently any
      string is accepted
    - [offset] - integer
    - [url] - string
    - [htmlUrl] - string
    - [type] - "wiggle_0" is the only value currently supported,
      leaving this attribute unset handles other track types
*)

type t = [
| `track of (string * string) list
| `comment of string
| `browser of
    [ `position of string * int * int | `hide of [`all] | `unknown of string ]
]
(** The type of the parser "track" lines. *)

type 'a content = [
| `content of 'a
]
(** The "content" lines of the files. *)

type parse_error =
[ `incomplete_input of Biocaml_pos.t * string list * string option
| `wrong_browser_position of Biocaml_pos.t * string
| `wrong_key_value_format of (string * string) list * string * string ]
(** The possible parsing errors. *)

(** {2 Low-level transforms.} *)

module Transform: sig
  (** Low-level transforms. *)

  val string_to_string_content: ?filename:string -> unit ->
    (string, ([ t | string content ], [> parse_error]) Core.Result.t)
      Biocaml_transform.t
  (** Create a parser that gets the "track", comment, and "browser"
      lines and puts the  other lines in [`content _]. *)

  val string_content_to_string:
    ?add_content_new_line:bool ->
    unit ->
    ([ t | string content ], string) Biocaml_transform.t
  (** Create a printer for track files containing [`content line] lines. *)

  val string_to_wig: ?filename:string -> unit ->
    (string,
     ([ t | Biocaml_wig.t ], [> parse_error | Biocaml_wig.parse_error ])
       Core.Result.t)
      Biocaml_transform.t
  (** Create a composite parser for UCSC WIG files.  *)

  val wig_to_string: unit ->
    ([ t | Biocaml_wig.t ], string) Biocaml_transform.t
  (** Create a printer for track files containing WIG lines. *)

  val string_to_gff: ?filename:string -> ?tags: Biocaml_gff.tag list -> unit ->
    (string,
     ([t | Biocaml_gff.stream_item], [> parse_error | Biocaml_gff.parse_error])
       Core.Result.t) Biocaml_transform.t
  (** Create a composite parser for UCSC GFF files.  *)

  val gff_to_string: ?tags: Biocaml_gff.tag list -> unit ->
    ([ t | Biocaml_gff.stream_item ], string) Biocaml_transform.t
  (** Create a printer for track files containing GFF lines. *)

  val string_to_bed: ?filename:string ->
    ?more_columns:Biocaml_bed.parsing_spec -> unit ->
    (string,
     ([t | Biocaml_bed.item content], [> parse_error | Biocaml_bed.Error.parsing ])
       Core.Result.t) Biocaml_transform.t
  (** Create a composite parser for UCSC Bed(Graph) files.  *)

  val bed_to_string: unit ->
    ([ t | Biocaml_bed.item content ], string) Biocaml_transform.t
  (** Create a printer for track files containing Bed(Graph) lines. *)

end

