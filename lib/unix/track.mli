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
open Core_kernel

(** {2 Item Types} *)

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

(** {2 Error Types} *)

module Error: sig
  type parsing =
    [ `incomplete_input of Pos.t * string list * string option
    | `wrong_browser_position of Pos.t * string
    | `wrong_key_value_format of (string * string) list * string * string ]
  (** The parsing errors that can happen while parsing Track-specific
      content. *)

  type t = parsing
  (** The union of all the errors. *)

  val parsing_of_sexp: Sexplib.Sexp.t -> parsing
  val sexp_of_parsing: parsing -> Sexplib.Sexp.t
  val t_of_sexp: Sexplib.Sexp.t -> t
  val sexp_of_t: t -> Sexplib.Sexp.t
end

(** {2 Low-level transforms.} *)

module Transform: sig
  (** Low-level transforms. *)

  val string_to_string_content: ?filename:string -> unit ->
    (string, ([ t | string content ], [> Error.parsing]) Result.t)
      Tfxm.t
  (** Create a parser that gets the "track", comment, and "browser"
      lines and puts the  other lines in [`content _]. *)

  val string_content_to_string:
    ?add_content_new_line:bool ->
    unit ->
    ([ t | string content ], string) Tfxm.t
  (** Create a printer for track files containing [`content line] lines. *)

  val string_to_wig: ?filename:string -> unit ->
    (string,
     ([ t | Wig.item ], [> Error.parsing | Wig.Error.parsing ])
       Result.t)
      Tfxm.t
  (** Create a composite parser for UCSC WIG files.  *)

  val wig_to_string: unit ->
    ([ t | Wig.item ], string) Tfxm.t
  (** Create a printer for track files containing WIG lines. *)

  val string_to_gff: ?filename:string -> tags: Gff.Tags.t -> unit ->
    (string,
     ([t | Gff.item], [> Error.parsing | Gff.Error.parsing])
       Result.t) Tfxm.t
  (** Create a composite parser for UCSC GFF files.  *)

  val gff_to_string: tags: Gff.Tags.t -> unit ->
    ([ t | Gff.item ], string) Tfxm.t
  (** Create a printer for track files containing GFF lines. *)

  val string_to_bed: ?filename:string ->
    ?more_columns:Bed.parsing_spec -> unit ->
    (string,
     ([t | Bed.item content], [> Error.parsing | Bed.Error.parsing ])
       Result.t) Tfxm.t
  (** Create a composite parser for UCSC Bed(Graph) files.  *)

  val bed_to_string: unit ->
    ([ t | Bed.item content ], string) Tfxm.t
  (** Create a printer for track files containing Bed(Graph) lines. *)

end
