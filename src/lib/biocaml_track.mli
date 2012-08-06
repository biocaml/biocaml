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
type 'a content = [
| `content of 'a
]

type parse_error =
[ `incomplete_input of Biocaml_pos.t * string list * string option
| `wrong_browser_position of Biocaml_pos.t * string
| `wrong_key_value_format of (string * string) list * string * string ]

val parser: ?filename:string -> unit ->
  (string, [ t | string content ], parse_error) Biocaml_transform.t

type wig_parser_error = [ parse_error | Biocaml_wig.parse_error ]

val wig_parser: ?filename:string -> unit ->
  (string, [ t | Biocaml_wig.t ], wig_parser_error) Biocaml_transform.t

    
(*
module TrackLine : sig
  type t
      (** Type of a track line. *)

  exception Bad of string
    
  val of_string : string -> t
    (** Parse string to track line. Must be in format as defined by UCSC. *)
    
  val to_string : t -> string
    (** Return string representing track line in required format. *)
    
  val empty : t
    (** Empty track line. *)
    
  val set : t -> string -> string -> t
    (** [set t a x] sets attribute [a] to value [x] in [t]. Value always given as string since [set] is a general function for setting any attribute. Unrecognized attributes are added without error checking, but [Bad] is raised if known attribute's value does not meet requirements specified above. Any previous value of [a] is overwritten. *)
    
  val unset : t -> string -> t
    (** [unset t a] deletes attribute [a] from [t]. Does nothing if attribute not in [t]. *)

  val find : t -> string -> string
    (** [find t a] returns the value of attribute [a], or raises [Not_found] if no such attribute. *)
    
  val mem : t -> string -> string -> bool
    (** [mem t a x] returns true if [a] is assigned value [x] in [t]. *)
    
  val to_list : t -> (string * string) list
    (** Return list of attribute=value pairs. *)

  val of_list : (string * string) list -> t
    (** [of_list l] sets [a] to [x] for every [(a,x)] pair in [l]. Raise [Bad] if any invalid values given. If attribute [a] given more than once, last will apply. *)

  val valid_wig : t -> bool
    (** Returns true if [t] is a valid wiggle track line. *)

end

(** Browser lines configure the overall display of the Genome Browser when your file is uploaded. *)
module BrowserLines : sig
  type t
      (** Type of a sequence of browser lines. *)
      
  exception Bad of string

  val position : string -> int -> int -> t
    (** [position chr start end] returns the browser line "chr:start-end". Raise [Bad] if [start] not less than [end]. *)
    
  val hide : string -> t
  val dense : string -> t
  val pack : string -> t
  val squish : string -> t
  val full : string -> t
    (** [hide], [dense], [pack], [squish], and [full] each take a string that should either be "all" or a space separated list of track names. *)

  val concat : t -> t -> t
    (** [concat t1 t2] returns a type representing the lines [t1] followed by [t2]. Raise [Bad] if concatentation would lead to ill-formed browser lines. *)
    
  val of_string : string -> t
    (** Creates a list of lines by splitting given string on newline characters. Raise [Bad] if result is ill-formed. *)

  val to_string : t -> string
    (** Return string representation of browser lines. All but last line will be followed by newline. *)
end
 (* 
(** A block of information. *)
type block =
    | B of BrowserLines.t (** sequence of browser lines *)
    | T of TrackLine.t    (** a track line *)
    | C of Biocaml_comments.t     (** one or more comment lines or blank lines *)
    | Wig of Biocaml_wig.t        (** WIG data section *)
    (* | Bed of Bed.t *)       (** WIG data section *)

type t
    (** Type of a track file. Can be thought of as a list of blocks with certain restrictions on the order in which blocks occur. *)

exception Bad of string

val to_channel : ?wig_fmt:Biocaml_wig.format -> t -> out_channel -> unit
val to_file : ?wig_fmt:Biocaml_wig.format -> t -> string -> unit
val to_list : t -> block list
val of_list : block list -> t
 *)
*)
