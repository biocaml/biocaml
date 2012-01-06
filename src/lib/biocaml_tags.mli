(** Classification of data formats. A data format is specified by a
    list of tags, each of which has an attribute name and a value of some
    type. Currently supported tags are:

    - table <bool> - true means data is in Table format

    - bed <bool> - true means data is in Bed format

    - header <bool> - true means column names are provided in a header
    row

    - header_ <bool> - true means header row is followed by a row with
    just underscores and spaces

    - comment-char <char> - providing a value y means data begins with
    comment lines, each of which begins with character y or is all
    whitespace

    - separator <char> - value is the character separating column
    values within a row

    The format for a list of tags is:
    {v
    x1 = y1, x2 = y2, x3 = y3, ...
    v}

    where extra spaces around the equal and comma symbols are
    disregarded.
    
    Not all combinations of tags are valid. In particular, some tags
    are 'principal' tags and none can be used with the others. The
    principal tags are: table, bed.

    header_=true is allowed only if header=true.

    bed=true disallows: header, header_, comment-char, and separator.

*)

open Batteries
  
exception Invalid of string
  (** Raised when an invalid tag or incompatible list of tags is encounted. *)
  
type t
    (** Collection of tags, which classify a data format. *)
    
val of_string : string -> t
  (** Parse given string as list of tags. Raise [Invalid] if any tags
      invalid, or if list of tags are incompatible. *)

val mem : string -> t -> bool
  (** [mem x t] returns true if [t] contains a value for tag [x]. *)

val find : string -> t -> string
  (** [find x t] returns the value of tag [x] in [t]. Raise
      [Not_found] if [t] does not contain value for [x]. *)

val tag_is : string -> string -> t -> bool
  (** [tag_is x y t] returns true if [x]=[y] in [t], or false if tag
      [x] assigned some other value or not found in [t]. *)
