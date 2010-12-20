(** Tabular data. Several variations can be specified:

    - Initial comment lines can be (dis)allowed (default =
    allowed).

    - The character defining the start of a comment line can be
    specified (default = '#').

    - A header row can be optionally provided giving the column
    names. If not provided, the names are "0", "1", "2",
    etc. (Default: no header row.)

    - If there is a header row, it can be followed by another row with
    only spaces and dashes.

    - The column separator can be specified (default = "\t").

*)

open Batteries_uni

exception Invalid of string

type row
    (** Data row. *)
    
type getter = row -> string -> string
  (** A function [get] of type [getter] can be used as [get r c] to
      get the value of the column named [c] in row [r]. Raise
      [Failure] if given row does not have a value for column [c]. *)

type columns = string list
    (** All column names occurring in a file, in order that they occur. *)

val enum_input : ?itags:string -> IO.input -> (Comments.t option * columns * getter * row Enum.t)
  (** Returns:
      - comments if itags specified there are any, or None otherwise
      - column names
      - the getter function
      - enumeration of the data rows

      Default itags is:
      "table,comment-char=#,header,header_,separator=\t".

      Raises [Invalid] if [input] does not conform to [itags]
      requirements. Raises [Tags.Invalid] if [itags] are ill-formed or
      invalid for this function. *)
