(** Tabular data as defined by principal tag 'table'. Design
    emphasizes support for large tables. Additional tags supported
    are: header, header_, comment-char, separator. By default, parsers
    assume input format is:

    {v    "table,comment-char=#,header,header_,separator=\t". v}

    If header=false, then column names are assigned the values: "0",
    "1", "2", etc.
*)

open Batteries_uni

exception Invalid of string

exception No_column of string
  
type row
    (** Data row. *)
    
type getter = row -> string -> string
  (** A function [get] of type [getter] can be used as [get r c] to
      get the value of the column named [c] in row [r]. Raise
      [No_column c] if given row does not have a value for column [c]. *)

type columns = string list
    (** All column names occurring in a file, in order that they occur. *)

type t = Comments.t option * columns * getter * row Enum.t
    (** A table consists of:
        - comments (optional)
        - column names
        - a getter function
        - an enumeration of the rows in the table
    *)

val of_input : ?itags:string -> IO.input -> t
  (** Default itags is:

      {v "table,comment-char=#,header,header_,separator=\t" v}

      Raises [Invalid] if [input] does not conform to [itags]
      requirements. Raises [Tags.Invalid] if [itags] are ill-formed or
      invalid for this function. *)

val of_string_list : ?itags:string -> ?comments:string -> ?columns:(string list) -> string list list -> t
  (** Default [itags] are:

      {v "table,comment-char=#,header" v}

      The only modification of this allowed is to change the
      comment-char or omit it. If omitted, [comments] must not be
      given.

  *)

val to_sqlite : ?otags:string -> t -> Sqlite3.db
  (** [to_sqlite t] writes contents of [t] into a SQLite database, and
      returns the handle to it. Comments in [t] are ignored. Default
      output tags are:

      {v "sqlite,db=:memory:,db_table=table" v}

      Raises [Tags.Invalid] if [otags] are ill-formed or invalid for
      this function. *)
