(** Parsing of tabular data with named columns. Input files must be in
    the format below. Columns must be separated by a single tab character
    and their names must be given on the first line.    
    {v
    colA     colB     .     colN
    x0A      x0B      .     x0N
    .        .       .      .
    .        .       .      .
    .        .       .      .
    xMA      xMB      .     xMN
    v}
    Table assumed to be rectangular even if some rows have fewer than
    N columns. Missing column values assumed to be empty
    string. However, any row with beyond N columns will trigger an
    error.
*)

type row
    (** Data row. *)
    
type getter = row -> string -> string
  (** A function [get] of type [getter] can be used as [get r c] to
      get the value of the column named [c] in row [r]. It raises
      [Failure] if the column name is invalid, or if the given row does
      not have a value for that column. *)

type columns = string list
    (** All column names occurring in a file, in order that they occur. *)

val make_fold : string -> (columns * getter * (('a -> row -> 'a) -> 'a -> 'a))
  (** [make_fold file] returns the column names, a [getter] function,
      and a fold function that can be used for the given [file]. For
      example, the result of the code below is a list of all the values in
      column [c].
      {v
      let _,get,fold = make_fold file in
      let f l r = (get r c)::l in
      List.rev (fold f [])
      v}
      Function [f] should raise [Failure] in case of error. The
      returned [fold] function will raise [Failure] with information about
      where the error occurred. Calling [fold] repeatedly will work
      correctly (assuming the [file] itself does not change between
      calls).  *)
  
val make_iter : string -> (columns * getter * ((row -> unit) -> unit))
  (** Like [make_fold] but the function returned is an [iter]ator. *)

val hashtbl_of_file : string -> string -> string -> (string, string) Hashtbl.t
  (** [hashtbl_of_file file x y] returns the mapping from values of
      column [x] to those in column [y]. Raise [Failure] if [x] maps to
      more than one value in [y]. *)

val multi_hashtbl_of_file : string -> string -> string -> (string, StringSet.t) Hashtbl.t
  (** [multi_hashtbl_of_file file x y] is like [hashtbl_of_file] but
      returns mapping from [x] to set of all [y]'s it maps to. *)

val map_of_file : string -> string -> string -> string StringMap.t
val multi_map_of_file : string -> string -> string -> StringSet.t StringMap.t
  (** Analogous to [hashtbl_of_file] and [multi_hashtbl_of_file]. *)
