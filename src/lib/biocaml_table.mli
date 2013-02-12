
module Row : sig
  type item = [`int of int | `float of float | `string of string ]

  type t = item array

  type item_type = [`type_int | `type_float | `type_string ]

  type t_type = item_type array

  val of_line: ?separators:char list -> format:t_type -> Biocaml_line.t ->
    (t, [> `wrong_format of
             [> `column_number
             | `float_of_string of string
             | `int_of_string of string ] * t_type * string ]) Core.Result.t

  val to_line: sep:string -> t -> Biocaml_line.t

end

