type in_channel

val open_in : string -> in_channel
val close_in : in_channel -> unit

exception Parse_error of string

val input_char : in_channel -> char
val input_byte : in_channel -> int
val input_int32 : in_channel -> int32
val input: in_channel -> string -> int -> int -> int
val really_input : in_channel -> string -> int -> int -> unit

val with_file_in : string -> f:(in_channel -> 'a) -> 'a


type out_channel

val open_out : ?level:int -> string -> out_channel
val close_out : out_channel -> unit

val output : out_channel -> string -> int -> int -> unit

val with_file_out : ?level:int -> string -> f:(out_channel -> 'a) -> 'a
