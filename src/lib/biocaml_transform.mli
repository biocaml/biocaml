
module Line_oriented: sig
  type parser
  val parser: ?filename:string -> unit -> parser
    
  val feed_line: parser -> string -> unit

  val feed_string: parser -> string -> unit

    
  val queued_lines: parser -> int
  val next_line: parser -> string option
  val next_line_exn: parser -> string

  val current_position: parser -> Biocaml_pos.t
end


class type ['input, 'output, 'error] transform =
object
  method feed: 'input -> unit
  method next: [ `output of 'output | `not_ready | `error of 'error ]
end


val compose:
  ( 'input_left, 'middle, 'error_left) transform ->
  ( 'middle, 'output_right, 'error_right) transform ->
  ( 'input_left, 'output_right, [ `left of 'error_left | `right of 'error_right ] )
    transform
    
val mix :
  ( 'input_left, 'output_left, 'error_left) transform ->
  ( 'input_right, 'output_right, 'error_right) transform ->
  f:('output_left -> 'output_right -> 'output_f) ->
  ( 'input_left * 'input_right, 'output_f,
    [ `left of 'error_left | `right of 'error_right ] ) transform
  
