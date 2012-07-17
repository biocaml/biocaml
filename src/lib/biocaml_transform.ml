
class type ['input, 'output, 'error] transform =
object
  method feed: 'input -> unit
  method next: [ `output of 'output | `not_ready | `error of 'error ]
end

let compose ta tb =
object
  method feed (i: 'a) : unit =
    ta#feed i
  method next : [ `output of 'e | `not_ready
                | `error of [`left of 'c | `right of 'f ] ] =
    match ta#next with
    | `output o ->
      tb#feed o;
      begin match tb#next with
      | `output o -> `output o
      | `not_ready -> `not_ready
      | `error e -> `error (`right e)
      end
    | `not_ready -> `not_ready
    | `error e -> `error (`left e)
end 
  
let mix ta tb ~f =
object 
  val mutable a_buffer = None
  method feed (a, b) =
    ta#feed a;
    tb#feed b
  method next : [ `output of 'e | `not_ready
                | `error of [`left of 'c | `right of 'f ] ] =
    begin match a_buffer with
    | None ->
      begin match ta#next with
      | `output oa ->
        begin match tb#next with
        | `output ob -> `output (f oa ob)
        | `not_ready ->
          a_buffer <- Some oa;
          `not_ready
        | `error e -> `error (`right e)
        end
      | `not_ready -> `not_ready
      | `error e -> `error (`left e)
      end
    | Some oa ->
      begin match tb#next with
      | `output ob -> `output (f oa ob)
      | `not_ready -> `not_ready
      | `error e -> `error (`right e)
      end
    end
end
