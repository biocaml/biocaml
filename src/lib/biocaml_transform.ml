
module Line_oriented = struct

  type parser = {
    mutable unfinished_line : string option;
    lines : string Queue.t;
    mutable parsed_lines : int;
    filename : string option;
  }

  let parser ?filename () =
    {unfinished_line = None;
     lines = Queue.create ();
     parsed_lines = 0;
     filename}

  let feed_line p s =
    Queue.push s p.lines
      
  let feed_string p s =
    let lines = BatString.nsplit s "\n" in 
    let rec faux = function
      | [] -> assert false
      | [ "" ] -> (* last char was a "\n" *) ()
      | [ s ] -> (* some remaining stuff *)
        p.unfinished_line <- Some s;
      | h :: t ->
        Queue.push h p.lines;
        faux t
    in
    match p.unfinished_line, lines with
    | _, [] -> ()
    | None, l -> faux l
    | Some s, h :: t ->
      p.unfinished_line <- None;
      faux ((s ^ h) :: t)
        
  let queued_lines p = Queue.length p.lines
  let next_line p =
    p.parsed_lines <- p.parsed_lines + 1;
    try Some (Queue.pop p.lines) with e -> None
  let next_line_exn p =
    p.parsed_lines <- p.parsed_lines + 1;
    Queue.pop p.lines

  let current_position p =
    Biocaml_pos.make ?file:p.filename ~line:p.parsed_lines ()
end



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
