open Biocaml_internal_pervasives
open Result

type t = string * int * int * [`Float of float| `Int of int | `String of string] list
with sexp

type parse_error =
[ `not_a_float of Biocaml_pos.t * string
| `not_an_int of Biocaml_pos.t * string
| `wrong_number_of_columns of Biocaml_pos.t * string list
| `incomplete_input of Biocaml_pos.t * string list * string option
]
with sexp

type parsing_spec = [
| `enforce of [ `float | `int | `string ] list
| `strings
| `best_effort
]
with sexp
  
module Transform = struct
  let next how_to_parse =
    let open Biocaml_transform.Line_oriented in
    fun p ->
      let parse_string s = Ok s in
      let parse_int s =
        try Ok (Int.of_string s)
        with e -> Error (`not_an_int (current_position p, s)) in
      let parse_float s =
        try Ok (Float.of_string s)
        with e -> Error (`not_a_float (current_position p, s)) in
      let best_effort s =
        match parse_float s with
        | Ok f -> Ok (`Float f)
        | Error _ -> Ok (`String s) in
      begin match next_line p with
      | Some l ->
        let exploded =
          String.split_on_chars l ~on:[' '; '\t'] |! List.filter ~f:((<>) "") in
        begin match exploded with
        | n :: b :: e :: l ->
          let m =
            parse_string n >>= fun name ->
            parse_int b >>= fun start ->
            parse_int e >>= fun stop ->
            let rec loop s spec acc =
              match s, spec with
              | [], `enforce [] -> Ok acc
              | [], `enforce (_ :: _) | _ :: _, `enforce [] ->
                fail (`wrong_number_of_columns (current_position p, l))
              | [], `strings | [], `best_effort -> Ok acc
              | (str :: s2), `enforce (`float :: l2) ->
                parse_float str >>= fun f ->
                loop s2 (`enforce l2) (`Float f :: acc)
              | (str :: s2), `enforce (`int :: l2) ->
                parse_int str >>= fun f ->
                loop s2 (`enforce l2) (`Int f :: acc)
              | (str :: s2), `enforce (`string :: l2) ->
                parse_string str >>= fun f ->
                loop s2 (`enforce l2) (`String f :: acc)
              | (str :: s2), `strings ->
                parse_string str >>= fun f ->
                loop s2 spec (`String f :: acc)
              | (str :: s2), `best_effort ->
                best_effort str >>= fun f ->
                loop s2 spec (f :: acc)
            in
            loop l how_to_parse [] >>= fun l ->
            return (name, start, stop, List.rev l)
          in
          `output m
        | l ->
          output_error (`wrong_number_of_columns (current_position p, l))
        end
      | None -> `not_ready
      end
        
        
  let string_to_t ?filename ?(more_columns=`best_effort) () =
    let name = sprintf "bed_parser:%s" Option.(value ~default:"<>" filename) in
    let next = next more_columns in
    Biocaml_transform.Line_oriented.make ~name ?filename ~next ()
      ~on_error:(function `next n -> n
      | `incomplete_input e -> `incomplete_input e)
      
  let t_to_string () =
    let module PQ = Biocaml_transform.Printer_queue in
    let printer =
      Biocaml_transform.Printer_queue.make ()
        ~to_string:(fun (n, b, e, l) ->
          sprintf "%s %d %d %s\n" n b e
            (List.map l (function
            | `Float f -> Float.to_string f
            | `Int i -> Int.to_string i
            | `String s -> s) |! String.concat ~sep:" ")) in
    Biocaml_transform.make ~name:"bed_printer" ()
      ~feed:(fun r ->
        PQ.feed printer r)
      ~next:(fun stopped ->
        match (PQ.flush printer) with
        | "" -> if stopped then `end_of_stream else `not_ready
        | s -> `output s)

end
