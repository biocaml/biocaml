open Biocaml_internal_pervasives

type t = string * int * int * [`Float of float| `Int of int | `String of string] list

type parse_error =
[ `not_a_float of Biocaml_pos.t * string
| `not_an_int of Biocaml_pos.t * string
| `wrong_number_of_columns of Biocaml_pos.t * string list
| `incomplete_input of Biocaml_pos.t * string list * string option
]

type parsing_spec = [ `float | `int | `string ] list
  
module Transform = struct
  let next how_to_parse =
    let more_columns = List.length how_to_parse in
    let open Biocaml_transform.Line_oriented in
    let open Result in
    fun p ->
      let parse_string s = Ok s in
      let parse_int s =
        try Ok (Int.of_string s)
        with e -> Error (`not_an_int (current_position p, s)) in
      let parse_float s =
        try Ok (Float.of_string s)
        with e -> Error (`not_a_float (current_position p, s)) in
      begin match next_line p with
      | Some l ->
        let exploded =
          String.split_on_chars l ~on:[' '; '\t'] |! List.filter ~f:((<>) "") in
        begin match exploded with
        | n :: b :: e :: l when List.length l = more_columns ->
          let m =
            parse_string n >>= fun name ->
            parse_int b >>= fun start ->
            parse_int e >>= fun stop ->
            let rec loop s l acc =
              match s, l with
              | [], [] -> Ok acc
              | (str :: s2), (`float :: l2) ->
                parse_float str >>= fun f ->
                loop s2 l2 (`Float f :: acc)
              | (str :: s2), (`int :: l2) ->
                parse_int str >>= fun f ->
                loop s2 l2 (`Int f :: acc)
              | (str :: s2), (`string :: l2) ->
                parse_string str >>= fun f ->
                loop s2 l2 (`String f :: acc)
              | _, _ -> assert false
            in
            loop l how_to_parse [] >>= fun l ->
            return (name, start, stop, List.rev l)
          in
          begin match m with
          | Ok l -> `output l
          | Error e -> `error e
          end
        | l ->
          `error (`wrong_number_of_columns (current_position p, l))
        end
      | None -> `not_ready
      end
        
        
  let string_to_t ?filename ?(more_columns=[]) () =
    let name = sprintf "bed_parser:%s" Option.(value ~default:"<>" filename) in
    let next = next more_columns in
    Biocaml_transform.Line_oriented.make_stoppable ~name ?filename ~next ()
      
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
    Biocaml_transform.make_stoppable ~name:"bed_printer" ()
      ~feed:(fun r ->
        PQ.feed printer r)
      ~next:(fun stopped ->
        match (PQ.flush printer) with
        | "" -> if stopped then `end_of_stream else `not_ready
        | s -> `output s)

end
