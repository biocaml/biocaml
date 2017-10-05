open Base
open Printf

type record = {
  seqname    : string ;
  source     : string option ;
  feature    : string option ;
  start_pos  : int ;
  stop_pos   : int ;
  score      : float option ;
  strand     : [`plus | `minus | `not_applicable | `unknown ] ;
  phase      : int option ;
  attributes : (string * string list) list ;
}

type item = [ `comment of string | `record of record ]

let record
  ?source ?feature ?score ?(strand = `unknown) ?phase ?(attributes = [])
  seqname start_pos stop_pos =
  {
    seqname ; source ; feature ; start_pos ; stop_pos ; score ; strand ; phase ; attributes ;
  }

let line_of_item version = function
  | `comment c -> Line.of_string_unsafe ("#" ^ c)
  | `record t ->
    let escape =
      match version with
      | `three -> (fun s -> Uri.pct_encode s)
      | `two -> sprintf "%S"
    in
    let optescape o =  Option.value_map ~default:"." o ~f:escape in
    String.concat ~sep:"\t" [
      t.seqname ;
      optescape t.source ;
      Option.value ~default:"." t.feature ;
      Int.to_string t.start_pos ;
      Int.to_string t.stop_pos ;
      Option.value_map ~default:"." ~f:(sprintf "%g") t.score;
      (match t.strand with`plus -> "+" | `minus -> "-"
                        | `not_applicable -> "." | `unknown -> "?");
      Option.value_map ~default:"." ~f:(sprintf "%d") t.phase;
      String.concat ~sep:";"
        (List.map t.attributes ~f:(fun (k,v) ->
           match version with
           | `three ->
             sprintf "%s=%s" (Uri.pct_encode k)
               (List.map v ~f:Uri.pct_encode |> String.concat ~sep:",")
           | `two ->
             sprintf "%s %s" k
               (List.map v ~f:escape |> String.concat ~sep:",")
         ));
    ]
    |> Line.of_string_unsafe

