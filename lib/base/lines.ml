module Parser = struct
  type state =
    | Current_line of Line.t
    | Finished

  let initial_state = Current_line Line.empty

  let step st i = match st, i with
    | Finished, _ -> Finished, []
    | Current_line l, None -> Finished, [ l ]
    | Current_line l, Some input ->
      match Line.rightmost input with
      | None, line -> Current_line (Line.append l line), []
      | Some left, right ->
        match Line.parse_string left with
        | [] -> assert false (* [Line.parse_string] never returns an empty list *)
        | h :: t -> Current_line right, (Line.append l h) :: t
end
