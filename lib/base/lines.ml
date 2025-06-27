module Parser = struct
  type state =
    | Current_line of
        { n : int
        ; value : Line.t
        }
    | Finished of { n : int }
  (* [n] represents the number of completed lines *)

  let initial_state = Current_line { n = 0; value = Line.empty }

  let line_number = function
    | Current_line { n; value } -> if String.equal (value :> string) "" then n else n + 1
    | Finished { n } -> n
  ;;

  let step st i =
    match st, i with
    | Finished _, _ -> st, []
    | Current_line { n; value }, None ->
      Finished { n }, if Line.is_empty value then [] else [ value ]
    | Current_line { n; value }, Some input -> (
      match Line.rightmost input with
      | None, line ->
        let value = Line.append value line in
        Current_line { n; value }, []
      | Some left, right -> (
        match Line.parse_string left with
        | [] -> assert false (* [Line.parse_string] never returns an empty list *)
        | h :: t ->
          ( Current_line { n = n + List.length t + 1; value = right }
          , Line.append value h :: t )))
  ;;
end

module Test = struct
  let%test _ =
    let s1, l1 = Parser.(step initial_state) (Some "\naa\n\n\nbb") in
    let s2, l2 = Parser.step s1 (Some "aa\n\n\n") in
    let s3, l3 = Parser.step s2 None in
    Int.equal 0 Parser.(line_number initial_state)
    && Int.equal 5 (Parser.line_number s1)
    && List.equal String.equal [ ""; "aa"; ""; "" ] (l1 :> string list)
    && Int.equal 7 (Parser.line_number s2)
    && List.equal String.equal [ "bbaa"; ""; "" ] (l2 :> string list)
    && Int.equal 7 (Parser.line_number s3)
    && List.equal String.equal [] (l3 :> string list)
  ;;
end
