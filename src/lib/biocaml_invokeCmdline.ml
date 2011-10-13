open BatEnum
   
let invoke_cmd = fun command args -> let chan = Unix.open_process_in (command ^ " " ^ (String.concat " " args)) in
 let res = ref ([] : string list) in
  let rec invoke_aux () =
   let e = input_line chan in
    res := e :: !res;
    invoke_aux ()
   in
   try
    invoke_aux ()
   with End_of_file ->
    ignore (Unix.close_process_in chan);
    (*TODO check status
 match (Unix.close_process_in chan) with
 | Unix.WEXITED (i) -> print_string "exit code "; print_int i; print_endline ""
 | _ -> print_endline "process was killed/stopped"
*)
    (List.rev !res)
    

let parse_lines fname parse_line = 
 BatList.of_enum ((Option.get) @/ ((parse_line @/ (BatFile.lines_of fname))) // (Option.is_some)) 
