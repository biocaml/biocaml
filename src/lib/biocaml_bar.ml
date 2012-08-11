open Biocaml_std

type header = (string * string) list
    (* list of tag-value pairs *)

type section =
    {
      sec_num:int;
      sec_name:string;
      sec_data:(int * float) list (* stored in ascending order by int *)
    }

type t = header * section list (* section list in ascending order by sec_name *)
exception Bad of string
let raise_bad msg = raise (Bad msg)
    
let num_sections = List.length <<- snd
let data_type = List.assoc_exn "Data" <<- fst
let scale = List.assoc_exn "Scale" <<- fst
let genomic_map = List.assoc_exn "Genomic_Map" <<- fst
let alg_name = List.assoc_exn "AlgName" <<- fst
let alg_version = List.assoc_exn "AlgVersion" <<- fst
let coord_convention = List.assoc_exn "probe_coordinate_convention" <<- fst
let sections = snd

let section (_,secs) nm =
  match List.find ~f:(fun s -> s.sec_name = nm) secs with
  | None -> failwith (sprintf "section %s not found" nm)
  | Some s -> s
    
let sectioni (_,secs) i =
  match List.find ~f:(fun s -> s.sec_num = i) secs with
  | None -> failwith (sprintf "section %d not found" i)
  | Some s -> s

let to_list (_,sections) =
  let f s =
    let chr = s.sec_name in
    List.map (fun (i,v) -> chr,i,v) s.sec_data
  in
  List.flatten (List.map f sections)      
      
module Parser = struct
  let junk_blank_lines lines =
    Stream.skip_while (String.for_all ~f:Char.is_whitespace) lines

  let tag_value (s':string) : string * string =
    let s = String.strip (String.drop_prefix s' 1) in
    let tv = String.split s '\t' in
      if List.length tv = 2
      then (List.nth tv 0, List.nth tv 1)
      else raise_bad (sprintf "invalid tag-value pair %s" s')
      
  (* lines should point to beginning of file, upon return will point to start of first section *)
  let header lines =
    let lines' = Stream.keep_while
                   (fun s -> not (String.for_all Char.is_whitespace s)) lines in
    let f accum l = (tag_value l)::accum in
    let ans = List.rev (Stream.fold f [] lines') in
      junk_blank_lines lines; ans

  (* lines should point to start of a section, upon return will point to start of next section or end of file *)
  let section lines =
    let tv lines = tag_value (Stream.next lines) in
    let seq_num = (int_of_string <<- snd <<- tv) lines in
    let seq_name = (snd <<- tv) lines in
    let num_hits = (int_of_string <<- snd <<- tv) lines in
    let _ = junk_blank_lines lines in

    let lines' = Stream.keep_while
                   (fun s -> not (String.for_all Char.is_whitespace s)) lines in
    let parse_line s =
      let sl = String.split s '\t' in
        if List.length sl = 2 then
          (int_of_string (List.nth sl 0), float_of_string (List.nth sl 1))
        else
          raise_bad ("data row must contain exactly two fields")
    in
    let data = Stream.to_list (Stream.map parse_line lines') in
    let data = List.sort ~cmp:(fun (p1,_) (p2,_) -> Pervasives.compare p1 p2) data in
    let sec = {sec_num=seq_num; sec_name=seq_name; sec_data=data} in
      if List.length data = num_hits then
        (junk_blank_lines lines; sec)
      else
        raise_bad (sprintf "expected %d hits but found %d" num_hits (List.length data))
         
  let of_file file =
    let of_channel cin =
      let lines = Stream.map String.strip_final_cr (Stream.lines_of_channel cin) in
      let err msg = Msg.err ~pos:(Pos.fl file (Stream.count lines)) msg in
        try
          let hdr = header lines in
          let secs = ref [] in
          let _ = while not (Stream.is_empty lines) do secs := (section lines)::!secs done in
          let secs = List.sort ~cmp:(fun s1 s2 -> Pervasives.compare s1.sec_name s2.sec_name) !secs in
          let expected_num_secs = int_of_string (List.assoc_exn "Number Sequences" hdr) in
          let actual_num_secs = List.length secs in
            if actual_num_secs = expected_num_secs then
              (hdr,secs)
            else
              raise_bad (sprintf "expected %d sequences but found %d" expected_num_secs actual_num_secs)
        with
            Failure msg | Bad msg -> raise_bad (err msg)
    in
    try_finally_exn of_channel ~fend:close_in (open_in file)
end
  
let of_file = Parser.of_file
  
