open TylesBase
open Printf

type 'hdr t =
    {
      parse_header : string -> (string * 'hdr);
      data : (string * Seq.t) list (* string is raw unparsed header *)
    }

exception Bad of string
let raise_bad msg = raise (Bad msg)

let headers t = List.map (t.parse_header <<- fst) t.data
let raw_headers t = List.map fst t.data
let names t = List.map fst (headers t)

let get_seq t n =
  let rec loop dat =
    match dat with
      | [] -> failwith (sprintf "Fasta.get_seq: no sequence named %s" n)
      | (hdr,seq)::dat -> if fst (t.parse_header hdr) = n then seq else loop dat
  in loop t.data

module Parser = struct
  let is_header s = 
    if String.length s > 0
    then s.[0] = '>'
    else false

  let header s =
    assert (String.length s > 0);
    assert (s.[0] = '>');
    String.lchop s
      
  (* lines should point to beginning of sequence specification,
   * returned buffer will contain valid nucleic acid sequence *)
  let sequence lines : Buffer.t =
    let ans = Buffer.create 10000 in
    let go l = not (Stream.is_empty lines || is_header l) in
    let all_are_nucleic_acid s =
      for i = 0 to String.length s - 1 
      do (if not (Seq.is_nucleic_acid s.[i]) then raise_bad (String.of_char s.[i]))
      done
    in
    let add l =
      try all_are_nucleic_acid l; Buffer.add_string ans l
      with Bad msg -> raise_bad ("invalid nucleic acid " ^ msg)
    in
    let lines = Stream.keep_while go lines in
    Stream.iter add lines;
    ans
        
  (** next non-empty line should be header line *)
  let section lines : (string * Seq.t) =
    Stream.skip_while (String.for_all Char.is_space) lines;
    let hdr = header (Stream.next lines) in
    let seq = sequence lines in
      (hdr, Seq.of_buffer_unsafe seq) (* ok to use unsafe because [sequence] returns valid buffer *)

  let fasta file_name cin : (string * Seq.t) list =
    let lines = Stream.lines_of_channel cin in
    let lines = Stream.map String.strip_final_cr lines in
    let err msg = Msg.err ~pos:(Pos.fl file_name (Stream.count lines)) msg in
    let rec loop lines ans =
      if Stream.is_empty lines
      then ans
      else let sect = section lines in loop lines (sect::ans)
    in 
      try List.rev (loop lines [])
      with Failure m | Bad m -> raise_bad (err m)
end

let of_file_exn' parse_header file =
  let data = try_finally (Parser.fasta file) close_in (open_in file) in
  let names = List.map parse_header (List.map fst data) in
  let err = Msg.err ~pos:(Pos.f file) in
    if List.length names <> List.length (List.unique names)
    then failwith (err "sequence names not unique")
    else {parse_header = parse_header; data = data}

let of_file' parse_header file =
  try Some (of_file_exn' parse_header file)
  with Bad _ -> None

let of_file_exn file = of_file_exn' (fun s -> (s,"")) file

let of_file file =
  try Some (of_file_exn file)
  with Bad _ -> None
