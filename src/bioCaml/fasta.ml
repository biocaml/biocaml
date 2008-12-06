open TylesBase
open Printf

type 'hdr t = ('hdr * Seq.t) StringMap.t
    (* key is name of sequence *)

exception Bad of string
let raise_bad msg = raise (Bad msg)

let fold f t init =
  let f name (hdr,seq) ans = f name hdr seq ans in
  StringMap.fold f t init

let iter f t =
  let f name (hdr,seq) = f name hdr seq in
  StringMap.iter f t

let headers t = 
  let f k (a,_) ans = (k,a)::ans in
  List.rev (StringMap.fold f t [])
    
let names t = List.map fst (headers t)

let get_header_seq t x =
  try StringMap.find x t
  with Not_found -> failwith (sprintf "sequence %s not found" x)

let get_header t x = fst (get_header_seq t x)
let get_seq t x = snd (get_header_seq t x)

module Parser = struct
  let is_header s = 
    if String.length s > 0
    then s.[0] = '>'
    else false

  (* return the name and header info *)
  let header parse_header s =
    assert (String.length s > 0);
    assert (s.[0] = '>');
    parse_header (String.lchop s)
      
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
  let section parse_header lines : (string * 'a * Seq.t) =
    Stream.skip_while (String.for_all Char.is_space) lines;
    let name,hdr = header parse_header (Stream.next lines) in
    let seq = sequence lines in
      (name, hdr, Seq.of_buffer_unsafe seq) (* ok to use unsafe because [sequence] returns valid buffer *)

  let fasta parse_header file_name cin : 'a t =
    let lines = Stream.lines_of_channel cin in
    let lines = Stream.map String.strip_final_cr lines in
    let err msg = Msg.err ~pos:(Pos.fl file_name (Stream.count lines)) msg in
    let rec loop lines ans =
      if Stream.is_empty lines then
        ans
      else
        let name,hdr,seq = section parse_header lines in
        let ans =
          if StringMap.mem name ans then
            failwith (sprintf "sequence %s previously inserted" name)
          else
            StringMap.add name (hdr,seq) ans
        in
        loop lines ans
    in 
    try loop lines StringMap.empty
    with Failure m | Bad m -> raise_bad (err m)
end

let of_file' parse_header file =
  try_finally (Parser.fasta parse_header file) close_in (open_in file)

let of_file file = of_file' (fun s -> (s,"")) file
