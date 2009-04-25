open Sesame
open Printf

type header = string
type t = Seq.t StringMap.t
    (* key is header *)

exception Bad of string
let raise_bad msg = raise (Bad msg)

let fold = StringMap.fold
let iter = StringMap.iter
let headers t = List.rev (fold (fun hdr _ ans -> hdr::ans) t [])

let get_seq t x =
  try StringMap.find x t
  with Not_found -> failwith (sprintf "%s: header not found" x)

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
  let section lines : (header * Seq.t) =
    Stream.skip_while (String.for_all Char.is_space) lines;
    let hdr = header (Stream.next lines) in
    let seq = sequence lines in
      (hdr, Seq.of_buffer_unsafe seq) (* ok to use unsafe because [sequence] returns valid buffer *)

  let fasta file_name cin : t =
    let lines = Stream.lines_of_channel cin in
    let lines = Stream.map String.strip_final_cr lines in
    let err msg = Msg.err ~pos:(Pos.fl file_name (Stream.count lines)) msg in
    let rec loop lines ans =
      if Stream.is_empty lines then
        ans
      else
        let hdr,seq = section lines in
        let ans =
          if StringMap.mem hdr ans then
            failwith (sprintf "%s: sequence with this header previously inserted" hdr)
          else
            StringMap.add hdr seq ans
        in
        loop lines ans
    in 
    try loop lines StringMap.empty
    with Failure m | Bad m -> raise_bad (err m)
end

let of_file file = 
  try_finally (Parser.fasta file) close_in (open_in file)
