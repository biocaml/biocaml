open Core.Std
open CFStream
open Or_error

module Bgzf = Biocaml_bgzf
module Sam = Biocaml_sam

type header = Biocaml_sam.header

type alignment = Biocaml_sam.alignment

exception Parse_error of Error.t

let parse_error msg =
  raise (Parse_error (Error.of_string msg))

let wrap f x =
  try f x
  with
  | Bgzf.Parse_error err -> error_string err
  | End_of_file -> error_string "Bam: premature end of file reading the header"

let input_int32_as_int iz =
  Int32.to_int_exn (Bgzf.input_int32 iz)

let read_header = wrap (fun iz ->
    let magic = Bgzf.input_string iz 4 in
    if magic <> "BAM\001" then parse_error "Incorrect magic string, not a BAM file" ;
    let l_text = input_int32_as_int iz in
    let text = Bgzf.input_string iz l_text in
    Biocaml_sam.parse_header text
  )

let read_one_reference_information iz =
  let parse iz =
    let l_name = input_int32_as_int iz in
    let name = Bgzf.input_string iz (l_name - 1) in
    let _ = Bgzf.input_char iz in (* name is a NULL terminated string *)
    let length = input_int32_as_int iz in
    return (name, length)
  in
  wrap parse iz >>= fun (name, length) ->
  Sam.ref_seq ~name ~length ()

let read_reference_information = wrap (fun iz ->
    let rec loop accu n =
      if n = 0 then Ok (List.rev accu)
      else
        match read_one_reference_information iz with
        | Ok refseq -> loop (refseq :: accu) (n - 1)
        | Error _ as e -> e
    in
    let n_ref = input_int32_as_int iz in
    loop [] n_ref
    >>| Array.of_list
  )

let hd s =
  String.iter s (fun c -> Printf.printf "%x  " (Char.to_int c))

let input_string_or_fail iz n =
  let r = Bgzf.input_string iz n in
  if String.length r <> n then parse_error "Premature end of file while reading alignment."
  else r

let get_8_0 =
  let mask = Int32.of_int_exn 0xff in
  fun x -> Int32.bit_and mask x |> Int32.to_int_exn

let get_16_0 =
  let mask = Int32.of_int_exn 0xffff in
  fun x -> Int32.bit_and mask x |> Int32.to_int_exn

let get_16_8 x =
  get_8_0 (Int32.shift_right x 8)

let get_32_4 x =
  Int32.shift_right x 4 |> Int32.to_int_exn

let get_4_0 =
  let mask = Int32.of_int_exn 0xf in
  fun x -> Int32.bit_and mask x |> Int32.to_int_exn

let cigar_op_of_int32 x =
  let op_len = get_32_4 x in
  match get_4_0 x with
  | 0 -> `Alignment_match op_len
  | 1 -> `Insertion op_len
  | 2 -> `Deletion op_len
  | 3 -> `Skipped op_len
  | 4 -> `Soft_clipping op_len
  | 5 -> `Hard_clipping op_len
  | 6 -> `Padding op_len
  | 7 -> `Seq_match op_len
  | 8 -> `Seq_mismatch op_len
  | _ -> assert false

let char_of_seq_code  = function
  | 0  -> '='
  | 1  -> 'A'
  | 2  -> 'C'
  | 3  -> 'M'
  | 4  -> 'G'
  | 5  -> 'R'
  | 6  -> 'S'
  | 7  -> 'V'
  | 8  -> 'T'
  | 9  -> 'W'
  | 10 -> 'Y'
  | 11 -> 'H'
  | 12 -> 'K'
  | 13 -> 'D'
  | 14 -> 'B'
  | 15 -> 'N'
  | l -> failwithf "letter not in [0, 15]: %d" l ()

let read_alignment (refseqs : Sam.ref_seq array) iz =
  (* the type annotation for the [refseqs] argument is mandatory to disambiguate
     types [Sam.ref_seq] and [Sam.program]. *)
  let block_size = input_int32_as_int iz in
  (* let block = Bgzf.input_string iz block_size in *)
  (* if String.length block < block_size then parse_error "Premature end of file while reading alignment." ; *)

  let refID = input_int32_as_int iz in
  if refID < -1 || refID > Array.length refseqs
  then raise (Parse_error (Error.create "Incorrect refID field while reading an alignment" refID <:sexp_of<int>>)) ;
  let rname =
    match refID with
    | -1 -> None
    | i -> Some refseqs.(i).Sam.name
  in

  let pos = match Bgzf.input_int32 iz |> Int32.to_int with
    | Some (- 1) -> None
    | Some 2147483647
    | None -> parse_error "A read has a position greater than 2^31"
    | Some n ->
      if n < 0 then parse_error "A read has a negative position"
      else Some (n + 1)
  in

  let bin_mq_nl = Bgzf.input_int32 iz in
  let l_read_name = get_8_0 bin_mq_nl in (* cannot fail since by construction get_8_0 returns integers less than 256 *)
  let mapq = get_16_8 bin_mq_nl in

  let flag_nc = Bgzf.input_int32 iz in
  let flags =
    match Int32.shift_right flag_nc 16 |> Int32.to_int_exn |> Sam.Flags.of_int with
    | Ok flags -> flags
    | Error err -> raise (Parse_error err)
    (* because we are shifting right just before, Int32.to_int_exn cannot fail *)
  in
  let n_cigar_op = get_16_0 flag_nc in

  let l_seq = input_int32_as_int iz in

  let next_refID = input_int32_as_int iz in
  if next_refID < -1 || next_refID > Array.length refseqs
  then raise (Parse_error (Error.create "Incorrect next_refID field while reading an alignment" next_refID <:sexp_of<int>>)) ;
  let rnext =
    match next_refID with
    | -1 -> None
    | i -> Some (`Value refseqs.(i).Sam.name)
  in

  let pnext = match Bgzf.input_int32 iz |> Int32.to_int with
    | Some (- 1) -> None
    | Some 2147483647
    | None -> parse_error "A read has a position > than 2^31"
    | Some n -> if n > 0 then Some (n + 1) else parse_error "A read has a negative next position"
  in

  let tlen = input_int32_as_int iz in

  let read_name =
    let r = input_string_or_fail iz (l_read_name - 1) in
    Pervasives.ignore (Bgzf.input_char iz) ; (* trailing null character *)
    r
  in

  let cigar =
    List.init n_cigar_op ~f:(fun _ ->
        cigar_op_of_int32 (Bgzf.input_int32 iz)
      ) in

  let seq =
    let r = String.make l_seq ' ' in
    for i = 0 to (l_seq + 1) / 2 - 1 do
      let c = Bgzf.input_byte iz in
      r.[2 * i] <- char_of_seq_code (c lsr 4) ;
      if 2 * i + 1 < l_seq then r.[2 * i + 1] <- char_of_seq_code (c land 0xf) ;
    done ;
    r in

  let qual =
    match Sam.parse_qual (input_string_or_fail iz l_seq) with
    | Ok q -> q
    | Error err -> raise (Parse_error err) in

  let remaining = block_size - 32 - l_read_name - 4 * n_cigar_op - ((l_seq + 1) / 2) - l_seq in

  (* UNDER CONSTRUCTION *)
  (* let optional_fields = *)
  (*   let rec loop remaining accu = *)
  (*     if remaining = 0 then List.rev accu *)
  (*     else ( *)
  (*       let tag = input_string_or_fail iz 2 in *)
  (*       let field_type = Bgzf.input_char iz in *)
  (*       let field_value, shift = match field_type with *)
  (*         | 'C' -> ( *)
  (*             match Int32.of_int (Bgzf.input_byte iz) with *)
  (*             | Some i -> `i i, 1 *)
  (*             | None -> parse_error "Expected a 32-bit integers in optional argument" *)
  (*           ) in *)
  (*       let field = Sam.optional_field ~tag ~field_type ~field_value () in *)
  (*       loop (remaining - 3 - shift) (field :: accu) *)
  (*     | _ -> assert false *)
  (*     ) *)
  (*   in *)
  (*   loop remaining [] in *)


  let _ = input_string_or_fail iz remaining in

  Sam.alignment ~qname:read_name ~flags ?rname ?pos ~mapq ~cigar ?rnext ?pnext ~tlen ~seq ~qual ()

let read_alignment_stream refseqs iz =
  Stream.from (fun _ ->
      try Some (read_alignment refseqs iz)
      with
      | Parse_error err -> Some (Error err)
      | Bgzf.Parse_error err -> Some (error_string err)
      | End_of_file -> None
    )


let read ic =
  let iz = Bgzf.of_in_channel ic in
  read_header iz >>= fun header ->
  read_reference_information iz >>= fun refinfo ->
  return (header, read_alignment_stream refinfo iz)


let with_file fn ~f =
  In_channel.with_file ~binary:true fn ~f:(fun ic ->
      read ic >>= fun (header, alignments) ->
      return (f header alignments)
    )

let write header alignments oc = assert false
