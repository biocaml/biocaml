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

let input_s32_as_int iz =
  Int32.to_int_exn (Bgzf.input_s32 iz)

let read_header = wrap (fun iz ->
    let magic = Bgzf.input_string iz 4 in
    if magic <> "BAM\001" then parse_error "Incorrect magic string, not a BAM file" ;
    let l_text = input_s32_as_int iz in
    let text = Bgzf.input_string iz l_text in
    Biocaml_sam.parse_header text
  )

let read_one_reference_information iz =
  let parse iz =
    let l_name = input_s32_as_int iz in
    let name = Bgzf.input_string iz (l_name - 1) in
    let _ = Bgzf.input_char iz in (* name is a NULL terminated string *)
    let length = input_s32_as_int iz in
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
    let n_ref = input_s32_as_int iz in
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

let cigar_op_of_s32 x : Sam.cigar_op Or_error.t =
  let open Sam in
  let op_len = get_32_4 x in
  match get_4_0 x with
  | 0 -> cigar_op_alignment_match op_len
  | 1 -> cigar_op_insertion op_len
  | 2 -> cigar_op_deletion op_len
  | 3 -> cigar_op_skipped op_len
  | 4 -> cigar_op_soft_clipping op_len
  | 5 -> cigar_op_hard_clipping op_len
  | 6 -> cigar_op_padding op_len
  | 7 -> cigar_op_seq_match op_len
  | 8 -> cigar_op_seq_mismatch op_len
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

let parse_cstring iz max_size =
  let buf = Buffer.create max_size in
  let rec loop n =
    if n > max_size
    then error_string "Unexpectedly long string in optional field"
    else (
      let c = Bgzf.input_char iz in
      if c = '\000' then return (Buffer.contents buf)
      else (Buffer.add_char buf c ; loop (n + 1))
    )
  in loop 0

let int32_is_positive =
  let mask = Int32.(shift_left one 31) in
  fun i -> Int32.(compare (bit_and i mask) zero) = 0

let parse_cCsSiIf iz = function
  | 'c' ->
    let i = Int32.of_int_exn (Bgzf.input_s8 iz) in
    return (Sam.optional_field_value_i i, 1)
  | 'C' ->
    let i = Int32.of_int_exn (Bgzf.input_u8 iz) in
    return (Sam.optional_field_value_i i, 1)
  | 's' ->
    let i = Int32.of_int_exn (Bgzf.input_s16 iz) in
    return (Sam.optional_field_value_i i, 2)
  | 'S' ->
    let i = Int32.of_int_exn (Bgzf.input_u16 iz) in
    return (Sam.optional_field_value_i i, 2)
  | 'i' ->
    let i = Bgzf.input_s32 iz in
    return (Sam.optional_field_value_i i, 4)
  | 'I' ->
    let i = Bgzf.input_s32 iz in
    if int32_is_positive i then return (Sam.optional_field_value_i i, 4)
    else error_string "Use of big unsigned ints in optional fields is not well-defined in the specification of SAM/BAM files"
        (* There's something fishy in the SAM/BAM format
           specification. It says:

           << An integer may be stored as one of `cCsSiI' in BAM, representing
           int8_t, uint8_t, int16_t, uint16_t, int32_t and uint32_t,
           respectively. In SAM, all single integer types are mapped
           to int32_t >>

           But how are we supposed to map uint32_t to int32_t? Seems
           like people from picard tools also noticed this problem:

           http://sourceforge.net/p/samtools/mailman/message/24239571/ *)
  | 'f' ->
    let i = Bgzf.input_s32 iz in
    return (Sam.optional_field_value_f (Int32.float_of_bits i), 4)
  | _ -> error_string "Incorrect numeric optional field type identifier"

let cCsSiIf_size = function
  | 'c'
  | 'C' -> return 1
  | 's' | 'S' -> return 2
  | 'i' | 'I'
  | 'f' -> return 4
  | _ -> error_string "Incorrect numeric optional field type identifier"

let parse_optional_field_value iz remaining = function
  | 'A' ->
    Sam.optional_field_value_A (Bgzf.input_char iz) >>= fun v ->
    return (v, 1)
  | 'c' | 'C' | 's' | 'S' | 'i' | 'I' | 'f' as typ ->
    parse_cCsSiIf iz typ
  | 'Z' ->
    parse_cstring iz remaining >>= fun s ->
    Sam.optional_field_value_Z s >>= fun value ->
    return (value, String.length s + 1) (* to account for the NULL character *)
  | 'H' ->
    parse_cstring iz remaining >>= fun s ->
    Sam.optional_field_value_H s >>= fun value ->
    return (value, String.length s + 1) (* to account for the NULL character *)
  | 'B' ->
    let typ = Bgzf.input_char iz in
    let n = Bgzf.input_s32 iz in
    (
      match Int32.to_int n with
      | Some n ->
        cCsSiIf_size typ >>= fun elt_size ->
        let elts = List.init n ~f:(fun _ -> input_string_or_fail iz elt_size) in
        let bytes_read = 5 (* array type and size *) + elt_size * n in
        Sam.optional_field_value_B typ elts >>= fun value ->
        return (value, bytes_read)
      | None ->
        error_string "Too many elements in B-type optional field"
    )
  | c -> error "Incorrect optional field type identifier" c <:sexp_of< char>>

let parse_optional_field iz remaining =
  let tag = input_string_or_fail iz 2 in
  let field_type = Bgzf.input_char iz in
  parse_optional_field_value iz remaining field_type >>= fun (field_value, shift) ->
  Sam.optional_field tag field_value >>= fun field ->
  return (field, shift)

let parse_optional_fields iz remaining =
  let rec loop remaining accu =
    if remaining = 0 then return (List.rev accu)
    else
      parse_optional_field iz remaining >>= fun (field, used_chars) ->
      loop (remaining - 3 - used_chars) (field :: accu)
  in
  loop remaining []

let read_alignment (refseqs : Sam.ref_seq array) iz =
  (* the type annotation for the [refseqs] argument is mandatory to disambiguate
     types [Sam.ref_seq] and [Sam.program]. *)
  let block_size = input_s32_as_int iz in
  (* let block = Bgzf.input_string iz block_size in *)
  (* if String.length block < block_size then parse_error "Premature end of file while reading alignment." ; *)

  let refID = input_s32_as_int iz in
  if refID < -1 || refID > Array.length refseqs
  then raise (Parse_error (Error.create "Incorrect refID field while reading an alignment" refID <:sexp_of<int>>)) ;
  let rname =
    match refID with
    | -1 -> None
    | i -> Some refseqs.(i).Sam.name
  in

  let pos = match Bgzf.input_s32 iz |> Int32.to_int with
    | Some (- 1) -> None
    | Some 2147483647
    | None -> parse_error "A read has a position greater than 2^31"
    | Some n ->
      if n < 0 then parse_error "A read has a negative position"
      else Some (n + 1)
  in

  let bin_mq_nl = Bgzf.input_s32 iz in
  let l_read_name = get_8_0 bin_mq_nl in (* cannot fail since by construction get_8_0 returns integers less than 256 *)
  let mapq = get_16_8 bin_mq_nl in

  let flag_nc = Bgzf.input_s32 iz in
  let flags =
    match Int32.shift_right flag_nc 16 |> Int32.to_int_exn |> Sam.Flags.of_int with
    | Ok flags -> flags
    | Error err -> raise (Parse_error err)
    (* because we are shifting right just before, Int32.to_int_exn cannot fail *)
  in
  let n_cigar_op = get_16_0 flag_nc in

  let l_seq = input_s32_as_int iz in

  let next_refID = input_s32_as_int iz in
  if next_refID < -1 || next_refID > Array.length refseqs
  then raise (Parse_error (Error.create "Incorrect next_refID field while reading an alignment" next_refID <:sexp_of<int>>)) ;

  begin match next_refID with
    | -1 -> return None
    | i -> Sam.parse_rnext refseqs.(i).Sam.name
  end >>= fun rnext ->

  let pnext = match Bgzf.input_s32 iz |> Int32.to_int with
    | Some (- 1) -> None
    | Some 2147483647
    | None -> parse_error "A read has a position > than 2^31"
    | Some n -> if n > 0 then Some (n + 1) else parse_error "A read has a negative next position"
  in

  let tlen = input_s32_as_int iz in

  let read_name =
    let r = input_string_or_fail iz (l_read_name - 1) in
    Pervasives.ignore (Bgzf.input_char iz) ; (* trailing null character *)
    r
  in

  begin
    List.init n_cigar_op ~f:(fun _ ->
        cigar_op_of_s32 (Bgzf.input_s32 iz)
      )
    |> Result.all
  end >>= fun cigar ->

  let seq =
    let r = String.make l_seq ' ' in
    for i = 0 to (l_seq + 1) / 2 - 1 do
      let c = Bgzf.input_u8 iz in
      r.[2 * i] <- char_of_seq_code (c lsr 4) ;
      if 2 * i + 1 < l_seq then r.[2 * i + 1] <- char_of_seq_code (c land 0xf) ;
    done ;
    r in

  let qual =
    match Sam.parse_qual (input_string_or_fail iz l_seq) with
    | Ok q -> q
    | Error err -> raise (Parse_error err) in

  let remaining = block_size - 32 - l_read_name - 4 * n_cigar_op - ((l_seq + 1) / 2) - l_seq in

  let optional_fields =
    match parse_optional_fields iz remaining with
    | Ok optf -> optf
    | Error e -> raise (Parse_error e)
  in

  Sam.alignment
    ~qname:read_name ~flags ?rname ?pos ~mapq ~cigar ?rnext ?pnext
    ~tlen ~seq ~qual ~optional_fields ()

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
