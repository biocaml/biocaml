open Core_kernel
module Result = Biocaml_result
open CFStream
open Or_error

let check b msg =
  if b then Ok ()
  else error_string msg

let checkf b format = Printf.ksprintf (check b) format

let check_buf ~buf ~pos ~len =
  check (String.length buf >= pos + len) "Buffer too short"

(* Helper functions to get parts of an Int32.t as an int *)
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

(* NB: uint32 is unpacked as an int64 *)
let unpack_unsigned_32_little_endian ~buf ~pos =
  let b1 = Char.to_int buf.[pos + 0] |> Int64.of_int_exn in
  let b2 = Char.to_int buf.[pos + 1] in
  let b3 = Char.to_int buf.[pos + 2] in
  let b4 = Char.to_int buf.[pos + 3] in
  Int64.bit_or
    (Int64.shift_left b1 24)
    (Int64.of_int_exn (b2 lor b3 lor b4))

(* This function is adapted from Core.Binary_packing.pack_signed_64_little_endian *)
let pack_u32_in_int64_little_endian ~buf ~pos v =
  (* Safely set the first and last bytes, so that we verify the string bounds. *)
  buf.[pos] <- Char.unsafe_of_int Int64.(to_int_exn (bit_and 0xFFL v));
  buf.[pos + 3] <- Char.unsafe_of_int Int64.(to_int_exn (bit_and 0xFFL (shift_right_logical v 24))) ;
  (* Now we can use [unsafe_set] for the intermediate bytes. *)
  String.unsafe_set buf (pos + 1)
    (Char.unsafe_of_int Int64.(to_int_exn (bit_and 0xFFL (shift_right_logical v 8)))) ;
  String.unsafe_set buf (pos + 2)
    (Char.unsafe_of_int Int64.(to_int_exn (bit_and 0xFFL (shift_right_logical v 16))))

let int64_is_neg n =
  Int64.(bit_and 0x8000000000000000L n <> zero)

let int64_fits_u32 n =
  Int64.(bit_and 0xFFFFFFFF00000000L n = zero)

let int64_fits_u31 n =
  Int64.(bit_and 0xFFFFFFFF80000000L n = zero)

let int64_fits_s32 n =
  if int64_is_neg n then
    int64_fits_u31 (Int64.bit_not n)
  else
    int64_fits_u31 n

(*
   A List.init with possibly failing initializer

   val result_list_init : int -> f:(int -> ('a, 'b) result) -> ('a list, 'b) result
*)
let result_list_init n ~f =
  let rec aux i accu =
    if i < 0 then Ok accu
    else (
      match f i with
      | Ok y -> aux (i - 1) (y :: accu)
      | Error _ as e -> e
    )
  in
  aux (n - 1) []

module Header = struct
  (* This type definition is essentially identical to the Sam version
     but makes parsing faster because in Bam, the seq name of a read
     is stored as an int index in the ref_seq array. *)
  type t = {
    ref_seq : Sam.ref_seq array ;
    sam_header : Sam.header ;
  }

  let to_sam h = h.sam_header
  let of_sam sam_header = {
    ref_seq = Array.of_list sam_header.Sam.ref_seqs ;
    sam_header
  }

end

open Header

type alignment = Sam.alignment

module Alignment0 = struct
  type t = {
    ref_id : int ;
    pos : int ;
    bin_mq_nl : int32 ;
    flag_nc : int32 ;
    next_ref_id : int ;
    pnext : int ;
    tlen : int ;
    read_name : string ;
    cigar : string ;
    seq : string ; (* compressed representation *)
    qual : string ;
    optional : string ;
  } [@@deriving sexp]

  (* ============================ *)
  (* ==== ACCESSOR FUNCTIONS ==== *)
  (* ============================ *)

  (* option constructor for encodings where a special value of the
     input type (here it is [none]) plays the role of [None] *)
  let option ~none x =
    if x = none then None
    else Some x

  let ref_id al = option ~none:(- 1) al.ref_id

  let qname al = option ~none:"*" al.read_name
  (* default is indicated in note 1 of page 14 of the spec *)

  let flags al =
    Int32.shift_right al.flag_nc 16
    |> Int32.to_int_exn (* because we are shifting right just before, Int32.to_int_exn cannot fail *)
    |> Sam.Flags.of_int

  let rname al header =
    try Ok (Option.map (ref_id al) ~f:(fun id -> (Array.get header.ref_seq id).Sam.name))
    with _ -> error_string "Bam.Alignment0.rname: unknown ref_id"

  let pos al = option ~none:(- 1) al.pos

  let mapq al = option ~none:255 (get_16_8 al.bin_mq_nl)

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

  let cigar al =
    result_list_init (String.length al.cigar / 4) ~f:(fun i ->
        let s32 = Binary_packing.unpack_signed_32 ~byte_order:`Little_endian ~buf:al.cigar ~pos:(i * 4) in
        cigar_op_of_s32 s32
      )

  let rnext al header =
    match al.next_ref_id with
    | -1 -> return None
    | i -> Sam.parse_rnext header.ref_seq.(i).Sam.name

  let pnext al = option ~none:(- 1) al.pnext
  (* value for unavailable is the corresponding value in SAM - 1 *)

  let tlen al = option ~none:0 al.tlen

  let l_seq al = String.length al.qual

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

  let seq al =
    let n = String.length al.seq in
    if n = 0 then None
    else
      let l_seq = l_seq al in
      let r = String.make l_seq ' ' in
      for i = 0 to n - 1 do
        let c = int_of_char al.seq.[i] in
        r.[2 * i] <- char_of_seq_code (c lsr 4) ;
        if 2 * i + 1 < l_seq then r.[2 * i + 1] <- char_of_seq_code (c land 0xf) ;
      done ;
      Some r

  let qual al =
    let shift = String.map ~f:Char.(fun c -> of_int_exn (to_int c + 33)) in
    match shift al.qual with
    | qual33 -> Sam.parse_qual qual33
    | exception Failure _ ->
      Or_error.error
        "Bam.Alignement0.qual: incorrect quality score"
        al.qual
        sexp_of_string

  (** Extracts string in buf starting from pos and finishing with a
      NULL character, and returns the position just after it. Returns
      an error if no NULL character is encountered before the end of
      the string. *)
  let parse_cstring buf pos =
    let rec aux i =
      if i < String.length buf then
        if buf.[i] = '\000' then return i
        else aux (i + 1)
      else error_string "Unfinished NULL terminated string"
    in
    aux pos >>= fun pos' ->
    return (String.sub buf ~pos ~len:(pos' - pos), pos' + 1)

  let cCsSiIf_size = function
    | 'c'
    | 'C' -> return 1
    | 's' | 'S' -> return 2
    | 'i' | 'I'
    | 'f' -> return 4
    | _ -> error_string "Incorrect numeric optional field type identifier"

  let parse_cCsSiIf buf pos typ =
    cCsSiIf_size typ >>= fun len ->
    check_buf ~buf ~pos ~len >>= fun () ->
    match typ with
    | 'c' ->
      let i = Int64.of_int_exn (Binary_packing.unpack_signed_8 ~buf ~pos) in
      return (Sam.optional_field_value_i i, len)
    | 'C' ->
      let i = Int64.of_int_exn (Binary_packing.unpack_unsigned_8 ~buf ~pos) in
      return (Sam.optional_field_value_i i, len)
    | 's' ->
      let i = Int64.of_int_exn (Binary_packing.unpack_signed_16_little_endian ~buf ~pos) in
      return (Sam.optional_field_value_i i, len)
    | 'S' ->
      let i = Int64.of_int_exn (Binary_packing.unpack_unsigned_16_little_endian ~buf ~pos) in
      return (Sam.optional_field_value_i i, len)
    | 'i' ->
      let i = Binary_packing.unpack_signed_32 ~byte_order:`Little_endian ~buf ~pos in
      return (Sam.optional_field_value_i (Int64.of_int32 i), len)
    | 'I' ->
      let i = unpack_unsigned_32_little_endian ~buf ~pos in
      return (Sam.optional_field_value_i i, len)
    | 'f' ->
      let f = Binary_packing.unpack_float ~byte_order:`Little_endian ~buf ~pos in
      return (Sam.optional_field_value_f f, len)
    | _ -> error_string "Incorrect numeric optional field type identifier"

  let parse_optional_field_value buf pos = function
    | 'A' ->
      check_buf ~buf ~pos ~len:1 >>= fun () ->
      Sam.optional_field_value_A buf.[pos] >>= fun v ->
      return (v, 1)
    | 'c' | 'C' | 's' | 'S' | 'i' | 'I' | 'f' as typ ->
      parse_cCsSiIf buf pos typ
    | 'Z' ->
      parse_cstring buf pos >>= fun (s, pos') ->
      Sam.optional_field_value_Z s >>= fun value ->
      return (value, pos' - pos)
    | 'H' ->
      parse_cstring buf pos >>= fun (s, pos') ->
      Sam.optional_field_value_H s >>= fun value ->
      return (value, pos' - pos)
    | 'B' ->
      check_buf ~buf ~pos ~len:5 >>= fun () ->
      let typ = buf.[0]  in
      let n = Binary_packing.unpack_signed_32 ~buf ~pos:(pos + 1) ~byte_order:`Little_endian in
      (
        match Int32.to_int n with
        | Some n ->
          cCsSiIf_size typ >>= fun elt_size ->
          check_buf ~buf ~pos:(pos + 5) ~len:(n * elt_size) >>= fun () ->
          let elts = List.init n ~f:(fun i -> String.sub buf ~pos:(pos + 5 + i * elt_size) ~len:elt_size) in
          let bytes_read = 5 (* array type and size *) + elt_size * n in
          Sam.optional_field_value_B typ elts >>= fun value ->
          return (value, bytes_read)
        | None ->
          error_string "Too many elements in B-type optional field"
      )
    | c -> error "Incorrect optional field type identifier" c [%sexp_of: char]

  let parse_optional_field buf pos =
    check_buf ~buf ~pos ~len:3 >>= fun () ->
    let tag = String.sub buf ~pos ~len:2 in
    let field_type = buf.[pos + 2] in
    parse_optional_field_value buf (pos + 3) field_type >>= fun (field_value, shift) ->
    Sam.optional_field tag field_value >>= fun field ->
    return (field, shift + 3)

  let parse_optional_fields buf =
    let rec loop buf pos accu =
      if pos = String.length buf then return (List.rev accu)
      else
        parse_optional_field buf pos >>= fun (field, used_chars) ->
        loop buf (pos + used_chars) (field :: accu)
    in
    loop buf 0 []

  let optional_fields al =
    tag (parse_optional_fields al.optional) "Bam.Alignment0.optional_fields"



  (* ============================ *)
  (* ==== ALIGNMENT DECODING ==== *)
  (* ============================ *)

  (* Alignement0.t -> Alignment.t conversion *)
  let decode al header =
    flags al >>= fun flags ->
    rname al header >>= fun rname ->
    cigar al >>= fun cigar ->
    rnext al header >>= fun rnext ->
    qual al >>= fun qual ->
    optional_fields al >>= fun optional_fields ->
    Sam.alignment
      ?qname:(qname al) ~flags ?rname ?pos:(pos al) ?mapq:(mapq al)
      ~cigar ?rnext ?pnext:(pnext al) ?tlen:(tlen al)
      ?seq:(seq al) ~qual ~optional_fields
      ()

  (* ============================ *)
  (* ==== ALIGNMENT ENCODING ==== *)
  (* ============================ *)

  (* Alignement.t -> Alignment0.t conversion *)
  let find_ref_id header ref_name =
    let open Or_error in
    match Array.findi header.ref_seq ~f:(fun _ rs -> rs.Sam.name = ref_name) with
    | Some (i, _) -> Ok i
    | None  -> error_string "Bam: unknown reference id"


  let string_of_cigar_ops cigar_ops =
    let buf = String.create (List.length cigar_ops * 4) in
    let write ith i32 =
      let pos = ith * 4 in
      Binary_packing.pack_signed_32 ~byte_order:`Little_endian ~buf ~pos i32 in
    let open Int32 in
    List.iteri cigar_ops ~f:(fun idx op ->
        let _, i = match op with
          | `Alignment_match  i -> 0l, i
          | `Insertion i -> 1l, i
          | `Deletion i -> 2l, i
          | `Skipped i -> 3l, i
          | `Soft_clipping i -> 4l, i
          | `Hard_clipping i -> 5l, i
          | `Padding i -> 6l, i
          | `Seq_match i -> 7l, i
          | `Seq_mismatch i -> 8l, i
        in
        write idx (bit_or 0l (of_int_exn Pervasives.(i lsl 4)))
      ) ;
    buf

  let sizeof al =
    let l_seq = l_seq al in
    8 * 4
    + String.length al.read_name + 1 (* NULL terminated string *)
    + String.length al.cigar
    + (l_seq + 1) / 2
    + l_seq
    + String.length al.optional


  (* supposes interval closed at both ends *)
  let reg2bin st ed =
    match st, ed with
    | b, e when b lsr 14 = e lsr 14 ->
      ((1 lsl 15) - 1) / 7  +  (st lsr 14)
    | b, e when b lsr 17 = e lsr 17 ->
      ((1 lsl 12) - 1) / 7  +  (st lsr 17)
    | b, e when b lsr 20 = e lsr 20 ->
      ((1 lsl 9) - 1) / 7  +  (st lsr 20)
    | b, e when b lsr 23 = e lsr 23 ->
      ((1 lsl 6) - 1) / 7  +  (st lsr 23)
    | b, e when b lsr 26 = e lsr 26 ->
      ((1 lsl 3) - 1) / 7  +  (st lsr 26)
    | _ -> 0


  let string_of_optional_fields opt_fields =

    let field_value_encoding = function
      | `B (typ, xs) ->
        Ok ('B',
            sprintf "%c%s" typ (String.concat ~sep:"" xs))

      | `A c ->
        Ok ('A', Char.to_string c)

      | `f f ->
        let bits = Int32.bits_of_float f in
        let buf = String.create 4 in
        Binary_packing.pack_signed_32  ~byte_order:`Little_endian bits ~buf ~pos:0 ;
        Ok ('f', buf)

      | `i i ->
        (* FIXME: encode i to the smallest usable integer type *)
        let buf = String.create 4 in
        if int64_fits_u32 i then (
          pack_u32_in_int64_little_endian ~buf ~pos:0 i ;
          Ok ('I', buf)
        )
        else if int64_fits_s32 i then (
          Binary_packing.pack_signed_32 ~buf ~byte_order:`Little_endian ~pos:0 (Int32.of_int64_exn i) ;
          Ok ('i', buf)
        )
        else error "Sam integer cannot be encoded in BAM format" i Int64.sexp_of_t

      | `H s -> (
          let r = ref [] in
          String.iter s ~f:(fun c ->
              r := sprintf "%02x" (Char.to_int c) :: !r
            ) ;
          Ok ('H',
              String.concat ~sep:"" (List.rev !r) ^ "\000")
        )

      | `Z s ->
        Ok ('Z', s ^ "\000")
    in
    let open Or_error.Monad_infix in
    List.map opt_fields ~f:(fun opt_field ->
        field_value_encoding opt_field.Sam.value >>= fun (c, s) ->
        Ok (sprintf "%s%c%s" opt_field.Sam.tag c s)
      )
    |> Or_error.all
    >>| String.concat ~sep:""

  let int32 i ~ub var =
    if i < ub then
      match Int32.of_int i with
      | Some i -> return i
      | None -> error_string "invalid conversion to int32"
    else
      errorf "invalid conversion to int32 (%s than %d)" var ub

  let encode_bin_mq_nl ~bin ~mapq ~l_read_name =
    let open Int32 in
    int32 bin ~ub:65536 "bin" >>= fun bin ->
    int32 mapq ~ub:256 "mapq" >>= fun mapq ->
    int32 l_read_name ~ub:256 "l_read_name" >>= fun l_read_name ->
    return (
      bit_or
        (shift_left bin 16)
        (bit_or (shift_left mapq 8) l_read_name)
    )

  let encode_flag_nc ~flags ~n_cigar_ops =
    let open Int32 in
    int32 flags ~ub:65536 "flags" >>= fun flags ->
    int32 n_cigar_ops ~ub:65536 "n_cigar_ops" >>= fun n_cigar_ops ->
    return (bit_or (shift_left flags 16) n_cigar_ops)

  let encode al header =
    begin match al.Sam.rname with
    | Some rname -> find_ref_id header rname
    | None -> Ok (-1)
    end
    >>= fun ref_id ->

    let read_name = Option.value ~default:"*" al.Sam.qname in
    let seq = Option.value ~default:"*" al.Sam.seq in

    let pos = (Option.value ~default:0 al.Sam.pos) - 1 in

    let bin = reg2bin pos (pos + String.(length seq)) in
    let mapq = Option.value ~default:255 al.Sam.mapq in
    let l_read_name = String.length read_name + 1 in (* NULL terminated string *)
    encode_bin_mq_nl ~bin ~mapq ~l_read_name
    >>= fun bin_mq_nl ->

    let flags = (al.Sam.flags :> int) in
    let n_cigar_ops = List.length al.Sam.cigar in
    encode_flag_nc ~flags ~n_cigar_ops
    >>= fun flag_nc ->

    begin match al.Sam.rnext with
    | Some `Equal_to_RNAME -> Ok ref_id
    | Some (`Value s) -> find_ref_id header s
    | None -> Ok (-1)
    end
    >>= fun next_ref_id ->

    let pnext = Option.value ~default:0 al.Sam.pnext - 1 in
    let tlen = Option.value ~default:0 al.Sam.tlen in

    let cigar = string_of_cigar_ops al.Sam.cigar in

    Result.List.map al.Sam.qual ~f:(Phred_score.to_char ~offset:`Offset33)
    >>| String.of_char_list
    >>= fun qual ->

    string_of_optional_fields al.Sam.optional_fields >>= fun optional ->

    Ok {
      ref_id; pos; bin_mq_nl; flag_nc; cigar;
      next_ref_id; pnext; tlen; seq; qual; optional;
      read_name
    }

end

let magic_string = "BAM\001"

let input_s32_as_int iz =
  Int32.to_int_exn (Bgzf.input_s32 iz)

let read_sam_header iz =
  try
    let magic = Bgzf.input_string iz 4 in
    check (magic = magic_string) "Incorrect magic string, not a BAM file" >>= fun () ->
    let l_text = input_s32_as_int iz in
     check (l_text >= 0) "Incorrect size of plain text in BAM header" >>= fun () ->
    let text = Bgzf.input_string iz l_text in
    Sam.parse_header text
  with End_of_file -> error_string "EOF while reading BAM header"

let read_one_reference_information iz =
  try
    let l_name = input_s32_as_int iz in
    check (l_name > 0) "Incorrect encoding of reference sequence name in BAM header" >>= fun () ->
    let name = Bgzf.input_string iz (l_name - 1) in
    let _ = Bgzf.input_char iz in (* name is a NULL terminated string *)
    let length = input_s32_as_int iz in
    Sam.ref_seq ~name ~length ()
  with End_of_file -> error_string "EOF while reading BAM reference information"

let read_reference_information iz =
  let rec loop accu n =
    if n = 0 then Ok (List.rev accu)
    else
      match read_one_reference_information iz with
      | Ok refseq -> loop (refseq :: accu) (n - 1)
      | Error _ as e -> e
  in
  try
    let n_ref = input_s32_as_int iz in
    loop [] n_ref
    >>| Array.of_list
  with End_of_file -> error_string "EOF while reading BAM reference information"

let read_alignment_aux iz block_size =
  try
    let ref_id = input_s32_as_int iz in
    begin match Bgzf.input_s32 iz |> Int32.to_int with
      | Some 2147483647 (* POS in BAM is 0-based *)
      | None -> error_string "A read has a position greater than 2^31"
      | Some n ->
        if n < -1 then errorf "A read has a negative position %d" n
        else return n
    end
    >>= fun pos ->

    let bin_mq_nl = Bgzf.input_s32 iz in
    let l_read_name = get_8_0 bin_mq_nl in
    checkf (l_read_name > 0) "Alignment with l_read_name = %d"  l_read_name >>= fun () ->
    let flag_nc = Bgzf.input_s32 iz in
    let n_cigar_ops = get_16_0 flag_nc in
    let l_seq = input_s32_as_int iz in
    check (l_seq >= 0) "Incorrect sequence length in alignment" >>= fun () ->
    let next_ref_id = input_s32_as_int iz in

    begin match Bgzf.input_s32 iz |> Int32.to_int with
    | Some 2147483647
    | None -> error_string "A read has a position > than 2^31"
    | Some n ->
      if n < -1 then errorf "A read has a negative next position %d" n
      else return n
    end
    >>= fun pnext ->

    let tlen = input_s32_as_int iz in

    let read_name =
      let r = Bgzf.input_string iz (l_read_name - 1) in
      Pervasives.ignore (Bgzf.input_char iz) ; (* trailing null character *)
      r
    in

    let cigar = Bgzf.input_string iz (n_cigar_ops * 4) in
    let seq = Bgzf.input_string iz ((l_seq + 1) / 2) in
    let qual = Bgzf.input_string iz l_seq in
    let remaining = block_size - 32 - l_read_name - 4 * n_cigar_ops - ((l_seq + 1) / 2) - l_seq in
    let optional = Bgzf.input_string iz remaining in
    return {
      Alignment0.ref_id ; read_name ; flag_nc ; pos ; bin_mq_nl ; cigar ;
      next_ref_id ; pnext ; tlen ; seq ; qual ; optional
    }
  with End_of_file -> error_string "EOF while reading alignment"

let read_alignment iz =
  try
    let block_size = input_s32_as_int iz in
    Some (read_alignment_aux iz block_size)
  with End_of_file -> None

let read_alignment_stream iz =
  Stream.from (fun _ -> read_alignment iz)

let read0 ic =
  let iz = Bgzf.of_in_channel ic in
  read_sam_header iz >>= fun sam_header ->
  read_reference_information iz >>= fun ref_seq ->
  let header = { sam_header ; ref_seq } in
  return (header, read_alignment_stream iz)


let with_file0 fn ~f =
  In_channel.with_file ~binary:true fn ~f:(fun ic ->
      read0 ic >>= fun (header, alignments) ->
      f header alignments
    )

let write_plain_SAM_header h oz =
  let open Sam in
  let buf = Buffer.create 1024 in
  let add_line x =
    Buffer.add_string buf x ;
    Buffer.add_char buf '\n'
  in
  Option.iter h.version ~f:(fun version ->
      let hl = header_line ~version ?sort_order:h.sort_order () |> ok_exn in (* the construction of the header line must be valid since we are building it from a validated header *)
      add_line (print_header_line hl)
    ) ;
  List.iter h.ref_seqs ~f:(fun x ->
      add_line (print_ref_seq x)
    ) ;
  List.iter h.read_groups ~f:(fun x ->
      add_line (print_read_group x)
    ) ;
  List.iter h.programs ~f:(fun x ->
      add_line (print_program x)
    ) ;
  List.iter h.comments ~f:(fun x ->
      Buffer.add_string buf "@CO\t" ;
      add_line x
    ) ;
  List.iter h.others ~f:(fun x ->
      add_line (print_other x)
    ) ;
  Bgzf.output_s32 oz (Int32.of_int_exn (Buffer.length buf)) ; (* safe conversion of int32 to int: SAM headers less than a few KB *)
  Bgzf.output_string oz (Buffer.contents buf)

let output_null_terminated_string oz s =
  Bgzf.output_string oz s ;
  Bgzf.output_char oz '\000'

let write_reference_sequences h oz =
  let open Sam in
  Bgzf.output_s32 oz (Int32.of_int_exn (Array.length h.ref_seq)) ; (* safe conversion: more than a few million reference sequences cannot happen in practice *)
  Array.iter h.ref_seq ~f:(fun rs ->
      Bgzf.output_s32 oz (Int32.of_int_exn (String.length rs.name + 1)) ; (* safe conversion: the length of the name of a reference sequence is shorter than a few hundreds *)
      output_null_terminated_string oz rs.name ;
      Bgzf.output_s32 oz (Int32.of_int_exn rs.length) ; (* FIXME: the conversion is possibly not safe, but maybe [Sam.ref_seq] type should keep the int32 representation? *)
    )

let write_header header oz =
  Bgzf.output_string oz magic_string ;
  write_plain_SAM_header header.sam_header oz  ;
  write_reference_sequences header oz

let write_alignment oz al =
  let open Alignment0 in
  Bgzf.output_s32 oz (Int32.of_int_exn (sizeof al)) ;
  Bgzf.output_s32 oz (Int32.of_int_exn al.ref_id) ;
  Bgzf.output_s32 oz (Int32.of_int_exn al.pos) ;
  Bgzf.output_s32 oz al.bin_mq_nl ;
  Bgzf.output_s32 oz al.flag_nc ;
  Bgzf.output_s32 oz (Int32.of_int_exn (l_seq al)) ;
  Bgzf.output_s32 oz (Int32.of_int_exn al.next_ref_id) ;
  Bgzf.output_s32 oz (Int32.of_int_exn al.pnext) ;
  Bgzf.output_s32 oz (Int32.of_int_exn al.tlen) ;
  output_null_terminated_string oz al.read_name ;
  Bgzf.output_string oz al.cigar ;
  Bgzf.output_string oz al.seq ;
  Bgzf.output_string oz al.qual ;
  Bgzf.output_string oz al.optional

let write0 header alignments oc =
  let oz = Bgzf.of_out_channel oc in
  write_header header oz ;
  Stream.iter alignments ~f:(write_alignment oz) ;
  Bgzf.dispose_out oz

let bind f x = Or_error.bind x f

let read ic =
  read0 ic >>= fun (header, xs) ->
  Ok (header, Stream.map xs ~f:(bind (fun r -> Alignment0.decode r header)))

let with_file fn ~f = with_file0 fn ~f:(fun header xs ->
    f header (Stream.map xs ~f:(bind (fun r -> Alignment0.decode r header)))
  )

let write h xs oc =
  let module M = struct exception E of Error.t end in
  let xs = Stream.map xs ~f:(fun al ->
      match Alignment0.encode al h with
      | Ok r -> r
      | Error e -> raise (M.E e)
    )
  in
  try write0 h xs oc ; Ok ()
  with M.E e -> Error e
