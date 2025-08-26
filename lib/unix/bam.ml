module Result = Biocaml_result
open Or_error

let check b msg = if b then Ok () else error_string msg
let checkf b format = Printf.ksprintf (check b) format
let check_buf ~buf ~pos ~len = check (String.length buf >= pos + len) "Buffer too short"

(* Helper functions to get parts of an Int32.t as an int *)
let get_8_0 =
  let mask = Int32.of_int_exn 0xff in
  fun x -> Int32.bit_and mask x |> Int32.to_int_exn
;;

let get_16_0 =
  let mask = Int32.of_int_exn 0xffff in
  fun x -> Int32.bit_and mask x |> Int32.to_int_exn
;;

let get_16_8 x = get_8_0 (Int32.shift_right x 8)
let get_32_4 x = Int32.shift_right x 4 |> Int32.to_int_exn

let get_4_0 =
  let mask = Int32.of_int_exn 0xf in
  fun x -> Int32.bit_and mask x |> Int32.to_int_exn
;;

(* this is adapted from JaneStreet's core_kernel Binary_packing
   module in order to read from strings instead of bytes *)
module BP = struct
  let unpack_unsigned_8 ~buf ~pos = Char.to_int (String.get buf pos)

  let unpack_signed_8 ~buf ~pos =
    let n = unpack_unsigned_8 ~buf ~pos in
    if n >= 0x80 then -(0x100 - n) else n
  ;;

  let unpack_signed_32_little_endian ~buf ~pos =
    let b1 =
      Int32.shift_left (Int32.of_int_exn (Char.to_int (String.get buf (pos + 3)))) 24
    in
    let b2 = Char.to_int (String.get buf (pos + 2)) lsl 16 in
    let b3 = Char.to_int (String.get buf (pos + 1)) lsl 8 in
    let b4 = Char.to_int (String.get buf pos) in
    (* LSB *)
    Int32.bit_or b1 (Int32.of_int_exn (b2 lor b3 lor b4))
  ;;

  let unpack_unsigned_16_little_endian ~buf ~pos =
    let b1 = Char.to_int (String.get buf (pos + 1)) lsl 8 in
    let b2 = Char.to_int (String.get buf pos) in
    b1 lor b2
  ;;

  let unpack_signed_16_little_endian ~buf ~pos =
    let n = unpack_unsigned_16_little_endian ~buf ~pos in
    if n >= 0x8000 then -(0x10000 - n) else n
  ;;

  (* NB: uint32 is unpacked as an int64 *)
  let unpack_unsigned_32_little_endian ~buf ~pos =
    let b1 = Char.to_int (String.get buf (pos + 0)) |> Int64.of_int_exn in
    let b2 = Char.to_int (String.get buf (pos + 1)) in
    let b3 = Char.to_int (String.get buf (pos + 2)) in
    let b4 = Char.to_int (String.get buf (pos + 3)) in
    Int64.bit_or (Int64.shift_left b1 24) (Int64.of_int_exn (b2 lor b3 lor b4))
  ;;

  let unpack_signed_64_little_endian ~buf ~pos =
    Int64.bit_or
      (Int64.bit_or
         (Int64.shift_left
            (Int64.of_int_exn
               ((Char.to_int (String.get buf (pos + 7)) lsl 16)
                lor (Char.to_int (String.get buf (pos + 6)) lsl 8)
                lor Char.to_int (String.get buf (pos + 5))))
            40)
         (Int64.shift_left
            (Int64.of_int_exn
               ((Char.to_int (String.get buf (pos + 4)) lsl 16)
                lor (Char.to_int (String.get buf (pos + 3)) lsl 8)
                lor Char.to_int (String.get buf (pos + 2))))
            16))
      (Int64.of_int_exn
         ((Char.to_int (String.get buf (pos + 1)) lsl 8)
          lor Char.to_int (String.get buf pos)))
  ;;

  let unpack_float_little_endian ~buf ~pos =
    Int64.float_of_bits (unpack_signed_64_little_endian ~buf ~pos)
  ;;

  let pack_signed_32_little_endian bits =
    let buf = Bytes.create 4 in
    Binary_packing.pack_signed_32 ~byte_order:`Little_endian bits ~buf ~pos:0;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
  ;;

  let pack_u32_in_int64_little_endian v =
    let buf = Bytes.create 4 in
    (* Safely set the first and last bytes, so that we verify the string bounds. *)
    Bytes.set buf 0 (Char.unsafe_of_int Int64.(to_int_exn (bit_and 0xFFL v)));
    Bytes.set
      buf
      3
      (Char.unsafe_of_int Int64.(to_int_exn (bit_and 0xFFL (shift_right_logical v 24))));
    (* Now we can use [unsafe_set] for the intermediate bytes. *)
    Bytes.unsafe_set
      buf
      1
      (Char.unsafe_of_int Int64.(to_int_exn (bit_and 0xFFL (shift_right_logical v 8))));
    Bytes.unsafe_set
      buf
      2
      (Char.unsafe_of_int Int64.(to_int_exn (bit_and 0xFFL (shift_right_logical v 16))));
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
  ;;
end

let int64_is_neg n = Int64.(bit_and 0x8000000000000000L n <> zero)
let int64_fits_u32 n = Int64.(bit_and 0xFFFFFFFF00000000L n = zero)
let int64_fits_u31 n = Int64.(bit_and 0xFFFFFFFF80000000L n = zero)

let int64_fits_s32 n =
  if int64_is_neg n then int64_fits_u31 (Int64.bit_not n) else int64_fits_u31 n
;;

(*
   A List.init with possibly failing initializer

   val result_list_init : int -> f:(int -> ('a, 'b) result) -> ('a list, 'b) result
*)
let result_list_init n ~f =
  let rec aux i accu =
    if i < 0
    then Ok accu
    else (
      match f i with
      | Ok y -> aux (i - 1) (y :: accu)
      | Error _ as e -> e)
  in
  aux (n - 1) []
;;

module Header = struct
  (* This type definition is essentially identical to the Sam version
     but makes parsing faster because in Bam, the seq name of a read
     is stored as an int index in the ref_seq array. *)
  type t =
    { ref_seq : Biocaml.Sam.Header.SQ.t array
    ; sam_header : Biocaml.Sam.Header.t
    }

  let to_sam h = h.sam_header

  let of_sam sam_header =
    { ref_seq = Array.of_list sam_header.Biocaml.Sam.Header.ref_seqs; sam_header }
  ;;
end

open Header

type alignment = Biocaml.Sam.Alignment.t

module Alignment0 = struct
  type t =
    { ref_id : int
    ; pos : int
    ; bin_mq_nl : int32
    ; flag_nc : int32
    ; next_ref_id : int
    ; pnext : int
    ; tlen : int
    ; read_name : string
    ; cigar : string
    ; seq : string (* compressed representation *)
    ; qual : string
    ; optional : string
    }
  [@@deriving sexp]

  (* ============================ *)
  (* ==== ACCESSOR FUNCTIONS ==== *)
  (* ============================ *)

  (* option constructor for encodings where a special value of the
     input type (here it is [none]) plays the role of [None] *)
  let option ~none x = if Poly.(x = none) then None else Some x
  let ref_id al = option ~none:(-1) al.ref_id
  let qname al = option ~none:"*" al.read_name
  (* default is indicated in note 1 of page 14 of the spec *)

  let flags al =
    Int32.shift_right al.flag_nc 16
    |> Int32.to_int_exn
       (* because we are shifting right just before, Int32.to_int_exn cannot fail *)
    |> Biocaml.Sam.Flags.of_int
  ;;

  let rname al header =
    try Ok (Option.map (ref_id al) ~f:(fun id -> (Array.get header.ref_seq id).name)) with
    | _ -> error_string "Bam.Alignment0.rname: unknown ref_id"
  ;;

  let pos al = option ~none:(-1) al.pos
  let mapq al = option ~none:255 (get_16_8 al.bin_mq_nl)

  let cigar_op_of_s32 x : Biocaml.Sam.Cigar_op.t Or_error.t =
    let op_len = get_32_4 x in
    match get_4_0 x with
    | 0 -> Biocaml.Sam.Cigar_op.cigar_op_alignment_match op_len
    | 1 -> Biocaml.Sam.Cigar_op.cigar_op_insertion op_len
    | 2 -> Biocaml.Sam.Cigar_op.cigar_op_deletion op_len
    | 3 -> Biocaml.Sam.Cigar_op.cigar_op_skipped op_len
    | 4 -> Biocaml.Sam.Cigar_op.cigar_op_soft_clipping op_len
    | 5 -> Biocaml.Sam.Cigar_op.cigar_op_hard_clipping op_len
    | 6 -> Biocaml.Sam.Cigar_op.cigar_op_padding op_len
    | 7 -> Biocaml.Sam.Cigar_op.cigar_op_seq_match op_len
    | 8 -> Biocaml.Sam.Cigar_op.cigar_op_seq_mismatch op_len
    | _ -> assert false
  ;;

  let cigar al =
    result_list_init
      (String.length al.cigar / 4)
      ~f:(fun i ->
        let s32 = BP.unpack_signed_32_little_endian ~buf:al.cigar ~pos:(i * 4) in
        cigar_op_of_s32 s32)
  ;;

  let rnext al header =
    match al.next_ref_id with
    | -1 -> return None
    | i -> Biocaml.Sam.Rnext.parse header.ref_seq.(i).name
  ;;

  let pnext al = option ~none:(-1) al.pnext
  (* value for unavailable is the corresponding value in SAM - 1 *)

  let tlen al = option ~none:0 al.tlen
  let l_seq al = String.length al.qual

  let char_of_seq_code = function
    | 0 -> '='
    | 1 -> 'A'
    | 2 -> 'C'
    | 3 -> 'M'
    | 4 -> 'G'
    | 5 -> 'R'
    | 6 -> 'S'
    | 7 -> 'V'
    | 8 -> 'T'
    | 9 -> 'W'
    | 10 -> 'Y'
    | 11 -> 'H'
    | 12 -> 'K'
    | 13 -> 'D'
    | 14 -> 'B'
    | 15 -> 'N'
    | l -> failwithf "letter not in [0, 15]: %d" l ()
  ;;

  let seq al =
    let n = String.length al.seq in
    if n = 0
    then None
    else (
      let l_seq = l_seq al in
      let r = Bytes.make l_seq ' ' in
      for i = 0 to n - 1 do
        let c = int_of_char al.seq.[i] in
        Bytes.set r (2 * i) (char_of_seq_code (c lsr 4));
        if (2 * i) + 1 < l_seq
        then Bytes.set r ((2 * i) + 1) (char_of_seq_code (c land 0xf))
      done;
      Some (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:r))
  ;;

  let qual al =
    let shift = String.map ~f:Char.(fun c -> of_int_exn (to_int c + 33)) in
    match shift al.qual with
    | qual33 -> Biocaml.Sam.Alignment.parse_qual qual33
    | exception Failure _ ->
      Or_error.error
        "Bam.Alignement0.qual: incorrect quality score"
        al.qual
        sexp_of_string
  ;;

  (** Extracts string in buf starting from pos and finishing with a
      NULL character, and returns the position just after it. Returns
      an error if no NULL character is encountered before the end of
      the string. *)
  let parse_cstring buf pos =
    let rec aux i =
      if i < String.length buf
      then if Char.(String.get buf i = '\000') then return i else aux (i + 1)
      else error_string "Unfinished NULL terminated string"
    in
    aux pos >>= fun pos' -> return (String.sub buf ~pos ~len:(pos' - pos), pos' + 1)
  ;;

  let cCsSiIf_size = function
    | 'c' | 'C' -> return 1
    | 's' | 'S' -> return 2
    | 'i' | 'I' | 'f' -> return 4
    | _ -> error_string "Incorrect numeric optional field type identifier"
  ;;

  let parse_cCsSiIf buf pos typ =
    cCsSiIf_size typ
    >>= fun len ->
    check_buf ~buf ~pos ~len
    >>= fun () ->
    match typ with
    | 'c' ->
      let i = Int64.of_int_exn (BP.unpack_signed_8 ~buf ~pos) in
      return (Biocaml.Sam.Optional_field_value.optional_field_value_i i, len)
    | 'C' ->
      let i = Int64.of_int_exn (BP.unpack_unsigned_8 ~buf ~pos) in
      return (Biocaml.Sam.Optional_field_value.optional_field_value_i i, len)
    | 's' ->
      let i = Int64.of_int_exn (BP.unpack_signed_16_little_endian ~buf ~pos) in
      return (Biocaml.Sam.Optional_field_value.optional_field_value_i i, len)
    | 'S' ->
      let i = Int64.of_int_exn (BP.unpack_unsigned_16_little_endian ~buf ~pos) in
      return (Biocaml.Sam.Optional_field_value.optional_field_value_i i, len)
    | 'i' ->
      let i = BP.unpack_signed_32_little_endian ~buf ~pos in
      return
        (Biocaml.Sam.Optional_field_value.optional_field_value_i (Int64.of_int32 i), len)
    | 'I' ->
      let i = BP.unpack_unsigned_32_little_endian ~buf ~pos in
      return (Biocaml.Sam.Optional_field_value.optional_field_value_i i, len)
    | 'f' ->
      let f = BP.unpack_float_little_endian ~buf ~pos in
      return (Biocaml.Sam.Optional_field_value.optional_field_value_f f, len)
    | _ -> error_string "Incorrect numeric optional field type identifier"
  ;;

  let parse_optional_field_value buf pos = function
    | 'A' ->
      check_buf ~buf ~pos ~len:1
      >>= fun () ->
      Biocaml.Sam.Optional_field_value.optional_field_value_A (String.get buf pos)
      >>= fun v -> return (v, 1)
    | ('c' | 'C' | 's' | 'S' | 'i' | 'I' | 'f') as typ -> parse_cCsSiIf buf pos typ
    | 'Z' ->
      parse_cstring buf pos
      >>= fun (s, pos') ->
      Biocaml.Sam.Optional_field_value.optional_field_value_Z s
      >>= fun value -> return (value, pos' - pos)
    | 'H' ->
      parse_cstring buf pos
      >>= fun (s, pos') ->
      Biocaml.Sam.Optional_field_value.optional_field_value_H s
      >>= fun value -> return (value, pos' - pos)
    | 'B' -> (
      check_buf ~buf ~pos ~len:5
      >>= fun () ->
      let typ = String.get buf 0 in
      let n = BP.unpack_signed_32_little_endian ~buf ~pos:(pos + 1) in
      match Int32.to_int n with
      | Some n ->
        cCsSiIf_size typ
        >>= fun elt_size ->
        check_buf ~buf ~pos:(pos + 5) ~len:(n * elt_size)
        >>= fun () ->
        let elts =
          List.init n ~f:(fun i ->
            String.sub buf ~pos:(pos + 5 + (i * elt_size)) ~len:elt_size)
        in
        let bytes_read = 5 (* array type and size *) + (elt_size * n) in
        Biocaml.Sam.Optional_field_value.optional_field_value_B typ elts
        >>= fun value -> return (value, bytes_read)
      | None -> error_string "Too many elements in B-type optional field")
    | c -> error "Incorrect optional field type identifier" c [%sexp_of: char]
  ;;

  let parse_optional_field buf pos =
    check_buf ~buf ~pos ~len:3
    >>= fun () ->
    let tag = String.sub buf ~pos ~len:2 in
    let field_type = buf.[pos + 2] in
    parse_optional_field_value buf (pos + 3) field_type
    >>= fun (field_value, shift) ->
    Biocaml.Sam.Optional_field.optional_field tag field_value
    >>= fun field -> return (field, shift + 3)
  ;;

  let parse_optional_fields buf =
    let rec loop buf pos accu =
      if pos = String.length buf
      then return (List.rev accu)
      else
        parse_optional_field buf pos
        >>= fun (field, used_chars) -> loop buf (pos + used_chars) (field :: accu)
    in
    loop buf 0 []
  ;;

  let optional_fields al =
    tag (parse_optional_fields al.optional) ~tag:"Bam.Alignment0.optional_fields"
  ;;

  (* ============================ *)
  (* ==== ALIGNMENT DECODING ==== *)
  (* ============================ *)

  (* Alignement0.t -> Alignment.t conversion *)
  let decode al header =
    flags al
    >>= fun flags ->
    rname al header
    >>= fun rname ->
    cigar al
    >>= fun cigar ->
    rnext al header
    >>= fun rnext ->
    qual al
    >>= fun qual ->
    optional_fields al
    >>= fun optional_fields ->
    Biocaml.Sam.Alignment.alignment
      ?qname:(qname al)
      ~flags
      ?rname
      ?pos:(pos al)
      ?mapq:(mapq al)
      ~cigar
      ?rnext
      ?pnext:(pnext al)
      ?tlen:(tlen al)
      ?seq:(seq al)
      ~qual
      ~optional_fields
      ()
  ;;

  (* ============================ *)
  (* ==== ALIGNMENT ENCODING ==== *)
  (* ============================ *)

  (* Alignment.t -> Alignment0.t conversion *)
  let find_ref_id header ref_name =
    let open Or_error in
    match Array.findi header.ref_seq ~f:(fun _ rs -> String.(rs.name = ref_name)) with
    | Some (i, _) -> Ok i
    | None -> error_string "Bam: unknown reference id"
  ;;

  let string_of_cigar_ops cigar_ops =
    let buf = Bytes.create (List.length cigar_ops * 4) in
    let write ith i32 =
      let pos = ith * 4 in
      Binary_packing.pack_signed_32 ~byte_order:`Little_endian ~buf ~pos i32
    in
    let open Int32 in
    List.iteri cigar_ops ~f:(fun idx op ->
      let _, i =
        match op with
        | `Alignment_match i -> 0l, i
        | `Insertion i -> 1l, i
        | `Deletion i -> 2l, i
        | `Skipped i -> 3l, i
        | `Soft_clipping i -> 4l, i
        | `Hard_clipping i -> 5l, i
        | `Padding i -> 6l, i
        | `Seq_match i -> 7l, i
        | `Seq_mismatch i -> 8l, i
      in
      write idx (bit_or 0l (of_int_exn Stdlib.(i lsl 4))));
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
  ;;

  let sizeof al =
    let l_seq = l_seq al in
    (8 * 4)
    + String.length al.read_name
    + 1 (* NULL terminated string *)
    + String.length al.cigar
    + ((l_seq + 1) / 2)
    + l_seq
    + String.length al.optional
  ;;

  (* supposes interval closed at both ends *)
  let reg2bin st ed =
    match st, ed with
    | b, e when b lsr 14 = e lsr 14 -> (((1 lsl 15) - 1) / 7) + (st lsr 14)
    | b, e when b lsr 17 = e lsr 17 -> (((1 lsl 12) - 1) / 7) + (st lsr 17)
    | b, e when b lsr 20 = e lsr 20 -> (((1 lsl 9) - 1) / 7) + (st lsr 20)
    | b, e when b lsr 23 = e lsr 23 -> (((1 lsl 6) - 1) / 7) + (st lsr 23)
    | b, e when b lsr 26 = e lsr 26 -> (((1 lsl 3) - 1) / 7) + (st lsr 26)
    | _ -> 0
  ;;

  let string_of_optional_fields opt_fields =
    let field_value_encoding = function
      | `B (typ, xs) -> Ok ('B', sprintf "%c%s" typ (String.concat ~sep:"" xs))
      | `A c -> Ok ('A', Char.to_string c)
      | `f f ->
        let bits = Int32.bits_of_float f in
        Ok ('f', BP.pack_signed_32_little_endian bits)
      | `i i ->
        (* FIXME: encode i to the smallest usable integer type *)
        if int64_fits_u32 i
        then Ok ('I', BP.pack_u32_in_int64_little_endian i)
        else if int64_fits_s32 i
        then Ok ('i', BP.pack_signed_32_little_endian (Int32.of_int64_exn i))
        else error "Sam integer cannot be encoded in BAM format" i Int64.sexp_of_t
      | `H s ->
        let r = ref [] in
        String.iter s ~f:(fun c -> r := sprintf "%02x" (Char.to_int c) :: !r);
        Ok ('H', String.concat ~sep:"" (List.rev !r) ^ "\000")
      | `Z s -> Ok ('Z', s ^ "\000")
    in
    let open Or_error.Monad_infix in
    List.map opt_fields ~f:(fun opt_field ->
      field_value_encoding opt_field.Biocaml.Sam.Optional_field.value
      >>= fun (c, s) -> Ok (sprintf "%s%c%s" opt_field.Biocaml.Sam.Optional_field.tag c s))
    |> Or_error.all
    >>| String.concat ~sep:""
  ;;

  let int32 i ~ub var =
    if i < ub
    then (
      match Int32.of_int i with
      | Some i -> return i
      | None -> error_string "invalid conversion to int32")
    else errorf "invalid conversion to int32 (%s than %d)" var ub
  ;;

  let encode_bin_mq_nl ~bin ~mapq ~l_read_name =
    let open Int32 in
    int32 bin ~ub:65536 "bin"
    >>= fun bin ->
    int32 mapq ~ub:256 "mapq"
    >>= fun mapq ->
    int32 l_read_name ~ub:256 "l_read_name"
    >>= fun l_read_name ->
    return (bit_or (shift_left bin 16) (bit_or (shift_left mapq 8) l_read_name))
  ;;

  let encode_flag_nc ~flags ~n_cigar_ops =
    let open Int32 in
    int32 flags ~ub:65536 "flags"
    >>= fun flags ->
    int32 n_cigar_ops ~ub:65536 "n_cigar_ops"
    >>= fun n_cigar_ops -> return (bit_or (shift_left flags 16) n_cigar_ops)
  ;;

  let encode al header =
    (match al.Biocaml.Sam.Alignment.rname with
     | Some rname -> find_ref_id header rname
     | None -> Ok (-1))
    >>= fun ref_id ->
    let read_name = Option.value ~default:"*" al.Biocaml.Sam.Alignment.qname in
    let seq = Option.value ~default:"*" al.Biocaml.Sam.Alignment.seq in
    let pos = Option.value ~default:0 al.Biocaml.Sam.Alignment.pos - 1 in
    let bin = reg2bin pos (pos + String.(length seq)) in
    let mapq = Option.value ~default:255 al.Biocaml.Sam.Alignment.mapq in
    let l_read_name = String.length read_name + 1 in
    (* NULL terminated string *)
    encode_bin_mq_nl ~bin ~mapq ~l_read_name
    >>= fun bin_mq_nl ->
    let flags = (al.Biocaml.Sam.Alignment.flags :> int) in
    let n_cigar_ops = List.length al.Biocaml.Sam.Alignment.cigar in
    encode_flag_nc ~flags ~n_cigar_ops
    >>= fun flag_nc ->
    (match al.Biocaml.Sam.Alignment.rnext with
     | Some `Equal_to_RNAME -> Ok ref_id
     | Some (`Value s) -> find_ref_id header s
     | None -> Ok (-1))
    >>= fun next_ref_id ->
    let pnext = Option.value ~default:0 al.Biocaml.Sam.Alignment.pnext - 1 in
    let tlen = Option.value ~default:0 al.Biocaml.Sam.Alignment.tlen in
    let cigar = string_of_cigar_ops al.Biocaml.Sam.Alignment.cigar in
    Result.List.map
      al.Biocaml.Sam.Alignment.qual
      ~f:(Biocaml.Phred_score.to_char ~offset:`Offset33)
    >>| String.of_char_list
    >>= fun qual ->
    string_of_optional_fields al.Biocaml.Sam.Alignment.optional_fields
    >>= fun optional ->
    Ok
      { ref_id
      ; pos
      ; bin_mq_nl
      ; flag_nc
      ; cigar
      ; next_ref_id
      ; pnext
      ; tlen
      ; seq
      ; qual
      ; optional
      ; read_name
      }
  ;;
end

let magic_string = "BAM\001"
let input_s32_as_int iz = Int32.to_int_exn (Bgzf.input_s32 iz)

let read_sam_header iz =
  try
    let magic = Bgzf.input_string iz 4 in
    check String.(magic = magic_string) "Incorrect magic string, not a BAM file"
    >>= fun () ->
    let l_text = input_s32_as_int iz in
    check (l_text >= 0) "Incorrect size of plain text in BAM header"
    >>= fun () ->
    let text = Bgzf.input_string iz l_text in
    Sam.parse_header text
  with
  | End_of_file -> error_string "EOF while reading BAM header"
;;

let read_one_reference_information iz =
  try
    let l_name = input_s32_as_int iz in
    check (l_name > 0) "Incorrect encoding of reference sequence name in BAM header"
    >>= fun () ->
    let name = Bgzf.input_string iz (l_name - 1) in
    let (_ : char) = Bgzf.input_char iz in
    (* name is a NULL terminated string *)
    let length = input_s32_as_int iz in
    Biocaml.Sam.Header.SQ.ref_seq ~name ~length ()
  with
  | End_of_file -> error_string "EOF while reading BAM reference information"
;;

let read_reference_information iz =
  let rec loop accu n =
    if n = 0
    then Ok (List.rev accu)
    else (
      match read_one_reference_information iz with
      | Ok refseq -> loop (refseq :: accu) (n - 1)
      | Error _ as e -> e)
  in
  try
    let n_ref = input_s32_as_int iz in
    loop [] n_ref >>| Array.of_list
  with
  | End_of_file -> error_string "EOF while reading BAM reference information"
;;

let read_alignment_aux iz block_size =
  try
    let ref_id = input_s32_as_int iz in
    (match Bgzf.input_s32 iz |> Int32.to_int with
     | Some 2147483647 (* POS in BAM is 0-based *) | None ->
       error_string "A read has a position greater than 2^31"
     | Some n -> if n < -1 then errorf "A read has a negative position %d" n else return n)
    >>= fun pos ->
    let bin_mq_nl = Bgzf.input_s32 iz in
    let l_read_name = get_8_0 bin_mq_nl in
    checkf (l_read_name > 0) "Alignment with l_read_name = %d" l_read_name
    >>= fun () ->
    let flag_nc = Bgzf.input_s32 iz in
    let n_cigar_ops = get_16_0 flag_nc in
    let l_seq = input_s32_as_int iz in
    check (l_seq >= 0) "Incorrect sequence length in alignment"
    >>= fun () ->
    let next_ref_id = input_s32_as_int iz in
    (match Bgzf.input_s32 iz |> Int32.to_int with
     | Some 2147483647 | None -> error_string "A read has a position > than 2^31"
     | Some n ->
       if n < -1 then errorf "A read has a negative next position %d" n else return n)
    >>= fun pnext ->
    let tlen = input_s32_as_int iz in
    let read_name =
      let r = Bgzf.input_string iz (l_read_name - 1) in
      let (_ : char) = Bgzf.input_char iz in
      (* trailing null character *)
      r
    in
    let cigar = Bgzf.input_string iz (n_cigar_ops * 4) in
    let seq = Bgzf.input_string iz ((l_seq + 1) / 2) in
    let qual = Bgzf.input_string iz l_seq in
    let remaining =
      block_size - 32 - l_read_name - (4 * n_cigar_ops) - ((l_seq + 1) / 2) - l_seq
    in
    let optional = Bgzf.input_string iz remaining in
    return
      { Alignment0.ref_id
      ; read_name
      ; flag_nc
      ; pos
      ; bin_mq_nl
      ; cigar
      ; next_ref_id
      ; pnext
      ; tlen
      ; seq
      ; qual
      ; optional
      }
  with
  | End_of_file -> error_string "EOF while reading alignment"
;;

let read_alignment iz =
  try
    let block_size = input_s32_as_int iz in
    Some (read_alignment_aux iz block_size)
  with
  | End_of_file -> None
;;

let read_alignment_stream iz = Stream.from (fun _ -> read_alignment iz)

let read_header iz =
  read_sam_header iz
  >>= fun sam_header ->
  read_reference_information iz >>= fun ref_seq -> Ok { sam_header; ref_seq }
;;

let read0 ic =
  let iz = Bgzf.of_in_channel ic in
  read_header iz >>= fun header -> return (header, read_alignment_stream iz)
;;

let with_file0 fn ~f =
  In_channel.with_file ~binary:true fn ~f:(fun ic ->
    read0 ic >>= fun (header, alignments) -> f header alignments)
;;

let write_plain_SAM_header h oz =
  let buf = Buffer.create 1024 in
  let add_line x =
    Buffer.add_string buf x;
    Buffer.add_char buf '\n'
  in
  Option.iter h.Biocaml.Sam.Header.version ~f:(fun version ->
    let hl =
      Biocaml.Sam.Header.HD.header_line
        ~version
        ?sort_order:h.Biocaml.Sam.Header.sort_order
        ()
      |> ok_exn
    in
    (* the construction of the header line must be valid since we are building it from a validated header *)
    add_line (Biocaml.Sam.Header.HD.print hl));
  List.iter h.Biocaml.Sam.Header.ref_seqs ~f:(fun x ->
    add_line (Biocaml.Sam.Header.SQ.print x));
  List.iter h.Biocaml.Sam.Header.read_groups ~f:(fun x ->
    add_line (Biocaml.Sam.Header.RG.print x));
  List.iter h.Biocaml.Sam.Header.programs ~f:(fun x ->
    add_line (Biocaml.Sam.Header.PG.print x));
  List.iter h.Biocaml.Sam.Header.comments ~f:(fun x ->
    Buffer.add_string buf "@CO\t";
    add_line x);
  List.iter h.Biocaml.Sam.Header.others ~f:(fun x ->
    add_line (Biocaml.Sam.Header.print_other x));
  Bgzf.output_s32 oz (Int32.of_int_exn (Buffer.length buf));
  (* safe conversion of int32 to int: SAM headers less than a few KB *)
  Bgzf.output_string oz (Buffer.contents buf)
;;

let output_null_terminated_string oz s =
  Bgzf.output_string oz s;
  Bgzf.output_char oz '\000'
;;

let write_reference_sequences h oz =
  Bgzf.output_s32 oz (Int32.of_int_exn (Array.length h.ref_seq));
  (* safe conversion: more than a few million reference sequences cannot happen in practice *)
  Array.iter h.ref_seq ~f:(fun rs ->
    Bgzf.output_s32 oz (Int32.of_int_exn (String.length rs.name + 1));
    (* safe conversion: the length of the name of a reference sequence is shorter than a few hundreds *)
    output_null_terminated_string oz rs.name;
    Bgzf.output_s32 oz (Int32.of_int_exn rs.length)
    (* FIXME: the conversion is possibly not safe, but maybe [Sam.ref_seq] type should keep the int32 representation? *))
;;

let write_header header oz =
  Bgzf.output_string oz magic_string;
  write_plain_SAM_header header.sam_header oz;
  write_reference_sequences header oz
;;

let write_alignment oz al =
  let open Alignment0 in
  Bgzf.output_s32 oz (Int32.of_int_exn (sizeof al));
  Bgzf.output_s32 oz (Int32.of_int_exn al.ref_id);
  Bgzf.output_s32 oz (Int32.of_int_exn al.pos);
  Bgzf.output_s32 oz al.bin_mq_nl;
  Bgzf.output_s32 oz al.flag_nc;
  Bgzf.output_s32 oz (Int32.of_int_exn (l_seq al));
  Bgzf.output_s32 oz (Int32.of_int_exn al.next_ref_id);
  Bgzf.output_s32 oz (Int32.of_int_exn al.pnext);
  Bgzf.output_s32 oz (Int32.of_int_exn al.tlen);
  output_null_terminated_string oz al.read_name;
  Bgzf.output_string oz al.cigar;
  Bgzf.output_string oz al.seq;
  Bgzf.output_string oz al.qual;
  Bgzf.output_string oz al.optional
;;

let write0 header alignments oc =
  let oz = Bgzf.of_out_channel oc in
  write_header header oz;
  CFStream.iter alignments ~f:(write_alignment oz);
  Bgzf.dispose_out oz
;;

let bind f x = Or_error.bind x ~f

let read ic =
  read0 ic
  >>= fun (header, xs) ->
  Ok (header, CFStream.map xs ~f:(bind (fun r -> Alignment0.decode r header)))
;;

let with_file fn ~f =
  with_file0 fn ~f:(fun header xs ->
    f header (CFStream.map xs ~f:(bind (fun r -> Alignment0.decode r header))))
;;

let write h xs oc =
  let module M = struct
    exception E of Error.t
  end
  in
  let xs =
    CFStream.map xs ~f:(fun al ->
      match Alignment0.encode al h with
      | Ok r -> r
      | Error e -> raise (M.E e))
  in
  try
    write0 h xs oc;
    Ok ()
  with
  | M.E e -> Error e
;;

module Test = struct
  let assert_equal
        ?(msg : string option)
        ?(printer : ('a -> Sexp.t) option)
        ?(cmp : ('a -> 'a -> bool) option)
        (x : 'a)
        (y : 'a)
    =
    let printer : 'a -> string =
      match printer with
      | None -> fun _ -> "<no printer>"
      | Some p -> fun x -> Sexp.to_string_hum (p x)
    in
    printf
      "%s: %s = %s: %b\n"
      (match msg with
       | Some x -> sprintf "%s: " x
       | None -> "")
      (printer x)
      (printer y)
      (match cmp with
       | Some cmp -> cmp x y
       | None -> Poly.equal x y)
  ;;

  let assert_headers h1 h2 =
    let h1, h2 = Header.to_sam h1, Header.to_sam h2 in
    assert_equal ~msg:"version" ~printer:[%sexp_of: string option] h1.version h2.version;
    assert_equal
      ~msg:"sort_order"
      ~printer:[%sexp_of: Biocaml.Sam.Header.HD.SO.t option]
      h1.sort_order
      h2.sort_order;
    assert_equal
      ~msg:"ref_seqs"
      ~printer:[%sexp_of: Biocaml.Sam.Header.SQ.t list]
      h1.ref_seqs
      h2.ref_seqs;
    assert_equal
      ~msg:"read_groups"
      ~printer:[%sexp_of: Biocaml.Sam.Header.RG.t list]
      h1.read_groups
      h2.read_groups;
    assert_equal
      ~msg:"programs"
      ~printer:[%sexp_of: Biocaml.Sam.Header.PG.t list]
      h1.programs
      h2.programs;
    assert_equal ~msg:"comments" ~printer:[%sexp_of: string list] h1.comments h2.comments;
    assert_equal
      ~msg:"others"
      ~printer:[%sexp_of: (string * Biocaml.Sam.Header.Tag_value.t list) list]
      h1.others
      h2.others;
    ()
  ;;

  let assert_alignment ~qname ~rname ~mapq ~n_cigar_ops ~seq header al =
    assert_equal
      ~msg:"wrong rname"
      ~printer:[%sexp_of: string option]
      rname
      (Alignment0.rname al header |> ok_exn);
    assert_equal
      ~msg:"wrong mapq"
      ~printer:[%sexp_of: int option]
      mapq
      (Alignment0.mapq al);
    assert_equal
      ~msg:"wrong tlen"
      ~printer:[%sexp_of: int option]
      None
      (Alignment0.tlen al);
    assert_equal
      ~msg:"wrong read_name"
      ~printer:[%sexp_of: string option]
      qname
      (Alignment0.qname al);
    assert_equal
      ~msg:"wrong n_cigar_ops"
      ~printer:Int.sexp_of_t
      n_cigar_ops
      (List.length (ok_exn (Alignment0.cigar al)));
    assert_equal
      ~msg:"wrong seq"
      ~printer:String.sexp_of_t
      seq
      (Option.value_exn (Alignment0.seq al));
    ()
  ;;

  let assert_alignments header al1 al2 =
    let open Alignment0 in
    assert_equal
      ~msg:"rname"
      ~printer:[%sexp_of: string option Or_error.t]
      (rname al1 header)
      (rname al2 header);
    assert_equal ~msg:"mapq" ~printer:[%sexp_of: int option] (mapq al1) (mapq al2);
    assert_equal ~msg:"tlen" ~printer:[%sexp_of: int option] (tlen al1) (tlen al2);
    assert_equal
      ~msg:"read_name"
      ~printer:[%sexp_of: string option]
      (qname al1)
      (qname al2);
    assert_equal
      ~msg:"n_cigar_ops"
      ~printer:Int.sexp_of_t
      (List.length (ok_exn (cigar al1)))
      (List.length (ok_exn (cigar al2)));
    assert_equal
      ~msg:"seq"
      ~printer:String.sexp_of_t
      (Option.value_exn (seq al1))
      (Option.value_exn (seq al2));
    ()
  ;;

  let%expect_test "test_read" =
    with_file0 "../../etc/test_data/bam_01.bam" ~f:(fun header alignments ->
      let sh = Header.to_sam header in
      assert_equal
        ~msg:"Sam version"
        ~printer:[%sexp_of: string option]
        (Some "1.0")
        sh.version;
      assert_equal ~msg:"Sort order" (Some `Unsorted) sh.sort_order;
      assert_equal
        ~msg:"Number of ref sequences"
        ~printer:Int.sexp_of_t
        22
        (List.length sh.ref_seqs);
      let al0 = CFStream.next_exn alignments |> ok_exn in
      assert_alignment
        ~qname:(Some "ILLUMINA-D118D2_0040_FC:7:20:2683:16044#0/1")
        ~rname:(Some "chr1")
        ~mapq:None
        ~n_cigar_ops:1
        ~seq:"TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT"
        header
        al0;
      let remaining_alignments = CFStream.to_list alignments in
      assert_equal
        ~msg:"Number of alignments"
        ~printer:Int.sexp_of_t
        25
        (List.length remaining_alignments);
      Ok ())
    |> ok_exn;
    [%expect
      {|
      Sam version: : (1.0) = (1.0): true
      Sort order: : <no printer> = <no printer>: true
      Number of ref sequences: : 22 = 22: true
      wrong rname: : (chr1) = (chr1): true
      wrong mapq: : () = (): true
      wrong tlen: : () = (): true
      wrong read_name: : (ILLUMINA-D118D2_0040_FC:7:20:2683:16044#0/1) = (ILLUMINA-D118D2_0040_FC:7:20:2683:16044#0/1): true
      wrong n_cigar_ops: : 1 = 1: true
      wrong seq: : TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT = TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT: true
      Number of alignments: : 25 = 25: true
    |}];
    Ok () (* FIXME: No idea why this is needed. *)
  ;;

  let%expect_test "test_read_write_and_read" =
    let bamfile = "../../etc/test_data/bam_01.bam" in
    Utils.with_temp_file "biocaml" ".bam" ~f:(fun fn ->
      with_file0 bamfile ~f:(fun header alignments ->
        Out_channel.with_file fn ~f:(write0 header (CFStream.map alignments ~f:ok_exn));
        Ok ())
      |> ok_exn;
      with_file0 bamfile ~f:(fun ref_header ref_alignments ->
        with_file0 fn ~f:(fun header alignments ->
          assert_headers ref_header header;
          try
            let x =
              CFStream.Result.map2_exn'
                ref_alignments
                alignments
                ~f:(assert_alignments ref_header)
              |> CFStream.Result.fold' ~init:() ~f:(fun () () -> ())
            in
            printf "%b\n" true;
            x
          with
          | CFStream.Expected_streams_of_equal_length ->
            failwith "Original and written files don't have the same number of alignments")))
    |> ok_exn;
    [%expect
      {|
      version: : (1.0) = (1.0): true
      sort_order: : (Unsorted) = (Unsorted): true
      ref_seqs: : (((name chr1) (length 197195432) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr2) (length 181748087) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr3) (length 159599783) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr4) (length 155630120) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr5) (length 152537259) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr6) (length 149517037) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr7) (length 152524553) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr8) (length 131738871) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr9) (length 124076172) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chrM) (length 16299) (assembly ()) (md5 ()) (species ()) (uri ()))
       ((name chrX) (length 166650296) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chrY) (length 15902555) (assembly ()) (md5 ()) (species ()) (uri ()))
       ((name chr10) (length 129993255) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr11) (length 121843856) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr12) (length 121257530) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr13) (length 120284312) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr14) (length 125194864) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr15) (length 103494974) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr16) (length 98319150) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr17) (length 95272651) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr18) (length 90772031) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr19) (length 61342430) (assembly ()) (md5 ()) (species ())
        (uri ()))) = (((name chr1) (length 197195432) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr2) (length 181748087) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr3) (length 159599783) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr4) (length 155630120) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr5) (length 152537259) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr6) (length 149517037) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr7) (length 152524553) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr8) (length 131738871) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr9) (length 124076172) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chrM) (length 16299) (assembly ()) (md5 ()) (species ()) (uri ()))
       ((name chrX) (length 166650296) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chrY) (length 15902555) (assembly ()) (md5 ()) (species ()) (uri ()))
       ((name chr10) (length 129993255) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr11) (length 121843856) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr12) (length 121257530) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr13) (length 120284312) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr14) (length 125194864) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr15) (length 103494974) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr16) (length 98319150) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr17) (length 95272651) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr18) (length 90772031) (assembly ()) (md5 ()) (species ())
        (uri ()))
       ((name chr19) (length 61342430) (assembly ()) (md5 ()) (species ())
        (uri ()))): true
      read_groups: : () = (): true
      programs: : (((id Bowtie) (name ())
        (command_line
         ("\"bowtie -S -n 3 -l 36 -e 70 -m 1 -p 7 /home/pveber/w/prabi-benoit-chIP-panRAR/_guizmin/cache/ca93d9566ac878a4b5808dc435623401/index /home/pveber/w/prabi-benoit-chIP-panRAR/_guizmin/cache/7194ab4f760983758d686ca5a5fe989a /home/pveber/w/prabi-benoit-chIP-panRAR/_guizmin/tmp/9525e1f572fabe8bcf861dde1442ec6b\""))
        (previous_id ()) (description ()) (version (0.12.9)))) = (((id Bowtie) (name ())
        (command_line
         ("\"bowtie -S -n 3 -l 36 -e 70 -m 1 -p 7 /home/pveber/w/prabi-benoit-chIP-panRAR/_guizmin/cache/ca93d9566ac878a4b5808dc435623401/index /home/pveber/w/prabi-benoit-chIP-panRAR/_guizmin/cache/7194ab4f760983758d686ca5a5fe989a /home/pveber/w/prabi-benoit-chIP-panRAR/_guizmin/tmp/9525e1f572fabe8bcf861dde1442ec6b\""))
        (previous_id ()) (description ()) (version (0.12.9)))): true
      comments: : () = (): true
      others: : () = (): true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:20:2683:16044#0/1) = (ILLUMINA-D118D2_0040_FC:7:20:2683:16044#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT = TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:23:11267:11746#0/1) = (ILLUMINA-D118D2_0040_FC:7:23:11267:11746#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT = TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:38:5490:10771#0/1) = (ILLUMINA-D118D2_0040_FC:7:38:5490:10771#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT = TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:64:12309:3545#0/1) = (ILLUMINA-D118D2_0040_FC:7:64:12309:3545#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT = TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:90:2470:10383#0/1) = (ILLUMINA-D118D2_0040_FC:7:90:2470:10383#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT = TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:106:8122:13018#0/1) = (ILLUMINA-D118D2_0040_FC:7:106:8122:13018#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT = TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:113:3145:13813#0/1) = (ILLUMINA-D118D2_0040_FC:7:113:3145:13813#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT = TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:120:3557:16472#0/1) = (ILLUMINA-D118D2_0040_FC:7:120:3557:16472#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT = TTTTGTCCTTCTTTTATTCCTATTTTTCTTAGGTTT: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:89:15967:2210#0/1) = (ILLUMINA-D118D2_0040_FC:7:89:15967:2210#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : TTTTTTAGATTTAACATTTCTGTCATAGATTAATCT = TTTTTTAGATTTAACATTTCTGTCATAGATTAATCT: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:99:2852:17993#0/1) = (ILLUMINA-D118D2_0040_FC:7:99:2852:17993#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : ATTTTCCCATCTCCTCCCTAATTAGAGCCATTCTGG = ATTTTCCCATCTCCTCCCTAATTAGAGCCATTCTGG: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:21:13804:4743#0/1) = (ILLUMINA-D118D2_0040_FC:7:21:13804:4743#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : CCATCCCACAGTGGTCCCTTTCTCCTTCTCTGGTTT = CCATCCCACAGTGGTCCCTTTCTCCTTCTCTGGTTT: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:25:4154:16191#0/1) = (ILLUMINA-D118D2_0040_FC:7:25:4154:16191#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : CCATCCCACAGTGGTCCCTTTCTCCTTCTCTGGTTT = CCATCCCACAGTGGTCCCTTTCTCCTTCTCTGGTTT: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:46:12968:20299#0/1) = (ILLUMINA-D118D2_0040_FC:7:46:12968:20299#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : CCATCCCACAGTGGTCCCTTTCTCCTTCTCTGGTTT = CCATCCCACAGTGGTCCCTTTCTCCTTCTCTGGTTT: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:90:4724:17349#0/1) = (ILLUMINA-D118D2_0040_FC:7:90:4724:17349#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : CCATCCCACAGTGGTCCCTTTCTCCTTCTCTGGTTT = CCATCCCACAGTGGTCCCTTTCTCCTTCTCTGGTTT: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:100:15207:17337#0/1) = (ILLUMINA-D118D2_0040_FC:7:100:15207:17337#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : CCATCCCACAGTGGTCCCTTTCTCCTTCTCTGGTTT = CCATCCCACAGTGGTCCCTTTCTCCTTCTCTGGTTT: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:4:13652:16578#0/1) = (ILLUMINA-D118D2_0040_FC:7:4:13652:16578#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : ATATTCATAAAGTATGATTGTATTGTGCATACTACA = ATATTCATAAAGTATGATTGTATTGTGCATACTACA: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:12:15599:13695#0/1) = (ILLUMINA-D118D2_0040_FC:7:12:15599:13695#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : ATATTCATAAAGTATGATTGTATTGTGCATACTACA = ATATTCATAAAGTATGATTGTATTGTGCATACTACA: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:19:9449:9103#0/1) = (ILLUMINA-D118D2_0040_FC:7:19:9449:9103#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : ATATTCATAAAGTATGATTGTATTGTGCATACTACA = ATATTCATAAAGTATGATTGTATTGTGCATACTACA: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:20:13653:11608#0/1) = (ILLUMINA-D118D2_0040_FC:7:20:13653:11608#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : ATATTCATAAAGTATGATTGTATTGTGCATACTACA = ATATTCATAAAGTATGATTGTATTGTGCATACTACA: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:34:16534:19508#0/1) = (ILLUMINA-D118D2_0040_FC:7:34:16534:19508#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : ATATTCATAAAGTATGATTGTATTGTGCATACTACA = ATATTCATAAAGTATGATTGTATTGTGCATACTACA: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:40:4227:16763#0/1) = (ILLUMINA-D118D2_0040_FC:7:40:4227:16763#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : ATATTCATAAAGTATGATTGTATTGTGCATACTACA = ATATTCATAAAGTATGATTGTATTGTGCATACTACA: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:60:16048:1210#0/1) = (ILLUMINA-D118D2_0040_FC:7:60:16048:1210#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : ATATTCATAAAGTATGATTGTATTGTGCATACTACA = ATATTCATAAAGTATGATTGTATTGTGCATACTACA: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:73:12776:1215#0/1) = (ILLUMINA-D118D2_0040_FC:7:73:12776:1215#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : ATATTCATAAAGTATGATTGTATTGTGCATACTACA = ATATTCATAAAGTATGATTGTATTGTGCATACTACA: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:100:18884:2185#0/1) = (ILLUMINA-D118D2_0040_FC:7:100:18884:2185#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : ATATTCATAAAGTATGATTGTATTGTGCATACTACA = ATATTCATAAAGTATGATTGTATTGTGCATACTACA: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:19:9587:11397#0/1) = (ILLUMINA-D118D2_0040_FC:7:19:9587:11397#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : GCTGTCGTGAAGACCACAGTGTTCACCACCTTGCTG = GCTGTCGTGAAGACCACAGTGTTCACCACCTTGCTG: true
      rname: : (Ok (chr1)) = (Ok (chr1)): true
      mapq: : () = (): true
      tlen: : () = (): true
      read_name: : (ILLUMINA-D118D2_0040_FC:7:27:3398:15663#0/1) = (ILLUMINA-D118D2_0040_FC:7:27:3398:15663#0/1): true
      n_cigar_ops: : 1 = 1: true
      seq: : GCTGTCGTGAAGACCACAGTGTTCACCACCTTGCTG = GCTGTCGTGAAGACCACAGTGTTCACCACCTTGCTG: true
      true
    |}];
    Ok () (* FIXME: No idea why this is needed. *)
  ;;
end
