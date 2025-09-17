module Result = Biocaml_result
open Or_error.Let_syntax

let check b msg = if b then Ok () else Or_error.error_string msg
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
    { ref_seq = Array.of_list (Biocaml.Sam.Header.ref_seqs sam_header); sam_header }
  ;;
end

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
  let qname al : string option = option ~none:"*" al.read_name

  (* default is indicated in note 1 of page 14 of the spec *)

  let flags al =
    Int32.shift_right al.flag_nc 16
    |> Int32.to_int_exn
       (* because we are shifting right just before, Int32.to_int_exn cannot fail *)
    |> Biocaml.Sam.Flag.of_int
  ;;

  let rname al header =
    try
      Ok (Option.map (ref_id al) ~f:(fun id -> (Array.get header.Header.ref_seq id).name))
    with
    | _ -> Or_error.error_string "Bam.Alignment0.rname: unknown ref_id"
  ;;

  let pos al = option ~none:(-1) al.pos
  let mapq al = option ~none:255 (get_16_8 al.bin_mq_nl)

  let cigar_op_of_s32 x : Biocaml.Sam.Cigar.Op.t Or_error.t =
    let op_len = get_32_4 x in
    match get_4_0 x with
    | 0 -> Biocaml.Sam.Cigar.Op.alignment_match_of_int op_len
    | 1 -> Biocaml.Sam.Cigar.Op.insertion_of_int op_len
    | 2 -> Biocaml.Sam.Cigar.Op.deletion_of_int op_len
    | 3 -> Biocaml.Sam.Cigar.Op.skipped_of_int op_len
    | 4 -> Biocaml.Sam.Cigar.Op.soft_clipping_of_int op_len
    | 5 -> Biocaml.Sam.Cigar.Op.hard_clipping_of_int op_len
    | 6 -> Biocaml.Sam.Cigar.Op.padding_of_int op_len
    | 7 -> Biocaml.Sam.Cigar.Op.seq_match_of_int op_len
    | 8 -> Biocaml.Sam.Cigar.Op.seq_mismatch_of_int op_len
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
    | -1 -> Ok None
    | i -> Biocaml.Sam.Rnext.t_option_of_string header.Header.ref_seq.(i).name
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
    | qual33 -> Biocaml.Sam.Qual.of_string qual33
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
      then if Char.(String.get buf i = '\000') then Ok i else aux (i + 1)
      else Or_error.error_string "Unfinished NULL terminated string"
    in
    let%bind pos' = aux pos in
    Ok (String.sub buf ~pos ~len:(pos' - pos), pos' + 1)
  ;;

  let cCsSiIf_size = function
    | 'c' | 'C' -> Ok 1
    | 's' | 'S' -> Ok 2
    | 'i' | 'I' | 'f' -> Ok 4
    | _ -> Or_error.error_string "Incorrect numeric optional field type identifier"
  ;;

  let parse_cCsSiIf buf pos typ =
    let%bind len = cCsSiIf_size typ in
    let%bind () = check_buf ~buf ~pos ~len in
    match typ with
    | 'c' ->
      let i = Int64.of_int_exn (BP.unpack_signed_8 ~buf ~pos) in
      Ok (Biocaml.Sam.Optional_field.Value.of_int64_i i, len)
    | 'C' ->
      let i = Int64.of_int_exn (BP.unpack_unsigned_8 ~buf ~pos) in
      Ok (Biocaml.Sam.Optional_field.Value.of_int64_i i, len)
    | 's' ->
      let i = Int64.of_int_exn (BP.unpack_signed_16_little_endian ~buf ~pos) in
      Ok (Biocaml.Sam.Optional_field.Value.of_int64_i i, len)
    | 'S' ->
      let i = Int64.of_int_exn (BP.unpack_unsigned_16_little_endian ~buf ~pos) in
      Ok (Biocaml.Sam.Optional_field.Value.of_int64_i i, len)
    | 'i' ->
      let i = BP.unpack_signed_32_little_endian ~buf ~pos in
      Ok (Biocaml.Sam.Optional_field.Value.of_int64_i (Int64.of_int32 i), len)
    | 'I' ->
      let i = BP.unpack_unsigned_32_little_endian ~buf ~pos in
      Ok (Biocaml.Sam.Optional_field.Value.of_int64_i i, len)
    | 'f' ->
      let f = BP.unpack_float_little_endian ~buf ~pos in
      Ok (Biocaml.Sam.Optional_field.Value.of_float_f f, len)
    | _ -> Or_error.error_string "Incorrect numeric optional field type identifier"
  ;;

  let parse_optional_field_value buf pos = function
    | 'A' ->
      let%bind () = check_buf ~buf ~pos ~len:1 in
      let%bind v = Biocaml.Sam.Optional_field.Value.of_char_A (String.get buf pos) in
      Ok (v, 1)
    | ('c' | 'C' | 's' | 'S' | 'i' | 'I' | 'f') as typ -> parse_cCsSiIf buf pos typ
    | 'Z' ->
      let%bind s, pos' = parse_cstring buf pos in
      let%bind value = Biocaml.Sam.Optional_field.Value.of_string_Z s in
      Ok (value, pos' - pos)
    | 'H' ->
      let%bind s, pos' = parse_cstring buf pos in
      let%bind value = Biocaml.Sam.Optional_field.Value.of_string_H s in
      Ok (value, pos' - pos)
    | 'B' -> (
      let%bind () = check_buf ~buf ~pos ~len:5 in
      let typ = String.get buf 0 in
      let n = BP.unpack_signed_32_little_endian ~buf ~pos:(pos + 1) in
      match Int32.to_int n with
      | Some n ->
        let%bind elt_size = cCsSiIf_size typ in
        let%bind () = check_buf ~buf ~pos:(pos + 5) ~len:(n * elt_size) in
        let elts =
          List.init n ~f:(fun i ->
            String.sub buf ~pos:(pos + 5 + (i * elt_size)) ~len:elt_size)
        in
        let bytes_read = 5 (* array type and size *) + (elt_size * n) in
        let%bind value =
          Biocaml.Sam.Optional_field.Value.of_char_string_list_B typ elts
        in
        Ok (value, bytes_read)
      | None -> Or_error.error_string "Too many elements in B-type optional field")
    | c -> error "Incorrect optional field type identifier" c [%sexp_of: char]
  ;;

  let parse_optional_field buf pos =
    let%bind () = check_buf ~buf ~pos ~len:3 in
    let tag = String.sub buf ~pos ~len:2 in
    let field_type = buf.[pos + 2] in
    let%bind field_value, shift = parse_optional_field_value buf (pos + 3) field_type in
    let%bind field = Biocaml.Sam.Optional_field.make tag field_value in
    Ok (field, shift + 3)
  ;;

  let parse_optional_fields buf =
    let rec loop buf pos accu =
      if pos = String.length buf
      then Ok (List.rev accu)
      else (
        let%bind field, used_chars = parse_optional_field buf pos in
        loop buf (pos + used_chars) (field :: accu))
    in
    loop buf 0 []
  ;;

  let optional_fields al =
    Or_error.tag (parse_optional_fields al.optional) ~tag:"Bam.Alignment0.optional_fields"
  ;;

  (* ============================ *)
  (* ==== ALIGNMENT DECODING ==== *)
  (* ============================ *)

  (* Alignement0.t -> Alignment.t conversion *)
  let decode al header =
    let%bind qname =
      match qname al with
      | None -> Ok None
      | Some qname -> Biocaml.Sam.Qname.t_option_of_string qname
    in
    let%bind flag = flags al in
    let%bind rname : Biocaml.Sam.Rname.t option Or_error.t =
      match rname al header with
      | Error _ as e -> e
      | Ok None as x -> x
      | Ok (Some rname) -> Biocaml.Sam.Rname.t_option_of_string rname
    in
    let%bind pos =
      match pos al with
      | None -> Ok None
      | Some pos -> Biocaml.Sam.Pos.t_option_of_string (Int.to_string pos)
    in
    let%bind mapq =
      match mapq al with
      | None -> Ok None
      | Some mapq -> Biocaml.Sam.Mapq.t_option_of_string (Int.to_string mapq)
    in
    let%bind cigar = cigar al in
    let%bind rnext = rnext al header in
    let%bind pnext =
      match pnext al with
      | None -> Ok None
      | Some pnext -> Biocaml.Sam.Pnext.t_option_of_string (Int.to_string pnext)
    in
    let%bind tlen =
      match tlen al with
      | None -> Ok None
      | Some tlen -> Biocaml.Sam.Tlen.t_option_of_string (Int.to_string tlen)
    in
    let%bind seq =
      match seq al with
      | None -> Ok None
      | Some seq -> Biocaml.Sam.Seq.t_option_of_string seq
    in
    let%bind qual = qual al in
    let%bind optional_fields = optional_fields al in
    Biocaml.Sam.Alignment.make
      ?qname
      ~flag
      ?rname
      ?pos
      ?mapq
      ~cigar
      ?rnext
      ?pnext
      ?tlen
      ?seq
      ~qual
      ~optional_fields
      ()
  ;;

  (* ============================ *)
  (* ==== ALIGNMENT ENCODING ==== *)
  (* ============================ *)

  (* Alignment.t -> Alignment0.t conversion *)
  let find_ref_id header ref_name =
    match
      Array.findi header.Header.ref_seq ~f:(fun _ rs -> String.(rs.name = ref_name))
    with
    | Some (i, _) -> Ok i
    | None -> Or_error.error_string "Bam: unknown reference id"
  ;;

  let string_of_cigar_ops cigar_ops =
    let buf = Bytes.create (List.length cigar_ops * 4) in
    let write ith i32 =
      let pos = ith * 4 in
      Binary_packing.pack_signed_32 ~byte_order:`Little_endian ~buf ~pos i32
    in
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
      write idx (Int32.bit_or 0l (Int32.of_int_exn Stdlib.(i lsl 4))));
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
    List.map opt_fields ~f:(fun opt_field ->
      let%bind c, s = field_value_encoding opt_field.Biocaml.Sam.Optional_field.value in
      Ok (sprintf "%s%c%s" opt_field.Biocaml.Sam.Optional_field.tag c s))
    |> Or_error.all
    >>| String.concat ~sep:""
  ;;

  let int32 i ~ub var =
    if i < ub
    then (
      match Int32.of_int i with
      | Some i -> Ok i
      | None -> Or_error.error_string "invalid conversion to int32")
    else Or_error.errorf "invalid conversion to int32 (%s than %d)" var ub
  ;;

  let encode_bin_mq_nl ~bin ~mapq ~l_read_name =
    let%bind bin = int32 bin ~ub:65536 "bin" in
    let%bind mapq = int32 mapq ~ub:256 "mapq" in
    let%bind l_read_name = int32 l_read_name ~ub:256 "l_read_name" in
    Ok
      (Int32.bit_or
         (Int32.shift_left bin 16)
         (Int32.bit_or (Int32.shift_left mapq 8) l_read_name))
  ;;

  let encode_flag_nc ~flags ~n_cigar_ops =
    let%bind flags = int32 flags ~ub:65536 "flags" in
    let%bind n_cigar_ops = int32 n_cigar_ops ~ub:65536 "n_cigar_ops" in
    Ok (Int32.bit_or (Int32.shift_left flags 16) n_cigar_ops)
  ;;

  let encode al header =
    let%bind ref_id =
      match al.Biocaml.Sam.Alignment.rname with
      | Some rname -> find_ref_id header (rname :> string)
      | None -> Ok (-1)
    in
    let read_name = Biocaml.Sam.Qname.to_string_option al.Biocaml.Sam.Alignment.qname in
    let seq = Option.value ~default:"*" (al.Biocaml.Sam.Alignment.seq :> string option) in
    let pos = Option.value ~default:0 (al.Biocaml.Sam.Alignment.pos :> int option) - 1 in
    let bin = reg2bin pos (pos + String.(length seq)) in
    let mapq = Option.value ~default:255 (al.Biocaml.Sam.Alignment.mapq :> int option) in
    let l_read_name = String.length read_name + 1 in
    (* NULL terminated string *)
    let%bind bin_mq_nl = encode_bin_mq_nl ~bin ~mapq ~l_read_name in
    let flags = (al.Biocaml.Sam.Alignment.flag :> int) in
    let n_cigar_ops = List.length al.Biocaml.Sam.Alignment.cigar in
    let%bind flag_nc = encode_flag_nc ~flags ~n_cigar_ops in
    let%bind next_ref_id =
      match al.Biocaml.Sam.Alignment.rnext with
      | Some `Equal_to_RNAME -> Ok ref_id
      | Some (`Value s) -> find_ref_id header (s :> string)
      | None -> Ok (-1)
    in
    let pnext =
      Option.value ~default:0 (al.Biocaml.Sam.Alignment.pnext :> int option) - 1
    in
    let tlen = Option.value ~default:0 (al.Biocaml.Sam.Alignment.tlen :> int option) in
    let cigar = string_of_cigar_ops al.Biocaml.Sam.Alignment.cigar in
    let%bind qual =
      Result.List.map
        al.Biocaml.Sam.Alignment.qual
        ~f:(Biocaml.Phred_score.to_char ~offset:`Offset33)
      >>| String.of_char_list
    in
    let%bind optional =
      string_of_optional_fields al.Biocaml.Sam.Alignment.optional_fields
    in
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
    let%bind () =
      check String.(magic = magic_string) "Incorrect magic string, not a BAM file"
    in
    let l_text = input_s32_as_int iz in
    let%bind () = check (l_text >= 0) "Incorrect size of plain text in BAM header" in
    let text = Bgzf.input_string iz l_text in
    Sam.parse_header text
  with
  | End_of_file -> Or_error.error_string "EOF while reading BAM header"
;;

let read_one_reference_information iz =
  try
    let l_name = input_s32_as_int iz in
    let%bind () =
      check (l_name > 0) "Incorrect encoding of reference sequence name in BAM header"
    in
    let name = Bgzf.input_string iz (l_name - 1) in
    let (_ : char) = Bgzf.input_char iz in
    (* name is a NULL terminated string *)
    let length = input_s32_as_int iz in
    Biocaml.Sam.Header.SQ.make ~name ~length ()
  with
  | End_of_file -> Or_error.error_string "EOF while reading BAM reference information"
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
  | End_of_file -> Or_error.error_string "EOF while reading BAM reference information"
;;

let read_alignment_aux iz block_size =
  try
    let ref_id = input_s32_as_int iz in
    let%bind pos =
      match Bgzf.input_s32 iz |> Int32.to_int with
      | Some 2147483647 (* POS in BAM is 0-based *) | None ->
        Or_error.error_string "A read has a position greater than 2^31"
      | Some n ->
        if n < -1 then Or_error.errorf "A read has a negative position %d" n else Ok n
    in
    let bin_mq_nl = Bgzf.input_s32 iz in
    let l_read_name = get_8_0 bin_mq_nl in
    let%bind () =
      checkf (l_read_name > 0) "Alignment with l_read_name = %d" l_read_name
    in
    let flag_nc = Bgzf.input_s32 iz in
    let n_cigar_ops = get_16_0 flag_nc in
    let l_seq = input_s32_as_int iz in
    let%bind () = check (l_seq >= 0) "Incorrect sequence length in alignment" in
    let next_ref_id = input_s32_as_int iz in
    let%bind pnext =
      match Bgzf.input_s32 iz |> Int32.to_int with
      | Some 2147483647 | None ->
        Or_error.error_string "A read has a position > than 2^31"
      | Some n ->
        if n < -1
        then Or_error.errorf "A read has a negative next position %d" n
        else Ok n
    in
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
    Ok
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
  | End_of_file -> Or_error.error_string "EOF while reading alignment"
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
  let%bind (sam_header : Biocaml.Sam.Header.t) = read_sam_header iz in
  let%bind ref_seq = read_reference_information iz in
  Ok { Header.sam_header; ref_seq }
;;

let read0 ic =
  let iz = Bgzf.of_in_channel ic in
  let%bind header = read_header iz in
  Ok (header, read_alignment_stream iz)
;;

let with_file0 fn ~f =
  In_channel.with_file ~binary:true fn ~f:(fun ic ->
    let%bind header, alignments = read0 ic in
    f header alignments)
;;

let write_plain_SAM_header (h : Biocaml.Sam.Header.t) oz =
  let buf = Buffer.create 1024 in
  let add_line x =
    Buffer.add_string buf x;
    Buffer.add_char buf '\n'
  in
  List.iter
    (h :> Biocaml.Sam.Header.Item.t list)
    ~f:(fun x -> add_line (Biocaml.Sam.Header.Item.to_line x));
  Bgzf.output_s32 oz (Int32.of_int_exn (Buffer.length buf));
  (* safe conversion of int32 to int: SAM headers less than a few KB *)
  Bgzf.output_string oz (Buffer.contents buf)
;;

let output_null_terminated_string oz s =
  Bgzf.output_string oz s;
  Bgzf.output_char oz '\000'
;;

let write_reference_sequences h oz =
  Bgzf.output_s32 oz (Int32.of_int_exn (Array.length h.Header.ref_seq));
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
  write_plain_SAM_header header.Header.sam_header oz;
  write_reference_sequences header oz
;;

let write_alignment oz al =
  Bgzf.output_s32 oz (Int32.of_int_exn (Alignment0.sizeof al));
  Bgzf.output_s32 oz (Int32.of_int_exn al.ref_id);
  Bgzf.output_s32 oz (Int32.of_int_exn al.pos);
  Bgzf.output_s32 oz al.bin_mq_nl;
  Bgzf.output_s32 oz al.flag_nc;
  Bgzf.output_s32 oz (Int32.of_int_exn (Alignment0.l_seq al));
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
  let%bind header, xs = read0 ic in
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
    assert_equal ~msg:"header" ~printer:[%sexp_of: Biocaml.Sam.Header.t] h1 h2;
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
    assert_equal
      ~msg:"rname"
      ~printer:[%sexp_of: string option Or_error.t]
      (Alignment0.rname al1 header)
      (Alignment0.rname al2 header);
    assert_equal
      ~msg:"mapq"
      ~printer:[%sexp_of: int option]
      (Alignment0.mapq al1)
      (Alignment0.mapq al2);
    assert_equal
      ~msg:"tlen"
      ~printer:[%sexp_of: int option]
      (Alignment0.tlen al1)
      (Alignment0.tlen al2);
    assert_equal
      ~msg:"read_name"
      ~printer:[%sexp_of: string option]
      (Alignment0.qname al1)
      (Alignment0.qname al2);
    assert_equal
      ~msg:"n_cigar_ops"
      ~printer:Int.sexp_of_t
      (List.length (ok_exn (Alignment0.cigar al1)))
      (List.length (ok_exn (Alignment0.cigar al2)));
    assert_equal
      ~msg:"seq"
      ~printer:String.sexp_of_t
      (Option.value_exn (Alignment0.seq al1))
      (Option.value_exn (Alignment0.seq al2));
    ()
  ;;

  let%expect_test "test_read" =
    with_file0 "../../etc/test_data/bam_01.bam" ~f:(fun header alignments ->
      let sh = Header.to_sam header in
      assert_equal
        ~msg:"Sam version"
        ~printer:[%sexp_of: string option]
        (Some "1.0")
        (sh |> Biocaml.Sam.Header.hd |> Option.map ~f:(fun x -> (x.version :> string)));
      assert_equal
        ~msg:"Sort order"
        (Some `Unsorted)
        (match sh |> Biocaml.Sam.Header.hd with
         | None -> None
         | Some hd -> hd.sort_order);
      assert_equal
        ~msg:"Number of ref sequences"
        ~printer:Int.sexp_of_t
        22
        (List.length (Biocaml.Sam.Header.ref_seqs sh));
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
    |}]
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
    |}]
  ;;
end
