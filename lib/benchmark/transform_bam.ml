open Core.Std
open Biocaml_unix
module Result = Biocaml_unix.Biocaml_result
open CFStream
module Sam = Transform_sam
module Zip = Biocaml_zip

type raw_alignment = {
  qname : string;
  flag : int;
  ref_id: int;
  (* rname : string; *)
  pos : int;
  mapq : int;
  bin: int;
  cigar : string;
  next_ref_id : int;
  pnext : int;
  tlen : int;
  seq : string;
  qual : int array;
  optional : string;
} [@@deriving sexp]

type raw_item =
[ `alignment of raw_alignment
| `header of string
| `reference_information of (string * int) array ]
[@@deriving sexp]

module Error = struct

  type parse_optional = [
  | `wrong_auxiliary_data of
      [ `array_size of int
      | `null_terminated_hexarray
      | `null_terminated_string
      | `wrong_int32 of string
      | `out_of_bounds
      | `unknown_type of char ] * string
  ]
  [@@deriving sexp]

  type raw_bam = [
  | `read_name_not_null_terminated of string
  | `reference_information_name_not_null_terminated of string
  | `reference_information_overflow of int * string
  | `wrong_magic_number of string
  | `wrong_int32 of string
  ]
  [@@deriving sexp]

  type parse_cigar = [
  | `wrong_cigar of string
  | `wrong_cigar_length of int ]
  [@@deriving sexp]

  type raw_to_item = [
  | `header_line_not_first of int
  | `header_line_without_version of (string * string) list
  | `header_line_wrong_sorting of string
  | `invalid_header_tag of int * string
  | `invalid_tag_value_list of int * string list
  | `reference_sequence_not_found of raw_alignment
  | parse_optional
  | parse_cigar
  | `wrong_flag of raw_alignment
  | `wrong_mapq of raw_alignment
  | `wrong_pnext of raw_alignment
  | `wrong_pos of raw_alignment
  | `wrong_qname of raw_alignment
  | `wrong_tlen of raw_alignment ]
  [@@deriving sexp]


  type item_to_raw =
  [ `cannot_get_sequence of Sam.alignment
  | `header_item_not_first of string
  | `reference_name_not_found of Sam.alignment * string ]
  [@@deriving sexp]

  type t = [ raw_bam | raw_to_item | item_to_raw ] [@@deriving sexp]

end

module Transform = struct
  open Result.Monad_infix

  let check b e = if b then Ok () else Error e

  let signed_int ~buf ~pos =
    let b1 = Char.to_int buf.[pos + 0] |> Int32.of_int_exn in
    let b2 = Char.to_int buf.[pos + 1] |> Int32.of_int_exn in
    let b3 = Char.to_int buf.[pos + 2] |> Int32.of_int_exn in
    let b4 = Char.to_int buf.[pos + 3] |> Int32.of_int_exn in
    let i32 =
      Int32.(bit_or  b1
               (bit_or (shift_left b2 8)
                  (bit_or (shift_left b3 16)
                     (shift_left b4 24)))) in
    try Ok (Int32.to_int_exn i32) with e -> Error (`wrong_int32 buf)

  let parse_header buf =
    check (String.length buf >= 12) `no
    >>= fun () ->
    check (String.sub buf 0 4 = "BAM\001")
      (`wrong_magic_number (String.sub buf 0 4))
    >>= fun () ->
    signed_int ~buf ~pos:4 >>= fun length ->
    check (String.length buf >= 4 + 4 + length + 4) `no
    >>= fun () ->
    let sam_header = String.sub buf 8 length in
    signed_int ~buf ~pos:(8 + length)
    >>= fun nb_refs ->
    Ok (`header sam_header, nb_refs, 4 + 4 + length + 4)

  let parse_reference_information_item buf pos =
    check (String.length buf - pos >= 4) `no >>= fun () ->
    signed_int ~buf ~pos
    >>= fun l_name ->
    check (String.length buf - pos >= 4 + l_name + 4) `no >>= fun () ->
    let name = String.sub buf (pos + 4) (l_name - 1) in
    check (buf.[pos + 4 + l_name - 1] = '\000')
      (`reference_information_name_not_null_terminated (String.sub buf 4 l_name))
    >>= fun () ->
    signed_int ~buf ~pos:(pos + 4 + l_name)
    >>= fun l_ref ->
    Ok (4 + l_name + 4, name, l_ref)

  let parse_reference_information buf nb =
    let bytes_read = ref 0 in
    let error = ref None in
    try
      let refinfo =
        (Array.init nb (fun _ ->
          match parse_reference_information_item buf !bytes_read with
          | Ok (read, name, lref) ->
            bytes_read := !bytes_read + read;
            (name, lref)
          | Error `no -> failwith "NO"
          | Error other -> error := Some other; failwith "ERROR")) in
      `reference_information (refinfo, !bytes_read)
    with
    | Failure "NO" -> `no
    | Failure "ERROR" -> `error Option.(value_exn !error)



  let parse_alignment buf =
    check (String.length buf >= 4 * 9) `no >>= fun () ->
    let uint16 pos =
      Binary_packing.unpack_unsigned_8 ~buf ~pos +
        Binary_packing.unpack_unsigned_8 ~buf ~pos:(pos + 1) lsl 7 in
    let uint8 pos = Binary_packing.unpack_unsigned_8 ~buf ~pos in
    signed_int ~buf ~pos:0 >>= fun block_size ->
    signed_int ~buf ~pos: 4
    >>= fun  ref_id ->
    signed_int ~buf ~pos: 8
    >>= fun  pos ->
  (* bin mq nl would be packed in a little-endian uint32, so we unpack
     its contents "in reverse order": *)
    let l_read_name = uint8 12 in
    let mapq = uint8 13 in
    let bin = uint16 14 in
  (* idem for flag_nc *)
    let n_cigar_op = uint16 16 in
    let flag = uint16 18 in
  (* back to "normal" *)
    signed_int ~buf ~pos: 20
    >>= fun  l_seq ->
    signed_int ~buf ~pos:24
    >>= fun  next_ref_id ->
    signed_int ~buf ~pos:28
    >>= fun  next_pos ->
    signed_int ~buf ~pos:32
    >>= fun  tlen ->
    check (String.length buf >= block_size + 4) `no
    (* (4 * 9) + l_read_name + (n_cigar_op * 4) + ((l_seq + 1) / 2) + l_seq) *)
    >>= fun () ->
    let qname = String.sub buf 36 (l_read_name - 1) in
    check (buf.[36 + l_read_name - 1] = '\000')
      (`read_name_not_null_terminated (String.sub buf 36 l_read_name))
    >>= fun () ->
    let cigar_buf = String.sub buf (36 + l_read_name) (n_cigar_op * 4) in
    let seq = String.make l_seq '*' in
    let letter  = function
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
      | l -> failwithf "letter not in [0, 15]: %d" l () in
    for i = 0 to ((l_seq + 1) / 2) - 1 do
      let byte = uint8 ((4 * 9) + l_read_name + (n_cigar_op * 4) + i) in
      seq.[2 * i] <- letter ((byte land 0xf0) lsr 4);
      if 2 * i + 1 < l_seq then
        seq.[2 * i + 1] <- letter (byte land 0x0f);
    done;
    let qual =
      Array.init l_seq (fun i ->
        Char.to_int
          buf.[(4 * 9) + l_read_name + (n_cigar_op * 4) + ((l_seq + 1) / 2) + i]
      ) in
    let aux_data =
      let offset =
        (4 * 9) + l_read_name + (n_cigar_op * 4) + ((l_seq + 1) / 2) + l_seq in
      String.sub buf offset (block_size + 4 - offset) in
    let alignment = {
      qname;

      flag;
      ref_id;
    (* rname = qname; *)
      pos;
      mapq;
      bin;
      cigar = cigar_buf ;
      next_ref_id;
      pnext = next_pos;
      tlen;
      seq;
      qual;
      optional = aux_data } in
    Ok (`alignment alignment, block_size + 4)

  let uncompressed_bam_parser () =
    let in_buffer = Buffer.create 42 in
    let state = ref `header in
    let next stopped =
      let buffered = Buffer.contents in_buffer in
      let len = String.length buffered in
      Buffer.clear in_buffer;
      begin match len with
      | 0 -> if stopped then `end_of_stream else `not_ready
      | _ ->
        begin match !state with
        | `header ->
          begin match parse_header buffered with
          | Ok (o, nbrefs, nbread) ->
            state := `reference_information nbrefs;
            Buffer.add_substring in_buffer buffered nbread (len - nbread);
            `output (Ok o)
          | Error `no ->
            Buffer.add_string in_buffer buffered; `not_ready
          | Error e -> `output (Error e)
          end
        | `reference_information nb ->
          begin match parse_reference_information buffered nb with
          | `no ->
            if len > 50000
            then `output (Error (`reference_information_overflow (len, buffered)))
            else begin
              Buffer.add_string in_buffer buffered;
              `not_ready
            end
          | `error  e -> `output (Error e)
          | `reference_information (refinfo, nbread) ->
            Buffer.add_substring in_buffer buffered nbread (len - nbread);
            state := `alignments refinfo;
            `output (Ok (`reference_information refinfo))
          end
        | `alignments refinfo ->
          begin match parse_alignment buffered with
          | Ok (o, nbread) ->
            Buffer.add_substring in_buffer buffered nbread (len - nbread);
            `output (Ok (o : raw_item))
          | Error `no ->
            Buffer.add_string in_buffer buffered; `not_ready
          | Error  e -> `output (Error e)
          end
        end
      end
    in
    Tfxm.make_result ()
      ~feed:(fun string -> Buffer.add_string in_buffer string;) ~next

  let string_to_raw ?zlib_buffer_size () =
    Tfxm.compose_results
      ~on_error:(function
      | `left l -> `unzip l
      | `right r ->
        match r with
        | `no -> failwith "got `right `no"
        | #Error.raw_bam as a -> `bam a)
      (Zip.Transform.unzip ~format:`gzip ?zlib_buffer_size ())
      (uncompressed_bam_parser ())

  let parse_optional ?(pos=0) ?len buf =
    let len =
      match len with Some s -> s | None -> String.length buf in
    let uint16 pos =
      Binary_packing.unpack_unsigned_8 ~buf ~pos +
        Binary_packing.unpack_unsigned_8 ~buf ~pos:(pos + 1) lsl 7 in
    let from () = String.sub buf pos len in
    let rec build ofs acc =
      let error e = Error (`wrong_auxiliary_data (e, from ())) in
      if ofs >= len then Ok acc
      else (
        if ofs + 2 >= len then error `out_of_bounds
        else (
          let tag = String.sub buf ofs 2 in
          let typ = buf.[ofs + 2] in
          let check_size_and_return n r =
            if ofs + 2 + n >= len then error `out_of_bounds
            else Ok (r, n) in
          let parse_cCsSiIf pos typ =
            begin match typ with
            | 'i' ->
              signed_int ~buf ~pos >>= fun v ->
              check_size_and_return 4 (`int v)
            | 'A' -> check_size_and_return 1 (`char buf.[pos])
            | 'c' | 'C' -> check_size_and_return 1 (`int (Char.to_int buf.[pos]))
            | 's' ->
              check_size_and_return 2 (`int (
                Binary_packing.unpack_signed_16
                  ~byte_order:`Little_endian ~buf ~pos))
            | 'S' -> check_size_and_return 2 (`int (uint16 pos))
            | 'f' ->
              let f =
                Binary_packing.unpack_signed_32
                  ~byte_order:`Little_endian ~buf ~pos |> Int32.float_of_bits in
              check_size_and_return 4 (`float f)
            | _ -> error (`unknown_type typ)
            end
          in
          let pos = ofs + 3 in
          begin match typ with
          | 'A' -> check_size_and_return 1 (`char buf.[pos])
          | 'Z' ->
            begin match String.index_from buf pos '\000' with
            | Some e -> Ok (`string String.(slice buf pos e), e - pos + 1)
            | None -> error `null_terminated_string
            end
          | 'H' ->
            begin match String.index_from buf pos '\000' with
            | Some e -> Ok (`string String.(slice buf pos e), e - pos + 1)
            | None -> error `null_terminated_hexarray
            end
          | 'B' ->
            check_size_and_return 1 buf.[pos] >>= fun (array_type, _) ->
            signed_int ~buf ~pos:(pos + 1) >>= fun i32 ->
            check_size_and_return 4 i32 >>= fun (size, _) ->
            (if size > 4000 then error (`array_size size) else Ok ())
            >>= fun () ->
            let arr = Array.create size (`char 'B') in
            let rec loop p = function
              | 0 -> Ok p
              | n ->
                parse_cCsSiIf p array_type
                >>= fun (v, nb) ->
                arr.(size - n) <- v;
                loop (p + nb) (n - 1) in
            loop (pos + 5) size
            >>= fun newpos ->
            Ok (`array (array_type, arr), newpos - pos)
          | c -> parse_cCsSiIf pos c
          end
              >>= fun (v, nbread) ->
          build (ofs + 3 + nbread) ((tag, typ, v) :: acc)
        )
      )
    in
    match build pos [] with
    | Ok r -> Ok (List.rev r)
    | Error (`wrong_auxiliary_data e) -> Error (`wrong_auxiliary_data e)
    | Error (`wrong_int32 e) -> Error (`wrong_auxiliary_data (`wrong_int32 e, from ()))


  let parse_cigar ?(pos=0) ?len buf =
    let len =
      match len with Some s -> s | None -> String.length buf in
    begin match len mod 4 with
    | 0 -> Ok (len / 4)
    | n -> Error (`wrong_cigar_length len)
    end
    >>= fun n_cigar_op ->
    begin
      try
        Ok (Array.init n_cigar_op (fun i ->
          let open Int64 in
          let int64 =
            let int8 pos =
              Binary_packing.unpack_unsigned_8 ~buf ~pos |> Int64.of_int in
            int8 Int.(pos + i * 4)
            + shift_left (int8 Int.(pos + i * 4 + 1)) 8
            + shift_left (int8 Int.(pos + i * 4 + 2)) 16
            + shift_left (int8 Int.(pos + i * 4 + 3)) 24
          in
          let op_len = shift_right int64 4 |> Int64.to_int_exn in
          let op =
            match bit_and int64 0x0fL with
            | 0L -> `M op_len
            | 1L -> `I op_len
            | 2L -> `D op_len
            | 3L -> `N op_len
            | 4L -> `S op_len
            | 5L -> `H op_len
            | 6L -> `P op_len
            | 7L -> `Eq op_len
            | 8L -> `X op_len
            | any -> failwithf "OP:%Ld" any () in
          op))
      with
      | e ->
        Error (`wrong_cigar
                 String.(sub buf pos (pos + n_cigar_op * 4)))
    end

  let parse_sam_header h =
    let lines = String.split ~on:'\n' h |> List.filter ~f:((<>) "") in
    Result.List.mapi lines (fun idx line ->
      Sam.parse_header_line idx line
      >>= fun raw_sam ->
      begin match raw_sam with
      | `comment s -> Ok (`comment s)
      | `header ("HD", l) ->
        if idx <> 0
        then Error (`header_line_not_first idx)
        else Sam.expand_header_line l
      | `header h -> Ok (`header h)
      end)

  let expand_alignment refinfo raw =
    let {
      qname (* : string *); flag (* : int *); ref_id;
      pos (* : int *); mapq (* : int *); bin (* : int *); cigar (* : string *);
      next_ref_id (* : int *); pnext (* : int *); tlen (* : int *);
      seq (* : string *); qual (* : int array *); optional (* : string *);} = raw in
    let check c e = if c then Ok () else Error e in
    check (1 <= String.length qname && String.length qname <= 255)
      (`wrong_qname raw)
    >>= fun () ->
    check (0 <= flag && flag <= 65535) (`wrong_flag raw) >>= fun () ->
    let find_ref id =
      begin match id with
      | -1 -> Ok `none
      | other ->
        begin try Ok (`reference_sequence refinfo.(other))
          with e -> Error (`reference_sequence_not_found raw) end
      end in
    find_ref ref_id >>= fun reference_sequence ->
    check (-1 <= pos && pos <= 536870910) (`wrong_pos raw) >>= fun () ->
    check (0 <= mapq && mapq <= 255) (`wrong_mapq raw) >>= fun () ->
    parse_cigar cigar >>= fun cigar_operations ->
    find_ref next_ref_id >>= fun next_reference_sequence ->
    check (-1 <= pnext && pnext <= 536870910) (`wrong_pnext raw) >>= fun () ->
    check (-536870911 <= tlen && tlen <= 536870911) (`wrong_tlen raw)
    >>= fun () ->
    parse_optional optional >>= fun optional_content ->
    Ok (`alignment {
      Sam.
      query_template_name = qname;
      flags = Sam.Flags.of_int flag;
      reference_sequence;
      position = if pos = -1 then None else Some (pos + 1);
      mapping_quality =if mapq = 255 then None else Some mapq;
      cigar_operations;
      next_reference_sequence = next_reference_sequence;
      next_position = if pnext = -1 then None else Some (pnext + 1);
      template_length  = if tlen = 0 then None else Some tlen;
      sequence = `string seq;
      quality = Array.map qual ~f:(fun x -> ok_exn (Phred_score.of_int x));
      optional_content;
    })


  let raw_to_item () :
      (raw_item, (Sam.item, _) Result.t) Tfxm.t=
    let name = "bam_item_parser" in
    let raw_queue = Dequeue.create () in
    let raw_items_count = ref 0 in
    let header_items = ref [] in
    let reference_information = ref [| |] in
    let first_alignment = ref true in
    let rec next stopped =
      begin match !header_items with
      | h :: t -> header_items := t; `output (Ok h)
      | [] ->
        begin match Dequeue.is_empty raw_queue, stopped with
        | true, true ->`end_of_stream
        | true, false -> `not_ready
        | false, _ ->
          incr raw_items_count;
          begin match Dequeue.dequeue_exn raw_queue `front with
          | `header s ->
            begin match parse_sam_header s with
            | Ok h -> header_items := h; next stopped
            | Error e -> `output (Error e)
            end
          | `reference_information ri ->
            let make_ref_info (s, i) = Sam.reference_sequence s i in
            reference_information := Array.map ri ~f:make_ref_info;
            next stopped
          | `alignment a ->
            if !first_alignment then (
              first_alignment := false;
              Dequeue.enqueue raw_queue `front (`alignment a);
              `output (Ok (`reference_sequence_dictionary !reference_information))
            ) else (
              expand_alignment !reference_information a |> (fun x -> `output x)
            )
          end
        end
      end
    in
    Tfxm.make ~name ~feed:(Dequeue.enqueue raw_queue `back) ()
      ~next

  let downgrade_alignement al ref_dict =
    let module S = Sam in
    let find_ref s =
      begin match Array.findi ref_dict (fun _ n -> n.S.ref_name = s) with
      | Some (i, _) -> Ok i
      | None -> Error (`reference_name_not_found (al, s))
      end
    in

    let qname = al.S.query_template_name in
    let flag = (al.S.flags :> int) in
    begin match al.S.reference_sequence with
    | `name s -> find_ref s
    | `none -> Ok (-1)
    | `reference_sequence rs -> find_ref rs.S.ref_name
    end
    >>= fun ref_id ->
    let pos = (Option.value ~default:0 al.S.position) - 1 in
    let mapq = Option.value ~default:255 al.S.mapping_quality in
    begin match al.S.sequence with
    | `string s -> Ok s
    | `none -> Ok ""
    | `reference -> Error (`cannot_get_sequence al)
    end
    >>= fun seq ->
    let bin =
      let beg = pos in
      let end_close (* open interval but then '--pos;' *) =
        pos + String.(length seq) in
      match beg, end_close with
      | b,e when b lsr 14 = e lsr 14 ->
        ((1 lsl 15) - 1) / 7  +  (beg lsr 14)
      | b,e when b lsr 17 = e lsr 17 ->
        ((1 lsl 12) - 1) / 7  +  (beg lsr 17)
      | b,e when b lsr 20 = e lsr 20 ->
        ((1 lsl 9) - 1) / 7  +  (beg lsr 20)
      | b,e when b lsr 23 = e lsr 23 ->
        ((1 lsl 6) - 1) / 7  +  (beg lsr 23)
      | b,e when b lsr 26 = e lsr 26 ->
        ((1 lsl 3) - 1) / 7  +  (beg lsr 26)
      | _ -> 0 in
    let cigar =
      let buf = String.create (Array.length al.S.cigar_operations * 4) in
      let write ith i32 =
        let pos = ith * 4 in
        Binary_packing.pack_signed_32 ~byte_order:`Little_endian ~buf ~pos i32 in
      let open Int32 in
      Array.iteri al.S.cigar_operations ~f:(fun idx -> function
      | `M  i -> bit_or 0l (of_int_exn (i lsl 4)) |> write idx
      | `I  i -> bit_or 1l (of_int_exn (i lsl 4)) |> write idx
      | `D  i -> bit_or 2l (of_int_exn (i lsl 4)) |> write idx
      | `N  i -> bit_or 3l (of_int_exn (i lsl 4)) |> write idx
      | `S  i -> bit_or 4l (of_int_exn (i lsl 4)) |> write idx
      | `H  i -> bit_or 5l (of_int_exn (i lsl 4)) |> write idx
      | `P  i -> bit_or 6l (of_int_exn (i lsl 4)) |> write idx
      | `Eq i -> bit_or 7l (of_int_exn (i lsl 4)) |> write idx
      | `X  i -> bit_or 8l (of_int_exn (i lsl 4)) |> write idx);
      buf
    in
    begin match al.S.next_reference_sequence with
    | `qname -> find_ref qname
    | `none -> Ok (-1)
    | `name s -> find_ref s
    | `reference_sequence rs -> find_ref rs.S.ref_name
    end
    >>= fun next_ref_id ->
    let pnext = Option.value ~default:0 al.S.next_position - 1 in
    let tlen = Option.value ~default:0 al.S.template_length in
    let qual = Array.map al.S.quality ~f:Phred_score.to_int in
    let optional =
      let rec content typ = function
        | `array (t, v) ->
          sprintf "%c%s" t (Array.map ~f:(content t) v |> String.concat_array ~sep:"")
        | `char c -> Char.to_string c
        | `float f ->
          let bits = Int32.bits_of_float f in
          let buf = String.create 4 in
          Binary_packing.pack_signed_32
            ~byte_order:`Little_endian bits ~buf ~pos:0;
          buf
        | `int i ->
          begin match typ with
          | 'c' | 'C' ->
            let buf = String.create 1 in
            Binary_packing.pack_unsigned_8 (0xff land i) ~buf ~pos:0;
            buf
          | 's' | 'S' ->
            let buf = String.create 2 in
            Binary_packing.pack_signed_16 (0xffff land i)
              ~byte_order:`Little_endian ~buf ~pos:0;
            buf
          | _ ->
            let buf = String.create 4 in
            Binary_packing.pack_signed_32_int i
              ~byte_order:`Little_endian ~buf ~pos:0;
            buf
          end
        | `string s ->
          begin match typ with
          | 'H' ->
            let r = ref [] in
            String.iter s (fun c ->
              r := sprintf "%02x" (Char.to_int c) :: !r
            );
            String.concat ~sep:"" (List.rev !r) ^ "\000"
          | _ -> s ^ "\000"
          end
      in
      List.map al.S.optional_content (fun (tag, typ, c) ->
        sprintf "%s%c%s" tag typ (content typ c))
      |> String.concat ~sep:""
    in
    Ok {
      qname; flag; ref_id; pos; mapq; bin; cigar;
      next_ref_id; pnext; tlen; seq; qual; optional;}

  let item_to_raw () :
      (Sam.item, (raw_item, _) Result.t) Tfxm.t =
    let name = "bam_item_downgrader" in
    let queue = Dequeue.create () in
    let items_count = ref 0 in
    let ref_dict = ref [| |] in
    let ref_dict_done = ref false in
    let header = Buffer.create 256 in
    let rec next stopped =
      begin match Dequeue.is_empty queue, stopped with
      | true, true ->`end_of_stream
      | true, false -> `not_ready
      | false, _ ->
        incr items_count;
        begin match Dequeue.dequeue_exn queue `front with
        | `comment c ->
          Buffer.add_string header "@CO\t";
          Buffer.add_string header c;
          Buffer.add_string header "\n";
          next stopped
        | `header_line (version, ordering, rest) ->
          if Buffer.contents header <> "" then
            `output (Error (`header_item_not_first (Buffer.contents header)))
          else begin
            ksprintf (Buffer.add_string header) "@HD\tVN:%s\tSO:%s%s\n"
              version
              (match ordering with
              | `unknown -> "unknown"
              | `unsorted -> "unsorted"
              | `queryname -> "queryname"
              | `coordinate -> "coordinate")
              (List.map rest (fun (t, v) -> sprintf "\t%s:%s" t v)
               |> String.concat ~sep:"");
            next stopped
          end
        | `header (pretag, l) ->
          ksprintf (Buffer.add_string header) "@%s" pretag;
          List.iter l (fun (t, v) ->
            ksprintf (Buffer.add_string header) "\t%s:%s" t v;
          );
          Buffer.add_string header "\n";
          next stopped
        | `reference_sequence_dictionary r ->
          ref_dict := r;
          `output (Ok (`header (Buffer.contents header)))
        | `alignment al ->
          if not !ref_dict_done
          then begin
            ref_dict_done := true;
            Dequeue.enqueue queue `front (`alignment al);
            `output (Ok (`reference_information (Array.map !ref_dict ~f:(fun rs ->
              let open Sam in
              (rs.ref_name, rs.ref_length)))))
          end
          else begin
            match downgrade_alignement al !ref_dict with
            | Ok o -> `output (Ok (`alignment o))
            | Error e -> `output (Error e)
          end
        end
      end
    in
    Tfxm.make ~name ~feed:(Dequeue.enqueue queue `back) ()
      ~next

  let uncompressed_bam_printer () : (raw_item, string) Tfxm.t =
    let name = "uncompressed_bam_printer" in
    let buffer = Buffer.create 4096  in
    let write buffer s = Buffer.add_string buffer s in
    let write_little_endian_int n buffer i =
      let r = ref i in
      for i = 1 to n do
        Buffer.add_char buffer (Char.of_int_exn (0xff land !r));
        r := !r lsr 8
      done in
    let write_32_int = write_little_endian_int 4 in
    let feed = function

      | `alignment ra ->

        let l_read_name = String.length ra.qname + 1 in
        let n_cigar_op = (String.length ra.cigar / 4) in
        let l_seq = (String.length ra.seq) in
        let size =
          (4 * 8) + l_read_name + (n_cigar_op * 4) + ((l_seq + 1) / 2) + l_seq
          + (String.length ra.optional) in

        write_32_int buffer size;
        write_32_int buffer ra.ref_id;
        write_32_int buffer ra.pos;
        write_little_endian_int 1 buffer l_read_name;
        write_little_endian_int 1 buffer ra.mapq;
        write_little_endian_int 2 buffer ra.bin ;
        write_little_endian_int 2 buffer n_cigar_op;
        write_little_endian_int 2 buffer ra.flag;
        write_32_int buffer l_seq;
        write_32_int buffer ra.next_ref_id;
        write_32_int buffer ra.pnext;
        write_32_int buffer ra.tlen;
        write buffer ra.qname; write_little_endian_int 1 buffer 0;
        write buffer ra.cigar;
        let base4 = function
          | '=' -> 0
          | 'A' -> 1
          | 'C' -> 2
          | 'M' -> 3
          | 'G' -> 4
          | 'R' -> 5
          | 'S' -> 6
          | 'V' -> 7
          | 'T' -> 8
          | 'W' -> 9
          | 'Y' -> 10
          | 'H' -> 11
          | 'K' -> 12
          | 'D' -> 13
          | 'B' -> 14
          | 'N' | _ -> 15 in
        for i = 0 to ((String.length ra.seq + 1) / 2) - 1 do
          write_little_endian_int 1 buffer
            (base4 ra.seq.[2 * i] lsl 4 +
               (try base4 ra.seq.[2 * i + 1] with _ -> 0))
        done;
        for i = 0 to String.length ra.seq - 1 do
        (* the spec says that if qual is absent, the qualities must be
           0xff anyway *)
          write_little_endian_int 1 buffer
            (try ra.qual.(i) with _ -> 0xff)
        done;
        write buffer ra.optional;

      | `header h ->
        write buffer "BAM\x01";
        write_32_int buffer (String.length h);
        write buffer h
      | `reference_information sia ->
        write_32_int buffer (Array.length sia);
        Array.iter sia ~f:(fun (name, lgth) ->
          write_32_int buffer (String.length name + 1);
          write buffer name;
          write_little_endian_int 1 buffer 0;
          write_32_int buffer lgth);
    in
    let rec next stopped =
      match Buffer.contents buffer with
      | "" -> if stopped then `end_of_stream else `not_ready
      | s ->
        Buffer.clear buffer;
        `output s in
    Tfxm.make ~name ~feed ~next ()

  let raw_to_string ?gzip_level ?zlib_buffer_size () =
    Tfxm.compose
      (uncompressed_bam_printer ())
      (Zip.Transform.zip ~format:`gzip ?level:gzip_level ?zlib_buffer_size ())
end

open Transform
let parse_optional = Transform.parse_optional
let parse_cigar = Transform.parse_cigar


let in_channel_to_raw_item_stream ?zlib_buffer_size ?buffer_size inp =
  let t = Transform.string_to_raw ?zlib_buffer_size () in
  Tfxm.in_channel_strings_to_stream ?buffer_size inp t

let in_channel_to_item_stream ?zlib_buffer_size ?buffer_size inp =
  let t1 = Transform.string_to_raw ?zlib_buffer_size () in
  let t2 = Transform.raw_to_item  () in
  Tfxm.(
    in_channel_strings_to_stream ?buffer_size inp
      (compose_results t1 t2 ~on_error:(function `left x -> x | `right x -> `bam x))
  )

exception Error of [ `bam of Error.t | `unzip of Zip.Error.unzip ]
let error_to_exn e = Error e

let in_channel_to_raw_item_stream_exn ?zlib_buffer_size ?buffer_size inp =
  Stream.result_to_exn ~error_to_exn
    (in_channel_to_raw_item_stream ?zlib_buffer_size ?buffer_size inp)
let in_channel_to_item_stream_exn ?zlib_buffer_size ?buffer_size inp =
  Stream.result_to_exn ~error_to_exn
    (in_channel_to_item_stream ?zlib_buffer_size ?buffer_size inp)
