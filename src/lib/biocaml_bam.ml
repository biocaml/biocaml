open Biocaml_internal_pervasives

type optional_content = [
| `array of optional_content array
| `char of char
| `float of float
| `int of int
| `string of string ]

type alignment = {
  qname : string;
  flag : int;
  (* rname : string; *)
  pos : int;
  mapq : int;
  cigar : (char * int) array;
  next_ref_id : int;
  pnext : int;
  tlen : int;
  seq : string;
  qual : int array;
  optional : (string * char * optional_content) list
}

type stream_item =
[ `alignment of alignment
| `header of string
| `reference_information of (string * int) array ]
  
type bam_parse_error = [
| `read_name_not_null_terminated of string
| `reference_information_name_not_null_terminated of string
| `wrong_auxiliary_data of
    [ `array_size of int
    | `null_terminated_hexarray
    | `null_terminated_string
    | `out_of_bounds
    | `unknown_type of char ] * string
| `wrong_cigar of string
| `wrong_magic_number of string
]
open Result

let string_of_bam_parse_error e =
  match e with
  | `read_name_not_null_terminated s ->
    sprintf "read_name_not_null_terminated %s" s
  | `reference_information_name_not_null_terminated s ->
    sprintf "reference_information_name_not_null_terminated %s" s
  | `wrong_auxiliary_data (wad, s) ->
    sprintf "wrong_auxiliary_data (%s, %S)" s
      (match wad with
      | `array_size d -> sprintf "array_size %d" d
      | `null_terminated_hexarray -> "null_terminated_hexarray"
      | `null_terminated_string -> "null_terminated_string"
      | `out_of_bounds -> "out_of_bounds"
      | `unknown_type c -> sprintf "unknown_type '%c'" c)
  | `wrong_cigar s -> sprintf "wrong_cigar %s" s
  | `wrong_magic_number s -> sprintf "wrong_magic_number %s" s

let debug = ref true
let dbg fmt =
  ksprintf (fun s ->
    if !debug then (
      eprintf "BAM: ";
      String.iter s (function
      | '\n' -> eprintf "\n    "
      | c -> eprintf "%c" c);
      eprintf "\n%!"
    )
  ) fmt

let check b e = if b then return () else fail e
    
let parse_header buf =
  check (String.length buf >= 12) `no
  >>= fun () ->
  check (String.sub buf 0 4 = "BAM\001")
    (`wrong_magic_number (String.sub buf 0 4))
  >>= fun () ->
  let length =
    Binary_packing.unpack_signed_32_int ~byte_order:`Little_endian ~buf ~pos:4 in
  dbg "header length: %d" length;
  check (String.length buf >= 4 + 4 + length + 4) `no
  >>= fun () ->
  let sam_header = String.sub buf 8 length in
  dbg "sam header: %S" sam_header;
  let nb_refs =
    let pos = 8 + length in
    Binary_packing.unpack_signed_32_int ~byte_order:`Little_endian ~buf ~pos in
  dbg "nb refs: %d" nb_refs;
  return (`header sam_header, nb_refs, 4 + 4 + length + 4)

let parse_reference_information_item buf pos =
  check (String.length buf - pos >= 4) `no >>= fun () ->
  let l_name =
    Binary_packing.unpack_signed_32_int ~byte_order:`Little_endian ~buf ~pos in
  dbg "l_name: %d" l_name;
  check (String.length buf - pos >= 4 + l_name + 4) `no >>= fun () ->
  let name = String.sub buf (pos + 4) (l_name - 1) in
  dbg "name: %S" name;
  check (buf.[pos + 4 + l_name - 1] = '\000')
    (`reference_information_name_not_null_terminated (String.sub buf 4 l_name))
  >>= fun () ->
  let l_ref =
    let pos = pos + 4 + l_name in
    Binary_packing.unpack_signed_32_int ~byte_order:`Little_endian ~buf ~pos in
  return (4 + l_name + 4, name, l_ref)

let parse_reference_information buf nb =
  let bytes_read = ref 0 in
  let error = ref None in
  try
    let refinfo =
      (Array.init nb (fun _ ->
        match parse_reference_information_item buf !bytes_read with
        | Ok (read, name, lref) ->
          bytes_read := !bytes_read + read;
          dbg "parse_reference_information_item: %d %s %d" read name lref;
          (name, lref)
        | Error `no -> failwith "NO"
        | Error other -> error := Some other; failwith "ERROR")) in
    `reference_information (refinfo, !bytes_read)
  with
  | Failure "NO" -> `no
  | Failure "ERROR" -> `error Option.(value_exn !error)
  

  
let parse_alignment buf =
  check (String.length buf >= 4 * 9) `no >>= fun () ->
  let int32 pos = 
    Binary_packing.unpack_signed_32_int ~byte_order:`Little_endian ~buf ~pos in
  let uint16 pos = 
    Binary_packing.unpack_unsigned_8 ~buf ~pos +
      Binary_packing.unpack_unsigned_8 ~buf ~pos:(pos + 1) lsl 7 in
  let uint8 pos = Binary_packing.unpack_unsigned_8 ~buf ~pos in
  let block_size = int32 0 in
  let ref_id = int32 4 in
  let pos = int32 8 in
  (* bin mq nl would be packed in a little-endian uint32, so we unpack
     its contents "in reverse order": *)
  let l_read_name = uint8 12 in
  let mapq = uint8 13 in
  let bin = uint16 14 in
  (* idem for flag_nc *)
  let n_cigar_op = uint16 16 in
  let flag = uint16 18 in
  (* back to "normal" *)
  let l_seq = int32 20 in
  let next_ref_id = int32 24 in
  let next_pos = int32 28 in
  let tlen = int32 32 in
  dbg " block_size: %d ref_id: %d pos: %d l_read_name: %d mapq: %d
  bin: %d n_cigar_op: %d flag: %d l_seq: %d next_ref_id: %d next_pos: %d tlen: %d"
    block_size ref_id pos l_read_name mapq bin n_cigar_op flag l_seq next_ref_id
    next_pos tlen;

  check (String.length buf >= block_size + 4) `no
    (* (4 * 9) + l_read_name + (n_cigar_op * 4) + ((l_seq + 1) / 2) + l_seq) *)
  >>= fun () ->
  let qname = String.sub buf 36 (l_read_name - 1) in
  check (buf.[36 + l_read_name - 1] = '\000')
    (`read_name_not_null_terminated (String.sub buf 36 l_read_name))
  >>= fun () ->
  (* dbg "qname: %S" qname; *)
  begin try
    return (Array.init n_cigar_op (fun i ->
      let open Int64 in
      let int64 =
        let int8 pos = 
          Binary_packing.unpack_unsigned_8 ~buf ~pos |! Int64.of_int in
        int8 Int.(36 + l_read_name + i * 4)
        + shift_left (int8 Int.(36 + l_read_name + i * 4 + 1)) 8
        + shift_left (int8 Int.(36 + l_read_name + i * 4 + 2)) 16
        + shift_left (int8 Int.(36 + l_read_name + i * 4 + 3)) 24
      in
      let op =
        match bit_and int64 0x0fL with
        | 0L -> 'M'
        | 1L -> 'I'
        | 2L -> 'D'
        | 3L -> 'N'
        | 4L -> 'S'
        | 5L -> 'H'
        | 6L -> 'P'
        | 7L -> '='
        | 8L -> 'X'
        | any -> failwithf "OP:%Ld" any () in
      let op_len = shift_right int64 4 |! Int64.to_int_exn in 
      (op, op_len)))
    with
    | e ->
      fail (`wrong_cigar
               String.(sub buf (36 + l_read_name)
                         (36 + l_read_name + n_cigar_op * 4)))
  end
  >>= fun cigarray ->
  (* dbg "cigar: %s" (Array.to_list cigarray *)
                   (* |! List.map ~f:(fun (op, len) -> sprintf "%c:%d" op len) *)
                   (* |! String.concat ~sep:"; "); *)
  
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
    (* dbg "i: %d" i; *)
    let byte = uint8 ((4 * 9) + l_read_name + (n_cigar_op * 4) + i) in
    (* dbg "byte: %d" byte; *)
    seq.[2 * i] <- letter ((byte land 0xf0) lsr 4);
    if 2 * i + 1 < l_seq then
      seq.[2 * i + 1] <- letter (byte land 0x0f);
  done;
  (* dbg "seq: %S" seq; *)
  let qual =
    Array.init l_seq (fun i ->
      Char.to_int
        buf.[(4 * 9) + l_read_name + (n_cigar_op * 4) + ((l_seq + 1) / 2) + i]
    ) in
  let aux_data =
    let ofset =
      (4 * 9) + l_read_name + (n_cigar_op * 4) + ((l_seq + 1) / 2) + l_seq in
    let from () =
      String.sub buf ofset (block_size + 4 - ofset) in
    dbg "from: %S" (from ());
    let rec build ofs acc =
      let error e = fail (`wrong_auxiliary_data (e, from ())) in
      if ofs >= block_size + 4 then return acc
      else (
        if ofs + 2 >= block_size + 4 then error `out_of_bounds
        else (
          let tag = String.sub buf ofs 2 in
          let typ = buf.[ofs + 2] in
          let check_size_and_return n r =
            if ofs + 2 + n >= block_size + 4 then error `out_of_bounds
            else return (r, n) in
          let parse_cCsSiIf pos typ =
            begin match typ with
            | 'i' -> check_size_and_return 4 (`int (int32 pos))
            | 'A' | 'c' | 'C' -> check_size_and_return 1 (`char buf.[pos])
            | 's' ->
              check_size_and_return 2 (`int (
                Binary_packing.unpack_signed_16 
                  ~byte_order:`Little_endian ~buf ~pos))
            | 'S' -> check_size_and_return 2 (`int (uint16 pos))
            | 'f' ->
              let f =
                Binary_packing.unpack_signed_32
                  ~byte_order:`Little_endian ~buf ~pos |! Int32.float_of_bits in
              check_size_and_return 4 (`float f)
            | _ -> error (`unknown_type typ)
            end
          in
          let pos = ofs + 3 in
          begin match typ with
          | 'A' -> check_size_and_return 1 (`char buf.[pos])
          | 'Z' ->
            begin match String.index_from buf pos '\000' with
            | Some e -> return (`string String.(slice buf pos e), e - pos + 1)
            | None -> error `null_terminated_string
            end
          | 'H' ->
            begin match String.index_from buf pos '\000' with
            | Some e -> return (`string String.(slice buf pos e), e - pos + 1)
            | None -> error `null_terminated_hexarray
            end
          | 'B' ->
            check_size_and_return 1 buf.[pos] >>= fun (array_type, _) ->
            check_size_and_return 4 (int32 (pos + 1)) >>= fun (size, _) ->
            (if size > 4000 then error (`array_size size) else return ())
            >>= fun () ->
            let arr = Array.create size (`char 'B') in
            let rec loop p = function
              | 0 -> return p
              | n -> 
                parse_cCsSiIf p array_type
                >>= fun (v, nb) ->
                arr.(size - n) <- v;
                loop (p + nb) (n - 1) in
            loop (pos + 5) size
            >>= fun newpos ->
            return (`array arr, newpos - pos)
          | c -> parse_cCsSiIf pos c
          end
          >>= fun (v, nbread) ->
          build (ofs + 3 + nbread) ((tag, typ, v) :: acc)
        )
      )
    in
    build ofset []
    >>= fun result ->
    return (List.rev result)
  in
  aux_data
  >>= fun optional ->
  let alignment = {
    qname;

    flag;
    (* rname = qname; *)
    pos;
    mapq;
    cigar = cigarray ;
    next_ref_id;
    pnext = next_pos;
    tlen;
    seq;
    qual;
    optional } in
  return (`alignment alignment, block_size + 4)
    
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
          `output o
        | Error `no ->
          dbg "rebuffering %d bytes" String.(length buffered);
          Buffer.add_string in_buffer buffered; `not_ready
        | Error e -> `error e
        end
      | `reference_information nb ->
        begin match parse_reference_information buffered nb with
        | `no ->
          dbg "(ri) rebuffering %d bytes" String.(length buffered);
          if len > 10000 then failwith "too much";
          Buffer.add_string in_buffer buffered; `not_ready
        | `error  e -> `error e
        | `reference_information (refinfo, nbread) ->
          Buffer.add_substring in_buffer buffered nbread (len - nbread);
          state := `alignements refinfo;
          `output (`reference_information refinfo)
        end
      | `alignements refinfo ->
        begin match parse_alignment buffered with
        | Ok (o, nbread) ->
          dbg "len: %d nbread: %d" len nbread;
          Buffer.add_substring in_buffer buffered nbread (len - nbread);
          `output (o : stream_item)
        | Error `no ->
          dbg "(al) rebuffering %d bytes" String.(length buffered);
          Buffer.add_string in_buffer buffered; `not_ready
        | Error  e -> `error e
        end
      end
    end
  in
  Biocaml_transform.make_stoppable ()
    ~feed:(fun string -> Buffer.add_string in_buffer string;) ~next

let parser ?zlib_buffer_size () =
  Biocaml_transform.(
    on_error
      ~f:(function
      | `left l -> `unzip l
      | `right r ->
        match r with
        | `no -> failwith "got `right `no"
        | #bam_parse_error as a -> `bam a)
      (compose
       (Biocaml_zip.unzip ~format:`gzip ?zlib_buffer_size ())
       (uncompressed_bam_parser ())))
