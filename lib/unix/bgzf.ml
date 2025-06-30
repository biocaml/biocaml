(***********************************************************************)
(*                                                                     *)
(*                         The CamlZip library                         *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(* Various parts of this module, as well as its global structure are
   adapted from the gzip module in the camlzip library. *)

let max_block_size = 0x10000
let max_isize = 0xff00

(* Justification for the above constants *)
(* let compressBound = *)
(*   let open Ctypes in *)
(*   let open Foreign in *)
(*   foreign "compressBound" (int @-> returning int) *)

(* # compressBound 0xff00;; *)
(* - : int = 65311 *)
(* The size of the header is 16, footer is 8, so data + header + footer < 0x10000 *)

exception Error of string

type in_channel =
  { ic : Stdlib.in_channel (* Underlying channel *)
  ; in_bufz : bytes (* Compressed block *)
  ; in_buf : bytes (* Uncompressed block *)
  ; mutable in_block_offset : Int64.t (* Offset of the current block *)
  ; mutable in_pos : int (* Position in the current block *)
  ; mutable in_avail : int
      (* Number of available characters in the current block, can be less than [max_block_size] *)
  ; mutable in_eof : bool (* Flag indicating we reached the end of the file *)
  ; mutable in_stream : Zlib.stream
  }

let of_in_channel ic =
  { ic
  ; in_bufz = Bytes.make max_block_size '\000'
  ; in_buf = Bytes.make max_block_size '\000'
  ; in_block_offset = Int64.zero
  ; in_pos = 0
  ; in_avail = 0
  ; in_stream = Zlib.inflate_init false
  ; in_eof = false
  }
;;

let open_in fn = of_in_channel (Stdlib.open_in_bin fn)

let dispose_in iz =
  iz.in_eof <- true;
  Zlib.inflate_end iz.in_stream
;;

let close_in iz =
  dispose_in iz;
  In_channel.close iz.ic
;;

let input_byte t = Caml.input_byte t

let input_u16 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  b1 + (b2 lsl 8)
;;

let input_s32 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  let open Int32 in
  bit_or
    (of_int_exn b1)
    (bit_or
       (shift_left (of_int_exn b2) 8)
       (bit_or (shift_left (of_int_exn b3) 16) (shift_left (of_int_exn b4) 24)))
;;

(* Raises End_of_file iff there is no more block to read *)
let read_header iz =
  match In_channel.input_byte iz.ic with
  | None ->
    iz.in_eof <- true;
    raise End_of_file
  | Some id1 -> (
    try
      let id2 = input_byte iz.ic in
      if id1 <> 0x1F || id2 <> 0x8B then raise (Error "bad magic number, not a bgzf file");
      let cm = input_byte iz.ic in
      if cm <> 8 then raise (Error "unknown compression method");
      let flags = input_byte iz.ic in
      if flags <> 0x04 then raise (Error "bad flags, not a bgzf file");
      for _ = 1 to 6 do
        ignore (input_byte iz.ic : int)
      done;
      let xlen = input_u16 iz.ic in
      let si1 = input_byte iz.ic in
      let si2 = input_byte iz.ic in
      let slen = input_u16 iz.ic in
      if si1 <> 66 || si2 <> 67 || slen <> 2 then raise (Error "bad extra subfield");
      let bsize = input_u16 iz.ic in
      for _ = 1 to xlen - 6 do
        ignore (input_byte iz.ic : int)
      done;
      bsize - xlen - 19
    with
    | End_of_file -> raise (Error "premature end of file, not a bgzf file"))
;;

let read_block iz =
  let rec loop posz lenz pos len crc size =
    let finished, used_in, used_out =
      try
        Zlib.inflate iz.in_stream iz.in_bufz posz lenz iz.in_buf pos len Zlib.Z_SYNC_FLUSH
      with
      | Zlib.Error (_, _) -> raise (Error "error during decompression")
    in
    let posz = posz + used_in in
    let lenz = lenz - used_in in
    let crc = Zlib.update_crc crc iz.in_buf pos used_out in
    let size = size + used_out in
    if finished
    then crc, size
    else loop posz lenz (pos + used_out) (len - used_out) crc size
  in
  try
    iz.in_block_offset <- In_channel.pos iz.ic;
    let cdata_size = read_header iz in
    (* read_header raises End_of_file iff there is no more block to read *)
    try
      Stdlib.really_input iz.ic iz.in_bufz 0 cdata_size;
      let ref_crc = input_s32 iz.ic in
      let ref_size = input_s32 iz.ic |> Int32.to_int_exn in
      Zlib.inflate_end iz.in_stream;
      iz.in_stream <- Zlib.inflate_init false;
      let crc, size = loop 0 cdata_size 0 max_block_size Int32.zero 0 in
      if Int32.(crc <> ref_crc) then raise (Error "CRC mismatch, data corrupted");
      if size <> ref_size then raise (Error "size mismatch, data corrupted");
      iz.in_pos <- 0;
      iz.in_avail <- size
    with
    | End_of_file -> raise (Error "premature end of file, not a bgzf file")
  with
  | End_of_file -> iz.in_eof <- true
;;

let input iz buf pos len =
  let n = Bytes.length buf in
  if pos < 0 || len < 0 || pos + len > n then raise (Invalid_argument "Bgzf.input");
  if iz.in_eof
  then 0
  else (
    let rec loop pos len read =
      if len = 0
      then read
      else (
        if iz.in_pos = iz.in_avail then read_block iz;
        if iz.in_eof
        then read
        else (
          let n = min (iz.in_avail - iz.in_pos) len in
          Stdlib.Bytes.blit iz.in_buf iz.in_pos buf pos n;
          iz.in_pos <- iz.in_pos + n;
          loop (pos + n) (len - n) (read + n)))
    in
    loop pos len 0)
;;

let rec really_input iz buf pos len =
  if len <= 0
  then ()
  else (
    let n = input iz buf pos len in
    if n = 0 then raise End_of_file else really_input iz buf (pos + n) (len - n))
;;

let input_string iz n =
  if n < 0
  then raise (Invalid_argument "Bgzf.input_string iz n: n should be non negative");
  let r = Bytes.make n '@' in
  really_input iz r 0 n;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:r
;;

let input_char =
  let buf = Bytes.create 1 in
  fun iz -> if input iz buf 0 1 = 0 then raise End_of_file else Bytes.get buf 0
;;

let input_u8 iz = Char.to_int (input_char iz)

(* input_s* functions adapted from Batteries BatIO module *)
let input_s8 iz =
  let b = input_u8 iz in
  if b land 128 <> 0 then b - 256 else b
;;

let input_u16 iz =
  let b1 = input_u8 iz in
  let b2 = input_u8 iz in
  b1 lor (b2 lsl 8)
;;

let input_s16 iz =
  let i = input_u16 iz in
  if i land 32768 <> 0 then i - 65536 else i
;;

let input_s32 iz =
  let b1 = input_u8 iz in
  let b2 = input_u8 iz in
  let b3 = input_u8 iz in
  let b4 = input_u8 iz in
  Int32.bit_or
    (Int32.of_int_exn b1)
    (Int32.bit_or
       (Int32.shift_left (Int32.of_int_exn b2) 8)
       (Int32.bit_or
          (Int32.shift_left (Int32.of_int_exn b3) 16)
          (Int32.shift_left (Int32.of_int_exn b4) 24)))
;;

let seek_in iz i =
  let coffset = Int64.shift_right i 16 in
  let uoffset = Int64.(to_int_exn (bit_and 0xFFFFL i)) in
  In_channel.seek iz.ic coffset;
  iz.in_block_offset <- coffset;
  iz.in_eof <- false;
  if uoffset = 0
  then (
    iz.in_pos <- 0;
    iz.in_avail <- 0)
  else (
    read_block iz;
    iz.in_pos <- iz.in_pos + uoffset)
;;

let virtual_offset iz =
  if iz.in_pos = iz.in_avail
  then Int64.(shift_left (In_channel.pos iz.ic) 16)
  else Int64.(shift_left iz.in_block_offset 16 + of_int_exn iz.in_pos)
;;

let with_file_in fn ~f =
  let iz = open_in fn in
  let r =
    try `Ok (f iz) with
    | e -> `Error e
  in
  close_in iz;
  match r with
  | `Ok y -> y
  | `Error exn -> raise exn
;;

exception Unparser_error of string

type out_channel =
  { out_chan : Stdlib.out_channel
  ; out_ubuffer : bytes
  ; out_cbuffer : bytes
  ; mutable out_pos : int (* position in out_ubuffer *)
  ; out_level : int
  }

let output_int16 oc n =
  Out_channel.output_byte oc n;
  Out_channel.output_byte oc (n lsr 8)
;;

let output_int32 oc n =
  let r = ref n in
  for _ = 1 to 4 do
    Out_channel.output_byte oc (Int32.to_int_exn !r);
    r := Int32.shift_right_logical !r 8
  done
;;

let write_block oc buf len ~isize ~crc32 =
  let xlen = 6 in
  let bsize = 20 + xlen + len in
  assert (bsize < 0x10000);
  Out_channel.output_byte oc 0x1F;
  (* ID1 *)
  Out_channel.output_byte oc 0x8B;
  (* ID2 *)
  Out_channel.output_byte oc 8;
  (* compression method *)
  Out_channel.output_byte oc 4;
  (* flags *)
  for _ = 1 to 4 do
    Out_channel.output_byte oc 0 (* mtime *)
  done;
  Out_channel.output_byte oc 0;
  (* xflags *)
  Out_channel.output_byte oc 0xFF;
  (* OS (unknown) *)
  output_int16 oc xlen;
  (* XLEN *)
  Out_channel.output_byte oc 0x42;
  (* SI1 *)
  Out_channel.output_byte oc 0x43;
  (* SI2 *)
  output_int16 oc 2;
  (* SLEN *)
  output_int16 oc (bsize - 1);
  (* BSIZE - 1*)
  Caml.output oc buf 0 len;
  (* DATA *)
  output_int32 oc crc32;
  (* CRC32 *)
  output_int32 oc isize (* ISIZE *)
;;

let of_out_channel ?(level = 6) oc =
  if level < 1 || level > 9 then raise (invalid_arg "Bgzf: bad compression level");
  { out_chan = oc
  ; out_ubuffer = Bytes.create max_isize
  ; out_cbuffer = Bytes.create max_block_size
  ; out_pos = 0
  ; out_level = level
  }
;;

let open_out ?(level = 6) filename = of_out_channel ~level (Stdlib.open_out_bin filename)

let push_block oz =
  let stream = Zlib.deflate_init oz.out_level false in
  let _, used_in, used_out =
    try
      Zlib.deflate
        stream
        oz.out_ubuffer
        0
        oz.out_pos
        oz.out_cbuffer
        0
        (Bytes.length oz.out_cbuffer)
        Zlib.Z_FINISH
    with
    | Zlib.Error (_, _) -> raise (Unparser_error "error during compression")
  in
  assert (used_in = oz.out_pos);
  let crc32 = Zlib.update_crc Int32.zero oz.out_ubuffer 0 used_in in
  Zlib.deflate_end stream;
  write_block oz.out_chan oz.out_cbuffer used_out ~isize:(Int32.of_int_exn used_in) ~crc32;
  oz.out_pos <- 0
;;

let rec output ~length ~blit oz buf ~pos ~len =
  if pos < 0 || len < 0 || pos + len > length buf then invalid_arg "Bgzf.output";
  (* If output buffer is full, flush it *)
  if oz.out_pos = Bytes.length oz.out_ubuffer then push_block oz;
  let available = Bytes.length oz.out_ubuffer - oz.out_pos in
  let ncopy = min len available in
  blit buf pos oz.out_ubuffer oz.out_pos ncopy;
  oz.out_pos <- oz.out_pos + ncopy;
  let remaining = len - ncopy in
  if remaining > 0 then output ~length ~blit oz buf ~pos:(pos + ncopy) ~len:remaining
;;

let output_from_string = output ~length:String.length ~blit:Caml.Bytes.blit_string
let output = output ~length:Bytes.length ~blit:Caml.Bytes.blit

let output_char =
  let buf = Bytes.make 1 ' ' in
  fun oz c ->
    Bytes.set buf 0 c;
    output oz buf ~pos:0 ~len:1
;;

(* output_* functions adapted from Batteries BatIO module *)
let output_u8 oz n =
  (* if n < 0 || n > 0xFF then raise (Invalid_argument "Bgzf.output_u8") ; *)
  output_char oz (Char.unsafe_of_int (n land 0xFF))
;;

let output_s8 oz n =
  if n < -0x80 || n > 0x7F then raise (Invalid_argument "Bgzf.output_s8");
  if n < 0 then output_u8 oz (n + 256) else output_u8 oz n
;;

let output_u16 oz n =
  output_u8 oz n;
  output_u8 oz (n lsr 8)
;;

let output_s16 oz n =
  if n < -0x8000 || n > 0x7FFF then raise (Invalid_argument "Bgzf.output_s16");
  if n < 0 then output_u16 oz (65536 + n) else output_u16 oz n
;;

let output_s32 oz n =
  let base = Int32.to_int_exn n in
  let big = Int32.to_int_exn (Int32.shift_right_logical n 24) in
  output_u8 oz base;
  output_u8 oz (base lsr 8);
  output_u8 oz (base lsr 16);
  output_u8 oz big
;;

let output_string oz s = output_from_string oz s ~pos:0 ~len:(String.length s)

let bgzf_eof =
  "\x1f\x8b\x08\x04\x00\x00\x00\x00\x00\xff\x06\x00BC\x02\x00\x1b\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00"
;;

let dispose_out oz =
  if oz.out_pos > 0 then push_block oz;
  Stdlib.output_string oz.out_chan bgzf_eof
;;

let close_out oz =
  dispose_out oz;
  Stdlib.close_out oz.out_chan
;;

let with_file_out ?level fn ~f =
  let oz = open_out ?level fn in
  let r =
    try `Ok (f oz) with
    | e -> `Error e
  in
  close_out oz;
  match r with
  | `Ok y -> y
  | `Error exn -> raise exn
;;

module Test = struct
  let random_string n =
    String.init n ~f:(fun _ -> if Float.(Random.float 1. > 0.5) then '.' else 'o')
  ;;

  let%expect_test "test_parse_past_eof" =
    Utils.with_temp_file "test" ".bgzf" ~f:(fun fn ->
      with_file_out fn ~f:(fun oz -> output_string oz "BAM");
      printf
        "Reading past end of file should raise: %b\n"
        (try
           ignore (with_file_in fn ~f:(fun iz -> input_string iz 4));
           false
         with
         | _ -> true));
    [%expect {| Reading past end of file should raise: true |}]
  ;;

  let test_parse_of_unparse n =
    let s = random_string n in
    let n = String.length s in
    Utils.with_temp_file "test" ".bgzf" ~f:(fun fn ->
      with_file_out fn ~f:(fun oz -> output_string oz s);
      let s' = with_file_in fn ~f:(fun iz -> input_string iz n) in
      printf "%b\n" (String.equal s s'))
  ;;

  let%expect_test "test_parse_of_unparse" =
    test_parse_of_unparse 0x100;
    test_parse_of_unparse 0x10000;
    test_parse_of_unparse 0x1000000;
    [%expect {|
      true
      true
      true
    |}]
  ;;

  let test_parse_file_per_char fn =
    with_file_in fn ~f:(fun iz ->
      try
        while true do
          ignore (input_char iz : char)
        done
      with
      | End_of_file -> printf "%b\n" true)
  ;;

  let%expect_test "test_parse_file_per_char" =
    test_parse_file_per_char "../../etc/test_data/bgzf_01.bgzf";
    test_parse_file_per_char "../../etc/test_data/bgzf_02.bgzf";
    [%expect {|
      true
      true
    |}]
  ;;
end
