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

open Printf

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

exception Parse_error of string

type in_channel = {
  ic : Pervasives.in_channel ; (* Underlying channel *)
  in_block : string ; (* Block being read *)
  mutable in_pos : int ; (* Position in the current block *)
  mutable in_avail : int ; (* Number of available characters in the current block, can be less than [max_block_size] *)
  mutable in_crc32 : int32 ; (* Partial calculation of the block's CRC, up to [in_pos] *)
  mutable in_isize : int32 ; (* Partial calculation of the block's size, up to [in_pos] *)
  mutable in_block_crc32 : int32 ; (* CRC of the current block *)
  mutable in_block_isize : int32 ; (* Length of the data in the current block in uncompressed form *)
  mutable in_eof : bool ; (* Flag indicating we reached the end of the file *)
  mutable in_stream : Zlib.stream;
}

let of_in_channel ic = {
  ic ;
  in_block = String.create max_block_size ;
  in_pos = 0 ;
  in_avail = 0 ;
  in_block_crc32 = Int32.zero ;
  in_crc32 = Int32.zero ;
  in_block_isize = Int32.zero ;
  in_isize = Int32.zero ;
  in_stream = Zlib.inflate_init false ;
  in_eof = false
}

let open_in fn = of_in_channel (Pervasives.open_in_bin fn)

let dispose_in iz =
  iz.in_eof <- true ;
  Zlib.inflate_end iz.in_stream

let close_in iz =
  dispose_in iz ;
  close_in iz.ic



let may_eof f x =
  try Some (f x)
  with End_of_file -> None

let input_u16 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  b1 + b2 lsl 8

let input_int32 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  Int32.logor (Int32.of_int b1)
    (Int32.logor (Int32.shift_left (Int32.of_int b2) 8)
      (Int32.logor (Int32.shift_left (Int32.of_int b3) 16)
                   (Int32.shift_left (Int32.of_int b4) 24)))

(* Raises End_of_file iff there is no more block to read *)
let read_header iz =
  match may_eof input_byte iz.ic with
  | None -> iz.in_eof <- true ; raise End_of_file
  | Some id1 ->
    try
      let id2 = input_byte iz.ic in
      if id1 <> 0x1F || id2 <> 0x8B then raise (Parse_error "bad magic number, not a bgzf file") ;
      let cm = input_byte iz.ic in
      if cm <> 8 then raise(Parse_error "unknown compression method") ;
      let flags = input_byte iz.ic in
      if flags <> 0x04 then raise(Parse_error("bad flags, not a bgzf file"));
      for i = 1 to 6 do ignore (input_byte iz.ic) done;
      let xlen = input_u16 iz.ic in
      let si1 = input_byte iz.ic in
      let si2 = input_byte iz.ic in
      let slen = input_u16 iz.ic in
      if si1 <> 66 || si2 <> 67 || slen <> 2 then raise (Parse_error "bad extra subfield") ;
      let bsize = input_u16 iz.ic in
      for i = 1 to xlen - 6 do ignore (input_byte iz.ic) done ;
      bsize - xlen - 19
    with End_of_file -> raise (Parse_error "premature end of file, not a bgzf file")

let read_block iz =
  let cdata_size = read_header iz in (* read_header raises End_of_file iff there is no more block to read *)
  try
    Pervasives.really_input iz.ic iz.in_block 0 cdata_size ;
    iz.in_avail <- cdata_size ;
    let crc32 = input_int32 iz.ic in
    let isize = input_int32 iz.ic in
    iz.in_pos <- 0 ;
    iz.in_crc32 <- crc32 ;
    iz.in_isize <- isize ;
    iz.in_block_crc32 <- Int32.zero ;
    iz.in_block_isize <- Int32.zero ;
  with End_of_file -> raise(Parse_error("premature end of file, not a bgzf file"))

let input iz buf pos len =
  let n = String.length buf in
  if pos < 0 || len < 0 || pos + len > n then raise (Invalid_argument "Bgzf.input") ;
  if iz.in_eof then 0
  else (
    let rec loop pos len read =
      if len = 0 then read
      else (
        let reached_eof =
          if iz.in_avail = 0 then (
            try read_block iz ; false
            with End_of_file -> true
          )
          else false
        in
        if reached_eof then read
        else (
          let (finished, used_in, used_out) =
            try Zlib.inflate iz.in_stream iz.in_block iz.in_pos iz.in_avail buf pos len Zlib.Z_SYNC_FLUSH
            with Zlib.Error(_, _) -> raise(Parse_error("error during decompression"))
          in
          iz.in_pos <- iz.in_pos + used_in;
          iz.in_avail <- iz.in_avail - used_in;
          iz.in_block_crc32 <- Zlib.update_crc iz.in_block_crc32 buf pos used_out;
          iz.in_block_isize <- Int32.add iz.in_block_isize (Int32.of_int used_out);
          if finished then (
            Zlib.inflate_end iz.in_stream ;
            if iz.in_block_crc32 <> iz.in_crc32 then raise(Parse_error(sprintf "CRC mismatch, data corrupted: %ld %ld" iz.in_block_crc32 iz.in_crc32));
            if iz.in_block_isize <> iz.in_block_isize then raise(Parse_error("size mismatch, data corrupted"));
            iz.in_stream <- Zlib.inflate_init false
          ) ;
          loop (pos + used_out) (len - used_out) (read + used_out)
        )
      )
    in
    loop pos len 0
  )

let rec really_input iz buf pos len =
  if len <= 0 then ()
  else (
    let n = input iz buf pos len in
    if n = 0 then raise End_of_file
    else really_input iz buf (pos + n) (len - n)
  )

let input_string iz n =
  if n < 0 then raise (Invalid_argument "Bgzf.input_string iz n: n should be non negative") ;
  let r = String.make n '@' in
  really_input iz r 0 n ;
  r

let input_char =
  let buf = String.create 1 in
  fun iz ->
    if input iz buf 0 1 = 0 then raise End_of_file
    else buf.[0]

let input_u8 iz =
  Char.code (input_char iz)

(* input_s* functions adapted from Batteries BatIO module *)
let input_s8 iz =
  let b = input_u8 iz in
  if b land 128 <> 0 then b - 256
  else b

let input_u16 iz =
  let b1 = input_u8 iz in
  let b2 = input_u8 iz in
  b1 lor (b2 lsl 8)

let input_s16 iz =
  let i = input_u16 iz in
  if i land 32768 <> 0 then i - 65536
  else i

let input_s32 iz =
  let b1 = input_u8 iz in
  let b2 = input_u8 iz in
  let b3 = input_u8 iz in
  let b4 = input_u8 iz in
  Int32.logor (Int32.of_int b1)
    (Int32.logor (Int32.shift_left (Int32.of_int b2) 8)
      (Int32.logor (Int32.shift_left (Int32.of_int b3) 16)
                   (Int32.shift_left (Int32.of_int b4) 24)))

let with_file_in fn ~f =
  let iz = open_in fn in
  let r =
    try `Ok (f iz)
    with e -> `Error e
  in
  close_in iz ;
  match r with
  | `Ok y -> y
  | `Error exn -> raise exn

exception Unparser_error of string

type out_channel = {
  out_chan : Pervasives.out_channel ;
  out_ubuffer : string ;
  out_cbuffer : string ;
  mutable out_pos : int ; (* position in out_ubuffer *)
  out_level : int ;
}

let output_int16 oc n =
  Pervasives.output_byte oc n ;
  Pervasives.output_byte oc (n lsr 8)

let output_int32 oc n =
  let r = ref n in
  for i = 1 to 4 do
    Pervasives.output_byte oc (Int32.to_int !r);
    r := Int32.shift_right_logical !r 8
  done

let write_block oc buf len ~isize ~crc32 =
  let xlen = 6 in
  let bsize = 20 + xlen + len in
  assert (bsize < 0x10000) ;
  output_byte oc 0x1F;                  (* ID1 *)
  output_byte oc 0x8B;                  (* ID2 *)
  output_byte oc 8;                     (* compression method *)
  output_byte oc 4;                     (* flags *)
  for i = 1 to 4 do
    output_byte oc 0                    (* mtime *)
  done ;
  output_byte oc 0;                     (* xflags *)
  output_byte oc 0xFF;                  (* OS (unknown) *)
  output_int16 oc xlen ;                (* XLEN *)
  output_byte oc 0x42 ;                 (* SI1 *)
  output_byte oc 0x43 ;                 (* SI2 *)
  output_int16 oc 2 ;                   (* SLEN *)
  output_int16 oc (bsize - 1);          (* BSIZE - 1*)
  output oc buf 0 len ;                 (* DATA *)
  output_int32 oc crc32 ;               (* CRC32 *)
  output_int32 oc isize                 (* ISIZE *)

let of_out_channel ?(level = 6) oc =
  if level < 1 || level > 9 then raise (invalid_arg "Bgzf: bad compression level") ;
  {
    out_chan = oc;
    out_ubuffer = String.create max_isize ;
    out_cbuffer = String.create max_block_size ;
    out_pos = 0 ;
    out_level = level ;
  }

let open_out ?(level = 6) filename =
  of_out_channel ~level (Pervasives.open_out_bin filename)


let push_block oz =
  let stream = Zlib.deflate_init oz.out_level false in
  let (_, used_in, used_out) =
    try
      Zlib.deflate
        stream
        oz.out_ubuffer 0 oz.out_pos
        oz.out_cbuffer 0 (String.length oz.out_cbuffer)
        Zlib.Z_FINISH
    with Zlib.Error(_, _) -> raise (Unparser_error("error during compression"))
  in
  assert (used_in = oz.out_pos) ;
  let crc32 = Zlib.update_crc Int32.zero oz.out_ubuffer 0 used_in in
  Zlib.deflate_end stream ;
  write_block oz.out_chan oz.out_cbuffer used_out ~isize:(Int32.of_int used_in) ~crc32 ;
  oz.out_pos <- 0

let rec output oz buf pos len =
  if pos < 0 || len < 0 || pos + len > String.length buf then invalid_arg "Bgzf.output";
  (* If output buffer is full, flush it *)
  if oz.out_pos = String.length oz.out_ubuffer then push_block oz ;
  let available = String.length oz.out_ubuffer - oz.out_pos in
  let ncopy = min len available in
  String.blit buf pos oz.out_ubuffer oz.out_pos ncopy ;
  oz.out_pos <- oz.out_pos + ncopy ;
  let remaining = len - ncopy in
  if remaining > 0 then output oz buf (pos + ncopy) remaining

let output_char =
  let buf = String.make 1 ' ' in
  fun oz c ->
    buf.[0] <- c ;
    output oz buf 0 1

(* output_* functions adapted from Batteries BatIO module *)
let output_u8 oz n =
  (* if n < 0 || n > 0xFF then raise (Invalid_argument "Bgzf.output_u8") ; *)
  output_char oz (Char.unsafe_chr (n land 0xFF))

let output_s8 oz n =
  if n < -0x80 || n > 0x7F then raise (Invalid_argument "Bgzf.output_s8") ;
  if n < 0 then
    output_u8 oz (n + 256)
  else
    output_u8 oz n

let output_u16 oz n =
  output_u8 oz n ;
  output_u8 oz (n lsr 8)

let output_s16 oz n =
  if n < -0x8000 || n > 0x7FFF then raise (Invalid_argument "Bgzf.output_s16") ;
  if n < 0 then
    output_u16 oz (65536 + n)
  else
    output_u16 oz n

let output_s32 oz n =
  let base = Int32.to_int n in
  let big = Int32.to_int (Int32.shift_right_logical n 24) in
  output_u8 oz base ;
  output_u8 oz (base lsr 8) ;
  output_u8 oz (base lsr 16) ;
  output_u8 oz big

let output_string oz s =
  output oz s 0 (String.length s)

let bgzf_eof = "\x1f\x8b\x08\x04\x00\x00\x00\x00\x00\xff\x06\x00BC\x02\x00\x1b\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00"

let dispose_out oz =
  if oz.out_pos > 0 then push_block oz ;
  Pervasives.output_string oz.out_chan bgzf_eof

let close_out oz =
  dispose_out oz ;
  Pervasives.close_out oz.out_chan

let with_file_out ?level fn ~f =
  let oz = open_out ?level fn in
  let r =
    try `Ok (f oz)
    with e -> `Error e
  in
  close_out oz ;
  match r with
  | `Ok y -> y
  | `Error exn -> raise exn
